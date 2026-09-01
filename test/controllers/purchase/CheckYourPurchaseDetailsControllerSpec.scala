/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers.purchase

import base.SpecBase
import controllers.routes
import models.{InvoiceType, PurchaseType}
import org.mockito.ArgumentMatchers.any
import org.mockito.{ArgumentCaptor, Mockito}
import org.mockito.Mockito._
import org.scalatestplus.mockito.MockitoSugar
import models.requests.UpdatePurchaseRequest
import play.api.inject.bind
import java.time.LocalDateTime
import java.time.LocalDate
import play.api.Configuration
import utils.ConfigCurrencyMapping
import models.SupplierAddress
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.SessionRepository
import services.EuVatRefundsService
import scala.concurrent.Future
import pages._
import models.responses.AddPurchaseResponse
import play.api.libs.json.Json
import models.responses.UpdatePurchaseResponse

class CheckYourPurchaseDetailsControllerSpec extends SpecBase with MockitoSugar {

  "CheckYourPurchaseDetailsController" - {

    "should use PurchaseSubTypePage value for goodsCategory and fall back to InvoiceType when simplified flag missing" in {
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      when(mockEuVatRefundsService.updatePurchase(any())(any())) thenReturn Future.successful(
        models.responses.UpdatePurchaseResponse(updateSequenceNumber = 5)
      )

      val userAnswers = emptyUserAnswers
        .set(PurchaseSubTypePage, "1.2").success.value
        .set(AddPurchaseResponsePage, AddPurchaseResponse(itemNumber = 1, updateSequenceNumber = 1)).success.value
        .set(queries.ClaimApplicationResponseQuery, models.responses.ApplicationResponse(1, "GB001", 1)).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.CheckYourPurchaseDetailsController.onSubmit().url)
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        verify(mockSessionRepository).set(any())
      }
    }

    "should derive simplified indicator from InvoiceType when explicit flag missing" in {
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      when(mockEuVatRefundsService.updatePurchase(any())(any())) thenReturn Future.successful(
        models.responses.UpdatePurchaseResponse(updateSequenceNumber = 7)
      )

      val userAnswers = emptyUserAnswers
        .set(InvoiceTypePage, InvoiceType.SimplifiedInvoice).success.value
        .set(AddPurchaseResponsePage, AddPurchaseResponse(itemNumber = 2, updateSequenceNumber = 2)).success.value
        .set(queries.ClaimApplicationResponseQuery, models.responses.ApplicationResponse(2, "GB002", 2)).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.CheckYourPurchaseDetailsController.onSubmit().url)
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        verify(mockSessionRepository).set(any())
      }
    }

    "should use explicit SimplifiedInvoiceVatRegCheckPage when present (true)" in {
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val captor: ArgumentCaptor[UpdatePurchaseRequest] = ArgumentCaptor.forClass(classOf[UpdatePurchaseRequest])
      when(mockEuVatRefundsService.updatePurchase(captor.capture())(any())) thenReturn Future.successful(
        models.responses.UpdatePurchaseResponse(updateSequenceNumber = 9)
      )

      val userAnswers = emptyUserAnswers
        .set(PurchaseSubTypePage, "1.2").success.value
        .set(SimplifiedInvoiceVatRegCheckPage, true).success.value
        .set(AddPurchaseResponsePage, AddPurchaseResponse(itemNumber = 3, updateSequenceNumber = 3)).success.value
        .set(queries.ClaimApplicationResponseQuery, models.responses.ApplicationResponse(3, "GB003", 3)).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.CheckYourPurchaseDetailsController.onSubmit().url)
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        val sent = captor.getValue
        sent.goodsDescriptionCategory mustEqual "1.2"
        sent.simplifiedInvoiceIndicator must contain("true")
      }
    }

    "should use explicit SimplifiedInvoiceVatRegCheckPage when present (false)" in {
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val captor: ArgumentCaptor[UpdatePurchaseRequest] = ArgumentCaptor.forClass(classOf[UpdatePurchaseRequest])
      when(mockEuVatRefundsService.updatePurchase(captor.capture())(any())) thenReturn Future.successful(
        models.responses.UpdatePurchaseResponse(updateSequenceNumber = 10)
      )

      val userAnswers = emptyUserAnswers
        .set(PurchaseSubTypePage, "1.2").success.value
        .set(SimplifiedInvoiceVatRegCheckPage, false).success.value
        .set(AddPurchaseResponsePage, AddPurchaseResponse(itemNumber = 4, updateSequenceNumber = 4)).success.value
        .set(queries.ClaimApplicationResponseQuery, models.responses.ApplicationResponse(4, "GB004", 4)).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.CheckYourPurchaseDetailsController.onSubmit().url)
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        val sent = captor.getValue
        sent.simplifiedInvoiceIndicator must contain("false")
      }
    }

    "should redirect to JourneyRecovery when service call fails" in {
      when(mockEuVatRefundsService.updatePurchase(any())(any())).thenReturn(Future.failed(new RuntimeException("boom")))

      val userAnswers = emptyUserAnswers
        .set(PurchaseSubTypePage, "1.2").success.value
        .set(AddPurchaseResponsePage, AddPurchaseResponse(itemNumber = 5, updateSequenceNumber = 5)).success.value
        .set(queries.ClaimApplicationResponseQuery, models.responses.ApplicationResponse(5, "GB005", 5)).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.CheckYourPurchaseDetailsController.onSubmit().url)
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value must include(controllers.routes.JourneyRecoveryController.onPageLoad().url)
      }
    }

    "should redirect to JourneyRecovery when ClaimApplicationResponseQuery missing" in {
      when(mockEuVatRefundsService.updatePurchase(any())(any())) thenReturn Future.successful(models.responses.UpdatePurchaseResponse(updateSequenceNumber = 11))

      val userAnswers = emptyUserAnswers
        .set(PurchaseSubTypePage, "1.2").success.value
        .set(AddPurchaseResponsePage, AddPurchaseResponse(itemNumber = 6, updateSequenceNumber = 6)).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.CheckYourPurchaseDetailsController.onSubmit().url)
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value must include(controllers.routes.JourneyRecoveryController.onPageLoad().url)
      }
    }

    "onPageLoad should include currency row when currency selection required" in {
      val fakeConfig = mock[ConfigCurrencyMapping]
      when(fakeConfig.currenciesFor("BG")).thenReturn(Seq(("euro", "EUR", "€"), ("pound", "GBP", "£")))
      when(fakeConfig.requiresCurrencySelection("BG")).thenReturn(true)

      val ua = emptyUserAnswers.set(RefundingCountryPage, "BG").success.value

      val application = applicationBuilder(userAnswers = Some(ua)).overrides(bind[ConfigCurrencyMapping].toInstance(fakeConfig)).build()

      running(application) {
        implicit val app = application
        val request = FakeRequest(GET, controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad().url)
        val result = route(application, request).value
        val body = contentAsString(result)
        val msgs = messages(application)

        status(result) mustEqual OK
        body must include(msgs("refundingCurrency.checkYourAnswersLabel"))
      }
    }

    "onPageLoad should NOT include currency row when currency selection not required" in {
      val fakeConfig = mock[ConfigCurrencyMapping]
      when(fakeConfig.currenciesFor("BG")).thenReturn(Seq(("euro", "EUR", "€")))
      when(fakeConfig.requiresCurrencySelection("BG")).thenReturn(false)

      val ua = emptyUserAnswers.set(RefundingCountryPage, "BG").success.value

      val application = applicationBuilder(userAnswers = Some(ua)).overrides(bind[ConfigCurrencyMapping].toInstance(fakeConfig)).build()

      running(application) {
        val request = FakeRequest(GET, controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad().url)
        val result = route(application, request).value
        val body = contentAsString(result)
        val msgs = messages(application)

        status(result) mustEqual OK
        body must not include msgs("refundingCurrency.checkYourAnswersLabel")
      }
    }

    "onSubmit should map subcategory, invoiceDate.atStartOfDay, supplier address and vat/tax ids" in {
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val captor: ArgumentCaptor[UpdatePurchaseRequest] = ArgumentCaptor.forClass(classOf[UpdatePurchaseRequest])
      when(mockEuVatRefundsService.updatePurchase(captor.capture())(any())) thenReturn Future.successful(
        models.responses.UpdatePurchaseResponse(updateSequenceNumber = 21)
      )

      val invoiceDate = LocalDate.of(2026, 1, 2)
      val supplierAddr = SupplierAddress(line1 = "L1", line2 = Some("L2"), line3 = Some("L3"))

      val userAnswers = emptyUserAnswers
        .set(PurchaseSubTypePage, "1.2").success.value
        .set(PurchaseSubCategoryPage, "7.1").success.value
        .set(InvoiceDatePage, invoiceDate).success.value
        .set(SupplierAddressPage, supplierAddr).success.value
        .set(SupplierVatRegistrationNumberPage, "VAT123").success.value
        .set(SupplierTaxIdentifierNumberPage, "TAX456").success.value
        .set(AddPurchaseResponsePage, AddPurchaseResponse(itemNumber = 7, updateSequenceNumber = 7)).success.value
        .set(queries.ClaimApplicationResponseQuery, models.responses.ApplicationResponse(7, "GB007", 7)).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.CheckYourPurchaseDetailsController.onSubmit().url)
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        val sent = captor.getValue
        sent.goodsDescriptionSubCategory must contain("7.1")
        sent.invoiceDate must contain(invoiceDate.atStartOfDay())
        sent.supplierAddress1 must contain("L1")
        sent.supplierAddress2 must contain("L2")
        sent.supplierAddress3 must contain("L3")
        sent.supplierVatRegNumber must contain("VAT123")
        sent.supplierTaxIdentifier must contain("TAX456")
      }
    }

    "should persist returned updateSequenceNumber replacing previous value" in {
      val savedCaptor: ArgumentCaptor[models.UserAnswers] = ArgumentCaptor.forClass(classOf[models.UserAnswers])

      when(mockSessionRepository.set(savedCaptor.capture())) thenReturn Future.successful(true)

      when(mockEuVatRefundsService.updatePurchase(any())(any())) thenReturn Future.successful(
        models.responses.UpdatePurchaseResponse(updateSequenceNumber = 99)
      )

      val userAnswers = emptyUserAnswers
        .set(PurchaseSubTypePage, "1.2").success.value
        .set(AddPurchaseResponsePage, AddPurchaseResponse(itemNumber = 8, updateSequenceNumber = 8)).success.value
        .set(queries.ClaimApplicationResponseQuery, models.responses.ApplicationResponse(8, "GB008", 8)).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.CheckYourPurchaseDetailsController.onSubmit().url)
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        val saved = savedCaptor.getValue
        val stored = saved.get(AddPurchaseResponsePage).value
        stored.updateSequenceNumber mustEqual 99
        stored.itemNumber mustEqual 8
      }
    }
  }
}
