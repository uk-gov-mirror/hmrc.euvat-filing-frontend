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
import forms.PurchaseOrImportSubTypeFormProvider
import models.{Fuel, Other}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{times, verify, when}
import org.scalatestplus.mockito.MockitoSugar
import pages.*
import play.api.data.Form
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.govukfrontend.views.Aliases
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem
import utils.ConfigPurchaseOrImportMapping

class PurchaseSubTypeControllerSpec extends SpecBase with MockitoSugar {
  val onwardRoute: Call = Call("GET", "/foo")
  val formProvider = new PurchaseOrImportSubTypeFormProvider()
  val form: Form[String] = formProvider()

  "PurchaseSubType Controller" - {

    "must return OK and the correct view when options exist" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.test.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val userAnswers = emptyUserAnswers.set(RefundingCountryPage, "DE").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig))
        .build()

      running(application) {
        val request = FakeRequest(GET, "/file-eu-vat/purchase/fuel-use")
        val result = route(application, request).value

        status(result) mustEqual OK
      }
    }

    "must redirect to JourneyRecovery when country is missing" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.test.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val application = applicationBuilder(userAnswers = None)
        .overrides(bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig))
        .build()

      running(application) {
        val request = FakeRequest(GET, "/file-eu-vat/purchase/fuel-use")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to InvoiceType when no options exist" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq.empty
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val userAnswers = emptyUserAnswers.set(RefundingCountryPage, "DE").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig))
        .build()

      running(application) {
        val request = FakeRequest(GET, "/file-eu-vat/purchase/fuel-use")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.InvoiceTypeController.onPageLoad(models.NormalMode).url
      }
    }

    "must save selection and redirect to PurchaseSubCategory when children exist" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.fuel.1"), ("1.1", "sub.fuel.1.1"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq(("1.1", "sub.fuel.1.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers.set(RefundingCountryPage, "DE").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, "/file-eu-vat/purchase/fuel-use")
          .withFormUrlEncodedBody(("value", "1"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual "/file-eu-vat/purchase/fuel-type"

        val captor = ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        val saved = captor.getValue
        saved.get(PurchaseSubTypePage) mustBe Some("1")
      }
    }

    "must short-circuit to purchase CYA in CheckMode when value unchanged" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.fuel.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val userAnswers = emptyUserAnswers
        .set(RefundingCountryPage, "DE")
        .success
        .value
        .set(PurchaseSubTypePage, "1")
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig))
        .build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.PurchaseSubTypeController.onSubmit("fuel-use", models.CheckMode).url)
          .withFormUrlEncodedBody(("value", "1"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad().url
      }
    }

    "must persist and continue in CheckMode when value changed" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.fuel.1"), ("1.1", "sub.fuel.1.1"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq(("1.1", "sub.fuel.1.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers.set(RefundingCountryPage, "DE").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.PurchaseSubTypeController.onSubmit("fuel-use", models.CheckMode).url)
          .withFormUrlEncodedBody(("value", "1"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value must include("file-eu-vat/purchase/change-fuel-type")
        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        captor.getValue.get(PurchaseSubTypePage) mustBe Some("1")
      }
    }

    "must persist and redirect to CYA in CheckMode when no children exist" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.fuel.1"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq.empty
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers.set(RefundingCountryPage, "DE").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.PurchaseSubTypeController.onSubmit("fuel-use", models.CheckMode).url)
          .withFormUrlEncodedBody(("value", "1"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad().url
        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        captor.getValue.get(PurchaseSubTypePage) mustBe Some("1")
      }
    }

    "must persist and redirect to CYA in CheckMode when PurchaseType changed to Other and selected sub-type has no subcategories" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.other.1"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq.empty
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers
        .set(RefundingCountryPage, "DE")
        .success
        .value
        .set(PurchaseTypePage, Other)
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.PurchaseSubTypeController.onSubmit("purchase-type-other", models.CheckMode).url)
          .withFormUrlEncodedBody(("value", "1"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad().url
        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        captor.getValue.get(PurchaseSubTypePage) mustBe Some("1")
      }
    }

    "must handle RefundingCountryNamePage stored as 'Austria,AT' and show children" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1.1", "sub.fuel.1"), ("1.3", "sub.fuel.3"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) =
          if (subcode == "1.1") Seq(("1.1.1", "sub.fuel.1.1"), ("1.1.2", "sub.fuel.1.2")) else Seq.empty
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers.set(RefundingCountryNamePage, "Austria,AT").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, "/file-eu-vat/purchase/fuel-use")
          .withFormUrlEncodedBody(("value", "1.1"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        val loc = redirectLocation(result).value
        loc must include("/file-eu-vat/purchase/fuel-type")
        loc mustNot include("parentCode=")

        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        val saved = captor.getValue
        saved.get(PurchaseSubTypePage) mustBe Some("1.1")
      }
    }

    "must clear PurchaseSubCategory and its label when PurchaseSubType is changed on POST" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.fuel.1"), ("2", "sub.fuel.2"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq.empty
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers
        .set(RefundingCountryPage, "DE")
        .success
        .value
        .set(PurchaseSubTypePage, "1")
        .success
        .value
        .set(PurchaseSubCategoryPage, "1.1")
        .success
        .value
        .set(PurchaseSubCategoryLabelPage, "lbl")
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, "/file-eu-vat/purchase/fuel-use")
          .withFormUrlEncodedBody(("value", "2"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        val saved = captor.getValue

        saved.get(PurchaseSubCategoryPage) mustBe None
        saved.get(PurchaseSubCategoryLabelPage) mustBe None
        saved.get(PurchaseSubTypePage) mustBe Some("2")
      }
    }

    "must clear describe items data when changing Other subtype away from none option" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) =
          Seq(("10.99", "sub.other.99"), ("10.6", "sub.other.6"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq.empty
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers
        .set(pages.RefundingCountryPage, "DE")
        .success
        .value
        .set(pages.PurchaseTypePage, models.Other)
        .success
        .value
        .set(pages.PurchaseSubTypePage, "10.99")
        .success
        .value
        .set(pages.DescribeItemsOnInvoicePage, "previous purchase description")
        .success
        .value
        .set(pages.DescribeItemsArrivedFromCheckYourAnswersPage, true)
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, controllers.purchase.routes.PurchaseSubTypeController.onSubmit("purchase-type-other", models.CheckMode).url)
          .withFormUrlEncodedBody(("value", "10.6"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad().url

        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        val saved = captor.getValue

        saved.get(pages.PurchaseSubTypePage) mustBe Some("10.6")
        saved.get(pages.DescribeItemsOnInvoicePage) mustBe None
        saved.get(pages.DescribeItemsArrivedFromCheckYourAnswersPage) mustBe None
      }
    }

    "must save selection and redirect to InvoiceType when no children exist" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.fuel.1"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq.empty
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers.set(RefundingCountryPage, "DE").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, "/file-eu-vat/purchase/fuel-use")
          .withFormUrlEncodedBody(("value", "1"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.InvoiceTypeController.onPageLoad(models.NormalMode).url

        val captor = ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        val saved = captor.getValue
        saved.get(PurchaseSubTypePage) mustBe Some("1")
        saved.get(PurchaseSubTypeLabelPage).isDefined mustBe true
      }
    }

    "must return a Bad Request and errors when invalid data is submitted" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.test.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val userAnswers = emptyUserAnswers.set(RefundingCountryPage, "DE").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig))
        .build()

      running(application) {
        val request = FakeRequest(POST, "/file-eu-vat/purchase/fuel-use")
          .withFormUrlEncodedBody(("value", ""))

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        contentAsString(result) must include("There is a problem")
        contentAsString(result) must include(messages(application)("sub.fuel.error.required"))
      }
    }

    "must return OK when PurchaseTypePage present and slug unknown" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.test.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val userAnswers = emptyUserAnswers
        .set(RefundingCountryPage, "DE")
        .success
        .value
        .set(PurchaseTypePage, Fuel)
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig))
        .build()

      running(application) {
        val request = FakeRequest(GET, "/file-eu-vat/purchase/fuel-use")
        val result = route(application, request).value

        status(result) mustEqual OK
      }
    }

    "must not show None option when PurchaseType is Other" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.test.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq(
          RadioItem(
            content = Aliases.Text(msgs("sub.test.1")),
            value   = Some("1"),
            id      = Some("value_0")
          ),
          RadioItem(
            content = Aliases.Text(ConfigPurchaseOrImportMapping.NoneValue),
            value   = Some(ConfigPurchaseOrImportMapping.NoneValue),
            id      = Some("value_none")
          )
        )
      }

      val userAnswers = emptyUserAnswers
        .set(RefundingCountryPage, "AT")
        .success
        .value
        .set(PurchaseTypePage, Other)
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig))
        .build()

      running(application) {
        val request = FakeRequest(GET, "/file-eu-vat/purchase/purchase-type-other")
        val result = route(application, request).value

        status(result) mustEqual OK
        val body = contentAsString(result)
        body mustNot include(ConfigPurchaseOrImportMapping.NoneValue)
      }
    }

    "must clear stored subtype and label when CountryChangedPage is true" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.test.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers
        .set(RefundingCountryPage, "DE")
        .success
        .value
        .set(PurchaseSubTypePage, "1")
        .success
        .value
        .set(PurchaseSubTypeLabelPage, "label")
        .success
        .value
        .set(CountryChangedPage, true)
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(GET, "/file-eu-vat/purchase/fuel-use")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual "/file-eu-vat/purchase/fuel-use"

        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        val saved = captor.getValue
        saved.get(PurchaseSubTypePage) mustBe None
        saved.get(PurchaseSubTypeLabelPage) mustBe None
        saved.get(CountryChangedPage) mustBe None
      }
    }

    "must save and redirect when parent comes from user answers (children exist)" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.fuel.1"), ("1.1", "sub.fuel.1.1"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq(("1.1", "sub.fuel.1.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers
        .set(RefundingCountryPage, "DE")
        .success
        .value
        .set(PurchaseTypePage, Fuel)
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, "/file-eu-vat/purchase/fuel-use")
          .withFormUrlEncodedBody(("value", "1"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual "/file-eu-vat/purchase/fuel-type"
      }
    }

    "must skip sub-type and go to describe page when Other has single None option" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("10.99", "sub.other.99"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers.set(RefundingCountryPage, "DE").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(GET, "/file-eu-vat/purchase/purchase-type-other")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.DescribeItemsOnInvoiceController.onPageLoad(models.NormalMode).url

        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        val saved = captor.getValue
        saved.get(PurchaseSubTypePage) mustBe Some("10.99")
      }
    }

    "must remove subtype and redirect to InvoiceType when None selected" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.fuel.1"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq.empty
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers
        .set(RefundingCountryPage, "DE")
        .success
        .value
        .set(PurchaseSubTypePage, "1")
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, "/file-eu-vat/purchase/fuel-use")
          .withFormUrlEncodedBody(("value", ConfigPurchaseOrImportMapping.NoneValue))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.InvoiceTypeController.onPageLoad(models.NormalMode).url

        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        val saved = captor.getValue
        saved.get(PurchaseSubTypePage) mustBe Some(ConfigPurchaseOrImportMapping.NoneValue)
        saved.get(PurchaseSubTypeLabelPage) mustBe Some(ConfigPurchaseOrImportMapping.NoneValue)
        saved.get(PurchaseSubCategoryPage) mustBe None
      }
    }

    "must remove subtype and redirect to CYA in CheckMode when None selected" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.fuel.1"))
        override def subcategoriesFor(country: String, parentKey: String, subcode: String) = Seq.empty
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val mockSessionRepository = mock[repositories.SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn scala.concurrent.Future.successful(true)

      val userAnswers = emptyUserAnswers
        .set(RefundingCountryPage, "DE")
        .success
        .value
        .set(PurchaseSubTypePage, "1")
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(
          bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig),
          bind[repositories.SessionRepository].toInstance(mockSessionRepository)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.PurchaseSubTypeController.onSubmit("fuel-use", models.CheckMode).url)
          .withFormUrlEncodedBody(("value", ConfigPurchaseOrImportMapping.NoneValue))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.CheckYourPurchaseDetailsController.onPageLoad().url

        val captor = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        verify(mockSessionRepository, times(1)).set(captor.capture())
        val saved = captor.getValue
        saved.get(PurchaseSubTypePage) mustBe Some(ConfigPurchaseOrImportMapping.NoneValue)
        saved.get(PurchaseSubTypeLabelPage) mustBe Some(ConfigPurchaseOrImportMapping.NoneValue)
        saved.get(PurchaseSubCategoryPage) mustBe None
      }
    }

    "must display inline error message above radio buttons when no radio button is selected" in {
      val fakeConfig = new ConfigPurchaseOrImportMapping() {
        override def subcodesFor(country: String, parentKey: String) = Seq(("1", "sub.test.1"))
        override def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages) = Seq.empty
      }

      val userAnswers = emptyUserAnswers.set(RefundingCountryPage, "DE").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(bind[ConfigPurchaseOrImportMapping].toInstance(fakeConfig))
        .build()

      running(application) {
        val request = FakeRequest(POST, "/file-eu-vat/purchase/fuel-use")
          .withFormUrlEncodedBody(("value", ""))

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        contentAsString(result) must include("There is a problem")
        contentAsString(result) must include(messages(application)("sub.fuel.error.required"))
      }
    }

  }
}
