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

package controllers.imports

import base.SpecBase
import models.{Fuel, NormalMode, Other, PurchaseOrImportType, Transport, UserAnswers}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{times, verify}
import org.scalatestplus.mockito.MockitoSugar
import pages.{ImportSubCategoryPage, ImportSubCodePage, ImportTypePage, RefundingCountryPage}
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import queries.{ImportSubCategoryLabelQuery, ImportSubTypeLabelQuery}
import repositories.SessionRepository

class ImportSubCodeControllerSpec extends SpecBase with MockitoSugar {

  private def fuelRoute = controllers.imports.routes.ImportSubCodeController.onPageLoad("fuel").url
  private def journeyRecoveryUrl = controllers.routes.JourneyRecoveryController.onPageLoad().url
  private def sadReferenceUrl = controllers.imports.routes.SadReferenceController.onPageLoad.url
  private def taskListUrl = controllers.routes.TaskListDashboardController.onPageLoad().url
  private def importSubCategoryUrl = controllers.imports.routes.ImportSubCategoryController.onPageLoad(NormalMode).url

  private def answers(importType: PurchaseOrImportType = Fuel, country: String = "AT"): UserAnswers =
    emptyUserAnswers
      .set(RefundingCountryPage, country)
      .success
      .value
      .set(ImportTypePage, importType)
      .success
      .value

  private def savedAnswers: UserAnswers = {
    val captor = ArgumentCaptor.forClass(classOf[UserAnswers])
    verify(mockSessionRepository, times(1)).set(captor.capture())
    captor.getValue
  }

  "ImportSubCode Controller" - {

    "must return OK with the fuel question and the member state's sub-code options for a GET" in {
      val application = applicationBuilder(userAnswers = Some(answers())).build()

      running(application) {
        val result = route(application, FakeRequest(GET, fuelRoute)).value

        status(result) mustEqual OK
        val content = contentAsString(result)
        content must include("What is the fuel used for?")
        content must include("Import details")
        content must include("value=\"1.3\"")
        content must include("value=\"__none__\"")
        """>\s*None\s*<""".r.findFirstIn(content) mustBe defined
        content must not include "value=\"1.2.6\""
      }
    }

    "must render the back link to the Import type page" in {
      val application = applicationBuilder(userAnswers = Some(answers())).build()

      running(application) {
        val result = route(application, FakeRequest(GET, fuelRoute)).value
        val importTypeUrl = controllers.imports.routes.ImportTypeController.onPageLoad(NormalMode).url

        status(result) mustEqual OK
        contentAsString(result) must include(s"""href="$importTypeUrl"""")
        contentAsString(result) must not include s"""href="$taskListUrl""""
      }
    }

    "must render the transport question when the transport category was selected" in {
      val application = applicationBuilder(userAnswers = Some(answers(Transport))).build()

      running(application) {
        val transportRoute = controllers.imports.routes.ImportSubCodeController.onPageLoad("transport").url
        val result = route(application, FakeRequest(GET, transportRoute)).value

        status(result) mustEqual OK
        contentAsString(result) must include("What is the type of transport cost?")
      }
    }

    "must populate the view correctly on a GET when the question has previously been answered" in {
      val userAnswers = answers().set(ImportSubCodePage, "1.3").success.value
      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val result = route(application, FakeRequest(GET, fuelRoute)).value

        status(result) mustEqual OK
        """value="1\.3"[^>]*\bchecked""".r.findFirstIn(contentAsString(result)) mustBe defined
      }
    }

    "must return OK with SAD question when the member state only offers the 10.99 sub-code" in {
      val application = applicationBuilder(userAnswers = Some(answers(Other))).build()

      running(application) {
        val otherRoute = controllers.imports.routes.ImportSubCodeController.onPageLoad("other").url
        val result = route(application, FakeRequest(GET, otherRoute)).value

        status(result) mustEqual OK
        contentAsString(result) must include(messages(application)("singleAdministrativeDocumentReferenceNumberAvailable.heading"))
      }
    }

    "must return OK with SAD question when the URL category does not match the import type answer" in {
      val application = applicationBuilder(userAnswers = Some(answers(Transport))).build()

      running(application) {
        val result = route(application, FakeRequest(GET, fuelRoute)).value

        status(result) mustEqual OK
        contentAsString(result) must include(messages(application)("singleAdministrativeDocumentReferenceNumberAvailable.heading"))
      }
    }

    "must redirect to Journey Recovery for a GET if no existing data is found" in {
      val application = applicationBuilder(userAnswers = None).build()

      running(application) {
        val result = route(application, FakeRequest(GET, fuelRoute)).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
      }
    }

    "must save the sub-code and its label and redirect to Import sub category when the sub-code has sub categories" in {
      val application = applicationBuilder(userAnswers = Some(answers()))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, fuelRoute).withFormUrlEncodedBody("value" -> "1.2")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual importSubCategoryUrl

        val saved = savedAnswers
        saved.get(ImportSubCodePage) mustBe Some("1.2")
        saved.get(ImportSubTypeLabelQuery) mustBe defined
      }
    }

    "must save the sub-code and redirect to Journey Recovery when the sub-code has no sub categories" in {
      val application = applicationBuilder(userAnswers = Some(answers()))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, fuelRoute).withFormUrlEncodedBody("value" -> "1.3")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual sadReferenceUrl
        savedAnswers.get(ImportSubCodePage) mustBe Some("1.3")
      }
    }

    "must save the none marker and redirect to Journey Recovery when None is submitted" in {
      val application = applicationBuilder(userAnswers = Some(answers()))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, fuelRoute).withFormUrlEncodedBody("value" -> "__none__")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
        savedAnswers.get(ImportSubCodePage) mustBe Some("__none__")
      }
    }

    "must clear the sub category and its label when a different sub-code is submitted" in {
      val userAnswers = answers()
        .set(ImportSubCodePage, "1.1")
        .success
        .value
        .set(ImportSubCategoryPage, "1.1.1")
        .success
        .value
        .set(ImportSubCategoryLabelQuery, "label")
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, fuelRoute).withFormUrlEncodedBody("value" -> "1.2")
        status(route(application, request).value) mustEqual SEE_OTHER

        val saved = savedAnswers
        saved.get(ImportSubCodePage) mustBe Some("1.2")
        saved.get(ImportSubCategoryPage) mustBe None
        saved.get(ImportSubCategoryLabelQuery) mustBe None
      }
    }

    "must keep the sub category when the same sub-code is submitted again" in {
      val userAnswers = answers()
        .set(ImportSubCodePage, "1.2")
        .success
        .value
        .set(ImportSubCategoryPage, "1.2.6")
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, fuelRoute).withFormUrlEncodedBody("value" -> "1.2")
        status(route(application, request).value) mustEqual SEE_OTHER

        savedAnswers.get(ImportSubCategoryPage) mustBe Some("1.2.6")
      }
    }

    "must return a Bad Request with the category's error message when no option is submitted" in {
      val application = applicationBuilder(userAnswers = Some(answers())).build()

      running(application) {
        val request = FakeRequest(POST, fuelRoute).withFormUrlEncodedBody()
        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        contentAsString(result) must include("Select what the fuel is used for")
      }
    }

    "must redirect to Journey Recovery when a sub-code outside the member state's options is submitted" in {
      val application = applicationBuilder(userAnswers = Some(answers())).build()

      running(application) {
        val request = FakeRequest(POST, fuelRoute).withFormUrlEncodedBody("value" -> "9.9")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
      }
    }
  }
}