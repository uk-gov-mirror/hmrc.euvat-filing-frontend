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
import models.{Fuel, NormalMode, UserAnswers}
import org.mockito.ArgumentCaptor
import org.mockito.Mockito.{times, verify}
import pages.{ImportSubCategoryPage, ImportSubCodePage, ImportTypePage, RefundingCountryPage}
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import queries.ImportSubCategoryLabelQuery
import repositories.SessionRepository

class ImportSubCategoryControllerSpec extends SpecBase {

  private def subCategoryRoute = controllers.imports.routes.ImportSubCategoryController.onPageLoad(NormalMode).url
  private def submitRoute = controllers.imports.routes.ImportSubCategoryController.onSubmit(NormalMode).url
  private def journeyRecoveryUrl = controllers.routes.JourneyRecoveryController.onPageLoad().url
  private def sadReferenceUrl = controllers.imports.routes.SadReferenceController.onPageLoad.url

  private def answers(subCode: String = "1.2"): UserAnswers =
    emptyUserAnswers
      .set(RefundingCountryPage, "AT")
      .success
      .value
      .set(ImportTypePage, Fuel)
      .success
      .value
      .set(ImportSubCodePage, subCode)
      .success
      .value

  private def savedAnswers: UserAnswers = {
    val captor = ArgumentCaptor.forClass(classOf[UserAnswers])
    verify(mockSessionRepository, times(1)).set(captor.capture())
    captor.getValue
  }

  "ImportSubCategory Controller" - {

    "must return OK with the sub category options for the chosen sub-code for a GET" in {
      val application = applicationBuilder(userAnswers = Some(answers())).build()

      running(application) {
        val result = route(application, FakeRequest(GET, subCategoryRoute)).value

        status(result) mustEqual OK
        val content = contentAsString(result)
        content must include("Import details")
        content must include("value=\"1.2.6\"")
        content must include("value=\"1.2.7\"")
        content must include("value=\"__none__\"")
        content must include("What is the type of fuel or vehicle?")
        content must include("Passenger car (PKW)")
        content must not include "value=\"1.3\""
      }
    }

    "must render the back link to the Import sub-code page" in {
      val application = applicationBuilder(userAnswers = Some(answers())).build()

      running(application) {
        val result = route(application, FakeRequest(GET, subCategoryRoute)).value
        val importSubCodeUrl = controllers.imports.routes.ImportSubCodeController.onPageLoad("fuel").url

        status(result) mustEqual OK
        contentAsString(result) must include(s"""href="$importSubCodeUrl"""")
      }
    }

    "must populate the view correctly on a GET when the question has previously been answered" in {
      val userAnswers = answers().set(ImportSubCategoryPage, "1.2.6").success.value
      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val result = route(application, FakeRequest(GET, subCategoryRoute)).value

        status(result) mustEqual OK
        """value="1\.2\.6"[^>]*\bchecked""".r.findFirstIn(contentAsString(result)) mustBe defined
      }
    }

    "must redirect to Journey Recovery for a GET when the sub-code has no sub categories" in {
      val application = applicationBuilder(userAnswers = Some(answers("1.3"))).build()

      running(application) {
        val result = route(application, FakeRequest(GET, subCategoryRoute)).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
      }
    }

    "must redirect to Journey Recovery for a GET when no sub-code has been answered" in {
      val userAnswers = answers().remove(ImportSubCodePage).success.value
      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val result = route(application, FakeRequest(GET, subCategoryRoute)).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
      }
    }

    "must redirect to Journey Recovery for a GET when no import type has been answered" in {
      val userAnswers = answers().remove(ImportTypePage).success.value
      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val result = route(application, FakeRequest(GET, subCategoryRoute)).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
      }
    }

    "must redirect to Journey Recovery for a GET when no member state has been answered" in {
      val userAnswers = answers().remove(RefundingCountryPage).success.value
      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val result = route(application, FakeRequest(GET, subCategoryRoute)).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
      }
    }

    "must redirect to Journey Recovery for a GET if no existing data is found" in {
      val application = applicationBuilder(userAnswers = None).build()

      running(application) {
        val result = route(application, FakeRequest(GET, subCategoryRoute)).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
      }
    }

    "must save the sub category and its label and redirect to the SAD reference page when valid data is submitted" in {
      val application = applicationBuilder(userAnswers = Some(answers()))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, submitRoute).withFormUrlEncodedBody("value" -> "1.2.6")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual sadReferenceUrl

        val saved = savedAnswers
        saved.get(ImportSubCategoryPage) mustBe Some("1.2.6")
        saved.get(ImportSubCategoryLabelQuery) mustBe defined
      }
    }

    "must save the none marker and redirect to Journey Recovery when None is submitted" in {
      val application = applicationBuilder(userAnswers = Some(answers()))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, submitRoute).withFormUrlEncodedBody("value" -> "__none__")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual sadReferenceUrl
        savedAnswers.get(ImportSubCategoryPage) mustBe Some("__none__")
      }
    }

    "must return a Bad Request with the sub-code's error message when no option is submitted" in {
      val application = applicationBuilder(userAnswers = Some(answers())).build()

      running(application) {
        val request = FakeRequest(POST, submitRoute).withFormUrlEncodedBody()
        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        val content = contentAsString(result)
        content must include("There is a problem")
        content must include("Select the type of fuel or vehicle")
      }
    }

    "must redirect to Journey Recovery when a sub category outside the sub-code's options is submitted" in {
      val application = applicationBuilder(userAnswers = Some(answers())).build()

      running(application) {
        val request = FakeRequest(POST, submitRoute).withFormUrlEncodedBody("value" -> "1.1.1")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
      }
    }

    "must redirect to Journey Recovery for a POST when the sub-code has no sub categories" in {
      val application = applicationBuilder(userAnswers = Some(answers("1.3"))).build()

      running(application) {
        val request = FakeRequest(POST, submitRoute).withFormUrlEncodedBody("value" -> "1.2.6")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual journeyRecoveryUrl
      }
    }
  }
}
