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
import org.scalatestplus.mockito.MockitoSugar
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import models.NormalMode

class SadReferenceControllerSpec extends SpecBase with MockitoSugar {

  val formProvider = new forms.imports.SadReferenceFormProvider()

  "SadReference Controller" - {

    "must return OK and the correct view for a GET" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, controllers.imports.routes.SadReferenceController.onPageLoad.url)
        val result = route(application, request).value

        status(result) mustBe OK
        val view = application.injector.instanceOf[views.html.imports.SadReferenceView]

        normalizeHtml(contentAsString(result)) mustEqual normalizeHtml(
          view(formProvider(), controllers.imports.routes.ImportTypeController.onPageLoad(NormalMode))(request, messages(application)).toString
        )
      }
    }

    "must show back link to ImportSubCode when ImportSubCodePage present" in {
      val userAnswers = emptyUserAnswers
        .set(pages.ImportTypePage, models.Fuel)
        .success
        .value
        .set(pages.ImportSubCodePage, "1.3")
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, controllers.imports.routes.SadReferenceController.onPageLoad.url)
        val result  = route(application, request).value

        status(result) mustBe OK
        contentAsString(result) must include(controllers.imports.routes.ImportSubCodeController.onPageLoad(models.Fuel.toString).url)
      }
    }

    "must redirect to Journey Recovery when 'yes' is submitted" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, controllers.imports.routes.SadReferenceController.onSubmit.url)
          .withFormUrlEncodedBody("value" -> "true")

        val result = route(application, request).value

        status(result) mustBe SEE_OTHER
        //TODO: update to SadReferenceNumberController once built
        redirectLocation(result).value mustBe controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to ImportDetailsInfoController when 'no' is submitted" in {
          val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

          running(application) {
            val request = FakeRequest(POST, controllers.imports.routes.SadReferenceController.onSubmit.url)
              .withFormUrlEncodedBody("value" -> "false")

            val result = route(application, request).value

            status(result) mustBe SEE_OTHER
            redirectLocation(result).value mustBe controllers.imports.routes.ImportDetailsInfoController.onPageLoad(NormalMode).url
          }
        }

    "must return a Bad Request and errors when invalid data is submitted" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, controllers.imports.routes.SadReferenceController.onSubmit.url)
          .withFormUrlEncodedBody("value" -> "")

        val result = route(application, request).value

        status(result) mustBe BAD_REQUEST
      }
    }
  }

}
