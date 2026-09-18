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
import forms.ImportDetailsInfoFormProvider
import models.{NormalMode, CheckMode, UserAnswers}
import navigation.{FakeNavigator, Navigator}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import pages.ImportDetailsInfoPage
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.SessionRepository
import views.html.imports.ImportDetailsInfoView

import scala.concurrent.Future

class ImportDetailsInfoControllerSpec extends SpecBase with MockitoSugar {

  def onwardRoute = Call("GET", "/foo")

  lazy val importDetailsInfoRoute: String = controllers.imports.routes.ImportDetailsInfoController.onPageLoad(NormalMode).url
  lazy val backLinkCall: Call             = controllers.imports.routes.SadReferenceController.onPageLoad

  val formProvider = new ImportDetailsInfoFormProvider()
  val form = formProvider()

  "ImportDetailsInfo Controller" - {

    "must return OK and the correct view for a GET" in {

      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, importDetailsInfoRoute)

        val result = route(application, request).value

        val view = application.injector.instanceOf[ImportDetailsInfoView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(form, NormalMode, backLinkCall)(request, messages(application)).toString
      }
    }

    "must populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers = UserAnswers(userAnswersId).set(ImportDetailsInfoPage, "answer").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, importDetailsInfoRoute)

        val view = application.injector.instanceOf[ImportDetailsInfoView]

        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(form.fill("answer"), NormalMode, backLinkCall)(request, messages(application)).toString
      }
    }

    "must redirect to the next page when valid data is submitted for the first time" in {

      val mockSessionRepository = mock[SessionRepository]

      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, importDetailsInfoRoute)
            .withFormUrlEncodedBody(("value", "answer"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "must redirect to Journey Recovery (as a placeholder for the warning page ) when the user visits the Import Details page more than once" in {

          val existingAnswers = UserAnswers(userAnswersId).set(ImportDetailsInfoPage, "existing answer").success.value

          val mockSessionRepository = mock[SessionRepository]

          when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

          val application =
            applicationBuilder(userAnswers = Some(existingAnswers))
              .overrides(
                bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
                bind[SessionRepository].toInstance(mockSessionRepository)
              )
              .build()

          running(application) {
            val request =
              FakeRequest(POST, importDetailsInfoRoute)
                .withFormUrlEncodedBody(("value", "a different answer"))

            val result = route(application, request).value

            status(result) mustEqual SEE_OTHER
            //TODO: update once the wrn11 warning controller is built
            redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
          }
        }

     "must not show the warning when the user is only changing their answer via the CYA page" in {

              val existingAnswers = UserAnswers(userAnswersId).set(ImportDetailsInfoPage, "existing answer").success.value

              val mockSessionRepository = mock[SessionRepository]

              when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

              val application =
                applicationBuilder(userAnswers = Some(existingAnswers))
                  .overrides(
                    bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
                    bind[SessionRepository].toInstance(mockSessionRepository)
                  )
                  .build()

              running(application) {
                val request =
                  FakeRequest(POST, controllers.imports.routes.ImportDetailsInfoController.onSubmit(CheckMode).url)
                    .withFormUrlEncodedBody(("value", "a different answer"))

                val result = route(application, request).value

                status(result) mustEqual SEE_OTHER
                redirectLocation(result).value mustEqual onwardRoute.url
              }
            }


    "must return a Bad Request and errors when empty data is submitted" in {

      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request =
          FakeRequest(POST, importDetailsInfoRoute)
            .withFormUrlEncodedBody(("value", ""))

        val boundForm = form.bind(Map("value" -> ""))

        val view = application.injector.instanceOf[ImportDetailsInfoView]

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        contentAsString(result) mustEqual view(boundForm, NormalMode, backLinkCall)(request, messages(application)).toString
      }
    }

     "must return a Bad Request and errors when data over 255 characters is submitted" in {

          val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

          running(application) {
            val tooLong = "a" * 256
            val request =
              FakeRequest(POST, importDetailsInfoRoute)
                .withFormUrlEncodedBody(("value",tooLong))

            val boundForm = form.bind(Map("value" -> tooLong))

            val view = application.injector.instanceOf[ImportDetailsInfoView]

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
            contentAsString(result) mustEqual view(boundForm, NormalMode, backLinkCall)(request, messages(application)).toString
          }
        }

    "must redirect to Journey Recovery for a GET if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {
        val request = FakeRequest(GET, importDetailsInfoRoute)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for a POST if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {
        val request =
          FakeRequest(POST, importDetailsInfoRoute)
            .withFormUrlEncodedBody(("value", "answer"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }
  }
}
