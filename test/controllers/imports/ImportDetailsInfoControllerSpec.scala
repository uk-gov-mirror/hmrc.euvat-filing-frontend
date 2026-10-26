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
import forms.imports.ImportDetailsInfoFormProvider
import models.requests.ReferenceInfoCountRequest
import models.responses.{AddImportResponse, ApplicationResponse, ReferenceInfoCountResponse}
import models.{CheckMode, Mode, NormalMode, UserAnswers}
import navigation.{FakeNavigator, Navigator}
import org.mockito.ArgumentMatchers.{any, eq as mockitoEq}
import org.mockito.Mockito.{reset, when}
import org.scalatestplus.mockito.MockitoSugar
import pages.AddImportResponsePage
import pages.imports.{ImportDetailsInfoPage, SadReferenceCheckPage, SadReferenceNumberPage}
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import queries.ClaimApplicationResponseQuery
import repositories.SessionRepository
import uk.gov.hmrc.http.HeaderCarrier
import views.html.imports.ImportDetailsInfoView

import scala.concurrent.Future

class ImportDetailsInfoControllerSpec extends SpecBase with MockitoSugar {

  def onwardRoute = Call("GET", "/foo")

  private val mockSessionRepository = mock[SessionRepository]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockEuVatRefundsService, mockSessionRepository)
  }

  private def importDetailsInfoRoute(mode: Mode = NormalMode): String = controllers.imports.routes.ImportDetailsInfoController.onPageLoad(mode).url

  lazy val backLinkCall: Call = controllers.imports.routes.SadReferenceCheckController.onPageLoad(NormalMode)
  lazy val checkModeBackLinkCall: Call = controllers.imports.routes.SadReferenceCheckController.onPageLoad(CheckMode)

  val formProvider = new ImportDetailsInfoFormProvider()
  val form = formProvider()

  "ImportDetailsInfo Controller" - {

    "must return OK and the correct view for a GET" in {

      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, importDetailsInfoRoute())

        val result = route(application, request).value

        val view = application.injector.instanceOf[ImportDetailsInfoView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(form, NormalMode, backLinkCall)(request, messages(application)).toString
      }
    }

    "must show back link to SadReferenceNumber when SadReference was yes and number present" in {

      val userAnswers = UserAnswers(userAnswersId)
        .set(SadReferenceCheckPage, true)
        .success
        .value
        .set(SadReferenceNumberPage, "ABC123")
        .success
        .value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, importDetailsInfoRoute())

        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include(controllers.imports.routes.SadReferenceNumberController.onPageLoad(NormalMode).url)
      }
    }

    "must show back link to SadReference when SadReference was yes but number missing" in {

      val userAnswers = UserAnswers(userAnswersId).set(SadReferenceCheckPage, true).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, importDetailsInfoRoute())

        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include(controllers.imports.routes.SadReferenceCheckController.onPageLoad(NormalMode).url)
      }
    }

    "must return OK and the correct view for a GET in Check Mode" in {

      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, controllers.imports.routes.ImportDetailsInfoController.onPageLoad(CheckMode).url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[ImportDetailsInfoView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(form, CheckMode, checkModeBackLinkCall)(request, messages(application)).toString
      }
    }

    "must populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers = UserAnswers(userAnswersId).set(ImportDetailsInfoPage, "answer").success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, importDetailsInfoRoute())

        val view = application.injector.instanceOf[ImportDetailsInfoView]

        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(form.fill("answer"), NormalMode, backLinkCall)(request, messages(application)).toString
      }
    }

    "must redirect to the next page when valid data is submitted for the first time (i.e. referenceInfoCount = 0)" in {
      val applicationId = 3L
      val itemNumber = 2
      val updateSeqNumber = 1
      val referenceInfoCount = 0
      val referenceInfoCountRequest = ReferenceInfoCountRequest(applicationId, itemNumber)
      val referenceInfoCountResponse = ReferenceInfoCountResponse(referenceInfoCount)
      val applicationResponse = ApplicationResponse(applicationId, applicationNumber = "123", updateSeqNumber)
      val addImportResponse = AddImportResponse(itemNumber, updateSeqNumber)

      val userAnswers = emptyUserAnswers
        .set(ClaimApplicationResponseQuery, applicationResponse)
        .success
        .value
        .set(AddImportResponsePage, addImportResponse)
        .success
        .value

      when(mockEuVatRefundsService.getReferenceInfoCount(mockitoEq(referenceInfoCountRequest))(any[HeaderCarrier]()))
        .thenReturn(Future.successful(referenceInfoCountResponse))
      when(mockSessionRepository.set(userAnswers.set(ImportDetailsInfoPage, "answer").success.value))
        .thenReturn(Future.successful(true))

      val application =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, importDetailsInfoRoute())
            .withFormUrlEncodedBody(("value", "answer"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "must redirect to import details warning page when referenceInfoCount > 0 in NormalMode" in {
      val applicationId = 3L
      val itemNumber = 2
      val updateSeqNumber = 1
      val referenceInfoCount = 1
      val referenceInfoCountResponse = ReferenceInfoCountResponse(referenceInfoCount)
      val applicationResponse = ApplicationResponse(applicationId, applicationNumber = "123", updateSeqNumber)
      val addImportResponse = AddImportResponse(itemNumber, updateSeqNumber)

      val userAnswers = emptyUserAnswers
        .set(ClaimApplicationResponseQuery, applicationResponse)
        .success
        .value
        .set(AddImportResponsePage, addImportResponse)
        .success
        .value

      when(mockEuVatRefundsService.getReferenceInfoCount(any())(any()))
        .thenReturn(Future.successful(referenceInfoCountResponse))
      when(mockSessionRepository.set(userAnswers.set(ImportDetailsInfoPage, "answer").success.value))
        .thenReturn(Future.successful(true))

      val application =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, importDetailsInfoRoute())
            .withFormUrlEncodedBody(("value", "answer"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.warning.routes.ImportDetailsWarningController.onPageLoad(NormalMode).url
      }
    }

    "must redirect to import details warning warning page when referenceInfoCount > 0 in CheckMode" in {
      val applicationId = 3L
      val itemNumber = 2
      val updateSeqNumber = 1
      val referenceInfoCount = 1
      val referenceInfoCountResponse = ReferenceInfoCountResponse(referenceInfoCount)
      val applicationResponse = ApplicationResponse(applicationId, applicationNumber = "123", updateSeqNumber)
      val addImportResponse = AddImportResponse(itemNumber, updateSeqNumber)

      val userAnswers = emptyUserAnswers
        .set(ClaimApplicationResponseQuery, applicationResponse)
        .success
        .value
        .set(AddImportResponsePage, addImportResponse)
        .success
        .value

      when(mockEuVatRefundsService.getReferenceInfoCount(any())(any()))
        .thenReturn(Future.successful(referenceInfoCountResponse))
      when(mockSessionRepository.set(userAnswers.set(ImportDetailsInfoPage, "answer").success.value))
        .thenReturn(Future.successful(true))

      val application =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, importDetailsInfoRoute(CheckMode))
            .withFormUrlEncodedBody(("value", "answer"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.warning.routes.ImportDetailsWarningController.onPageLoad(CheckMode).url
      }
    }

    "must return a Bad Request and errors when empty data is submitted" in {

      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request =
          FakeRequest(POST, importDetailsInfoRoute())
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
          FakeRequest(POST, importDetailsInfoRoute())
            .withFormUrlEncodedBody(("value", tooLong))

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
        val request = FakeRequest(GET, importDetailsInfoRoute())

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for a POST if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {
        val request =
          FakeRequest(POST, importDetailsInfoRoute())
            .withFormUrlEncodedBody(("value", "answer"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }
  }
}
