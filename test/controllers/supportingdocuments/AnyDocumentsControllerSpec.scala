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

package controllers.supportingdocuments

import base.SpecBase
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import pages.supportingdocuments.AnyDocumentsPage
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import repositories.SessionRepository

import scala.concurrent.Future

class AnyDocumentsControllerSpec extends SpecBase {

  "AnyDocuments Controller" - {

    "must display the supporting documents question" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, routes.AnyDocumentsController.onPageLoad().url)
        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include(messages(application)("supportingDocuments.anyDocuments.heading"))
        contentAsString(result) must include(messages(application)("supportingDocuments.caption"))
      }
    }

    "must show an error when no answer is selected" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(POST, routes.AnyDocumentsController.onSubmit().url)
        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        contentAsString(result) must include(messages(application)("supportingDocuments.anyDocuments.error.required"))
      }
    }

    "must save yes and redirect to the upload file page" in {
      when(mockSessionRepository.set(any())).thenReturn(Future.successful(true))
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.AnyDocumentsController.onSubmit().url)
          .withFormUrlEncodedBody("value" -> "true")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual "/file-eu-vat/supporting-documents/upload-file"
        val savedAnswers = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        org.mockito.Mockito.verify(mockSessionRepository).set(savedAnswers.capture())
        savedAnswers.getValue.get(AnyDocumentsPage) mustBe Some(true)
      }
    }

    "must save no and redirect to the task list" in {
      when(mockSessionRepository.set(any())).thenReturn(Future.successful(true))
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers))
        .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.AnyDocumentsController.onSubmit().url)
          .withFormUrlEncodedBody("value" -> "false")
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.routes.TaskListDashboardController.onPageLoad().url
        val savedAnswers = org.mockito.ArgumentCaptor.forClass(classOf[models.UserAnswers])
        org.mockito.Mockito.verify(mockSessionRepository).set(savedAnswers.capture())
        savedAnswers.getValue.get(AnyDocumentsPage) mustBe Some(false)
      }
    }
  }
}
