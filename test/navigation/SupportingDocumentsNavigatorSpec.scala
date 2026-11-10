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

package navigation

import base.SpecBase
import controllers.supportingdocuments.routes as supportingDocumentsRoutes
import pages.supportingdocuments.AnyDocumentsPage

class SupportingDocumentsNavigatorSpec extends SpecBase {

  private val navigator = new SupportingDocumentsNavigator()

  "SupportingDocumentsNavigator" - {

    "must go from the before you start page to AnyDocumentsController" in {
      navigator.navigateFromBeforeYouStartPage() mustBe supportingDocumentsRoutes.AnyDocumentsController.onPageLoad()
    }

    "must go from AnyDocumentsPage to UploadFileController when the user has documents" in {
      val answers = emptyUserAnswers.set(AnyDocumentsPage, true).success.value

      navigator.navigateFromAnyDocumentsPage(answers) mustBe supportingDocumentsRoutes.UploadFileController.onPageLoad()
      navigator.navigateFromAnyDocumentsPage(answers).url mustBe "/file-eu-vat/supporting-documents/upload-file"
    }

    "must go from AnyDocumentsPage to TaskListDashboardController when the user has no documents" in {
      val answers = emptyUserAnswers.set(AnyDocumentsPage, false).success.value

      navigator.navigateFromAnyDocumentsPage(answers) mustBe controllers.routes.TaskListDashboardController.onPageLoad()
    }

    "must go from AnyDocumentsPage to Journey Recovery when no answer is saved" in {
      navigator.navigateFromAnyDocumentsPage(emptyUserAnswers) mustBe controllers.routes.JourneyRecoveryController.onPageLoad()
    }
  }
}
