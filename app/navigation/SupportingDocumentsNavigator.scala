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

import controllers.supportingdocuments.routes as supportingDocumentsRoutes
import models.UserAnswers
import pages.supportingdocuments.AnyDocumentsPage
import play.api.mvc.Call

import javax.inject.{Inject, Singleton}

@Singleton
class SupportingDocumentsNavigator @Inject() () {

  def navigateFromBeforeYouStartPage(): Call =
    supportingDocumentsRoutes.AnyDocumentsController.onPageLoad()

  def navigateFromAnyDocumentsPage(userAnswers: UserAnswers): Call =
    userAnswers.get(AnyDocumentsPage) match {
      case Some(true)  => supportingDocumentsRoutes.UploadFileController.onPageLoad()
      case Some(false) => controllers.routes.TaskListDashboardController.onPageLoad()
      case None        => controllers.routes.JourneyRecoveryController.onPageLoad()
    }
}
