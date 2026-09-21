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

import controllers.actions.*
import forms.imports.SadReferenceFormProvider
import pages.SadReferencePage
import models.requests.DataRequest
import navigation.Navigator
import models.{Mode, NormalMode}

import javax.inject.Inject
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.imports.SadReferenceView

import scala.concurrent.{ExecutionContext, Future}

class SadReferenceController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: SadReferenceFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: SadReferenceView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  val form: Form[Boolean] = formProvider()

  private def computeBackLink(implicit request: DataRequest[AnyContent]): Call =
    (request.userAnswers.get(pages.ImportTypePage), request.userAnswers.get(pages.ImportSubCodePage)) match {
      case (Some(importType), Some(_)) => controllers.imports.routes.ImportSubCodeController.onPageLoad(importType.toString)
      case _                           => controllers.imports.routes.ImportTypeController.onPageLoad(models.NormalMode)
    }

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(SadReferencePage).fold(form)(form.fill)
    Ok(view(preparedForm, computeBackLink))
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(BadRequest(view(formWithErrors, computeBackLink))),
        value =>
          for {
            updated <- Future.fromTry(request.userAnswers.set(SadReferencePage, value))
            _       <- sessionRepository.set(updated)
          } yield Redirect(navigator.nextPage(SadReferencePage, mode, updated))
      )
  }

}
