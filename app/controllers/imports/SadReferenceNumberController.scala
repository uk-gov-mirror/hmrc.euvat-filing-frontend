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
import forms.imports.SadReferenceNumberFormProvider
import pages.SadReferenceNumberPage
import navigation.Navigator
import models.{Mode, NormalMode}

import javax.inject.Inject
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.imports.SadReferenceNumberView

import scala.concurrent.{ExecutionContext, Future}

class SadReferenceNumberController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: SadReferenceNumberFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: SadReferenceNumberView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  val form: Form[String] = formProvider()

  private def backLink(mode: Mode): Call = controllers.imports.routes.SadReferenceController.onPageLoad(mode)

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(SadReferenceNumberPage).fold(form)(form.fill)
    Ok(view(preparedForm, backLink(mode)))
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(BadRequest(view(formWithErrors, backLink(mode)))),
        value =>
          for {
            updated <- Future.fromTry(request.userAnswers.set(SadReferenceNumberPage, value))
            _       <- sessionRepository.set(updated)
          } yield Redirect(navigator.nextPage(SadReferenceNumberPage, mode, updated))
      )
  }

}
