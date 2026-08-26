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

package controllers

import controllers.actions.*
import forms.BusinessActivityTwoFormProvider

import javax.inject.Inject
import models.{Mode, NormalMode}
import navigation.Navigator
import pages.{BusinessActivityCodePage, BusinessActivityCodeThreePage, BusinessActivityCodeTwoPage, BusinessActivityTwoPage}
import play.api.Logging
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.ControllerHelpers.*
import views.html.BusinessActivityTwoView

import scala.concurrent.{ExecutionContext, Future}

class BusinessActivityTwoController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: BusinessActivityTwoFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: BusinessActivityTwoView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form: Form[Boolean] = formProvider()

  private def backLink: Call = routes.BusinessActivityCodeTwoController.onPageLoad(NormalMode)

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    val userAnswers = request.userAnswers
    (userAnswers.get(BusinessActivityCodePage), userAnswers.get(BusinessActivityCodeTwoPage)) match {
      case (Some(baCode1), Some(baCode2)) =>
        val preparedForm = form.preparedFromAnswers(BusinessActivityTwoPage, userAnswers)

        Ok(view(preparedForm, mode, backLink, baCode1, baCode2)).addingToSession("removeOrigin" -> "business-activity-2")(request)

      case _ =>
        logger.warn("Data guard error, missing required information")
        Redirect(routes.UnauthorisedController.onPageLoad()) // TODO - update to system guard error page when ready
    }

  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    val userAnswers = request.userAnswers
    (userAnswers.get(BusinessActivityCodePage), userAnswers.get(BusinessActivityCodeTwoPage)) match {
      case (Some(baCode1), Some(baCode2)) =>
        form
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Future.successful(
                BadRequest(view(formWithErrors, mode, backLink, baCode1, baCode2)).addingToSession("removeOrigin" -> "business-activity-2")
              ),
            value =>
              for {
                updateAnswers <- Future.fromTry(userAnswers.set(BusinessActivityTwoPage, value))
                updatedAnswers <- if (value) {
                                    Future.successful(updateAnswers)
                                  } else {
                                    Future.fromTry(updateAnswers.remove(BusinessActivityCodeThreePage))
                                  }
                _ <- sessionRepository.set(updatedAnswers)
              } yield Redirect(navigator.nextPage(BusinessActivityTwoPage, mode, updatedAnswers))
          )

      case _ =>
        logger.warn("Data guard error, missing required information")
        Future.successful(Redirect(routes.UnauthorisedController.onPageLoad())) // TODO - update to system guard error page when ready
    }
  }
}
