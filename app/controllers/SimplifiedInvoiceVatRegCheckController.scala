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
import forms.SimplifiedInvoiceVatRegCheckFormProvider
import models.requests.DataRequest
import models.{CheckMode, Mode, NormalMode, UserAnswers}
import navigation.Navigator
import pages.{PurchaseTypePage, SimplifiedInvoiceVatRegCheckPage, SupplierAddressPage, SupplierVatRegistrationNumberPage}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.CheckModeShortCircuit
import utils.ControllerHelpers.*
import views.html.SimplifiedInvoiceVatRegCheckView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class SimplifiedInvoiceVatRegCheckController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: SimplifiedInvoiceVatRegCheckFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: SimplifiedInvoiceVatRegCheckView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  val form: Form[Boolean] = formProvider()
  private def backLink: Call = routes.SupplierAddressController.onPageLoad(NormalMode)

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    request.userAnswers.get(SupplierAddressPage) match {
      case None => Redirect(routes.JourneyRecoveryController.onPageLoad())
      case Some(_) =>
        val preparedForm = form.preparedFromAnswers(SimplifiedInvoiceVatRegCheckPage, request.userAnswers)
        okView(preparedForm, mode)
    }
  }

  private def handleCheckModePurchaseNoVat(userAnswersTry: scala.util.Try[UserAnswers]): Future[Result] =
    for {
      afterSet     <- Future.fromTry(userAnswersTry)
      afterCleared <- Future.fromTry(afterSet.remove(SupplierVatRegistrationNumberPage))
      _            <- sessionRepository.set(afterCleared)
    } yield Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad())

  private def handleCheckModePurchaseWithVat(userAnswersTry: scala.util.Try[UserAnswers], mode: Mode): Future[Result] =
    for {
      afterSet <- Future.fromTry(userAnswersTry)
      _        <- sessionRepository.set(afterSet)
    } yield Redirect(routes.SupplierVatRegistrationNumberController.onPageLoad(mode))

  private def handleDefaultFlow(userAnswersTry: scala.util.Try[UserAnswers], value: Boolean, mode: Mode): Future[Result] =
    for {
      persistedAnswers <- Future.fromTry(userAnswersTry)
      _                <- sessionRepository.set(persistedAnswers)
    } yield {
      if (value) {
        Redirect(routes.SupplierVatRegistrationNumberController.onPageLoad(mode))
      } else {
        Redirect(routes.TotalPurchaseAmountBeforeVatController.onPageLoad(mode))
      }
    }

  private def handleChangedAnswer(value: Boolean, mode: Mode)(implicit request: DataRequest[AnyContent]): Future[Result] = {
    val userAnswersTry = request.userAnswers.set(SimplifiedInvoiceVatRegCheckPage, value)

    (mode, request.userAnswers.get(PurchaseTypePage)) match {
      case (CheckMode, Some(_)) if !value =>
        handleCheckModePurchaseNoVat(userAnswersTry)

      case (CheckMode, Some(_)) if value =>
        handleCheckModePurchaseWithVat(userAnswersTry, mode)

      case _ =>
        handleDefaultFlow(userAnswersTry, value, mode)
    }
  }

  private def handleValidSubmit(value: Boolean, mode: Mode)(implicit request: DataRequest[AnyContent]): Future[Result] =
    if (mode == CheckMode && request.userAnswers.isAnswerUnchanged(SimplifiedInvoiceVatRegCheckPage, value)) {
      Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
    } else {
      handleChangedAnswer(value, mode)
    }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(badRequestView(formWithErrors, mode)),
        value => handleValidSubmit(value, mode)
      )
  }

  private def okView(formToRender: Form[?], mode: Mode)(implicit request: DataRequest[?]) =
    Ok(view(formToRender, mode, backLink))

  private def badRequestView(formWithErrors: Form[?], mode: Mode)(implicit request: DataRequest[?]) =
    BadRequest(view(formWithErrors, mode, backLink))
}
