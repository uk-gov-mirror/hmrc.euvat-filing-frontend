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

import controllers.actions.{DataRequiredAction, DataRetrievalAction, IdentifierAction}
import forms.InvoiceDateFormProvider
import models.Mode
import models.requests.DataRequest
import navigation.Navigator
import pages.{InvoiceDatePage, PurchaseTypePage}
import play.api.data.Form
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.*
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.ControllerHelpers.*
import views.html.InvoiceDateView

import java.time.LocalDate
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class InvoiceDateController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: InvoiceDateFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: InvoiceDateView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  private def form(implicit messages: Messages) = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    request.userAnswers.get(pages.RefundPeriodPage) match {
      case None =>
        Redirect(routes.JourneyRecoveryController.onPageLoad())

      case Some(_) =>
        val preparedForm = form.preparedFromAnswers(InvoiceDatePage, request.userAnswers)
        Ok(view(preparedForm, mode, routes.InvoiceNumberController.onPageLoad(mode)))
    }
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => badRequestToInvoiceNumber(formWithErrors, mode),
        value =>
          request.userAnswers.get(pages.RefundPeriodPage) match {
            case None => Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad()))
            case Some(refundPeriod) =>
              val today = java.time.LocalDate.now()
              if (value.isAfter(today)) {
                val errorForm = form.bindFromRequest().withError("value", "invoiceDate.error.past")
                badRequestToInvoiceNumber(errorForm, mode)
              }
              /*
            TODO: business rule to prevent users entering a date outside the refund period
            this is currently commented out as
            it is not yet clear whether this validation will be possible at this point in the journey.
            Once the refund period can be calculated, this validation should be added back in
            and the relevant error message added to the messages file.

            else if (value.isBefore(refundPeriod.startDate) || value.isAfter(refundPeriod.endDate)) {
              val errorForm = form.withError("value", "invoiceDate.error.outsideRefundPeriod")
              Future.successful(BadRequest(view(errorForm, mode, routes.InvoiceNumberController.onPageLoad(mode))))
            } */
              else {
                handleSubmission(value, mode)(request)
              }
          }
      )
  }

  private def badRequestToInvoiceNumber(formWithErrors: Form[?], mode: Mode)(implicit
    request: Request[AnyContent]
  ): Future[play.api.mvc.Result] = {
    val html = view(formWithErrors, mode, routes.InvoiceNumberController.onPageLoad(mode))(request, messagesApi.preferred(request))
    Future.successful(BadRequest(html))
  }

  private def persistAndRedirectToNext(value: LocalDate, mode: Mode)(implicit request: DataRequest[?]): Future[Result] =
    for {
      persistedAnswers <- Future.fromTry(request.userAnswers.set(InvoiceDatePage, value))
      _                <- sessionRepository.set(persistedAnswers)
    } yield Redirect(navigator.nextPage(InvoiceDatePage, mode, persistedAnswers))

  private def handleSubmission(value: LocalDate, mode: Mode)(implicit request: DataRequest[?]): Future[Result] = {
    if (mode == models.CheckMode && request.userAnswers.get(PurchaseTypePage).isDefined) {
      request.userAnswers.get(InvoiceDatePage) match {
        case Some(prev) if prev == value =>
          Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))

        case _ =>
          if (mode == models.CheckMode && request.userAnswers.isAnswerUnchanged(InvoiceDatePage, value)) {
            Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
          } else {
            for {
              persistedAnswers <- Future.fromTry(request.userAnswers.set(InvoiceDatePage, value))
              _                <- sessionRepository.set(persistedAnswers)
            } yield Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad())
          }
      }
    } else {
      persistAndRedirectToNext(value, mode)
    }
  }
}
