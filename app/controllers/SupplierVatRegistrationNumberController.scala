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
import forms.SupplierVatRegistrationNumberFormProvider
import models.requests.{DataRequest, SupplierVrnCountRequest}
import models.{CheckMode, InvoiceType, Mode, UserAnswers}
import navigation.Navigator
import pages.*
import play.api.data.Form
import play.api.i18n.Lang.logger
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.*
import queries.ClaimApplicationResponseQuery
import repositories.SessionRepository
import services.EuVatRefundsService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import utils.ControllerHelpers.*
import views.html.SupplierVatRegistrationNumberView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class SupplierVatRegistrationNumberController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: SupplierVatRegistrationNumberFormProvider,
  val controllerComponents: MessagesControllerComponents,
  euVatRefundsService: EuVatRefundsService,
  view: SupplierVatRegistrationNumberView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  val form: Form[String] = formProvider()

  private def backLink(mode: Mode)(implicit request: DataRequest[?]): Call = {
    val warningActive = request.userAnswers.get(VrnWarningFlowPage).isDefined
    val isGermany = request.userAnswers.get(RefundingCountryPage).exists(_.equalsIgnoreCase("DE"))
    val isSimplified = request.userAnswers.get(InvoiceTypePage).contains(InvoiceType.SimplifiedInvoice)

    (warningActive, isGermany, isSimplified) match {
      case (true, _, _)     => routes.InvoiceNumberController.onPageLoad(mode)
      case (_, true, _)     => routes.SupplierTaxNumberController.onPageLoad(mode)
      case (_, false, true) => routes.SimplifiedInvoiceVatRegCheckController.onPageLoad(mode)
      case _                => routes.SupplierAddressController.onPageLoad(mode)
    }
  }

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    // Remove any lingering SupplierTaxIdentifierNumber as VAT reg number page takes precedence
    for {
      updatedAnswers <- Future.fromTry(request.userAnswers.remove(SupplierTaxIdentifierNumberPage))
      _              <- sessionRepository.set(updatedAnswers)
    } yield None

    // Prepare the form pre-filling from session when present
    val preparedForm = form.preparedFromAnswers(SupplierVatRegistrationNumberPage, request.userAnswers)

    // Detect whether refunding country is Germany to inform the view
    val isGermany = request.userAnswers.get(RefundingCountryPage).exists(_.equalsIgnoreCase("DE"))
    okView(preparedForm, mode, isGermany)
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    val isGermany = request.userAnswers.get(RefundingCountryPage).exists(_.equalsIgnoreCase("DE"))
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(badRequestView(formWithErrors, mode, isGermany)),
        value => {
          if (shouldShortCircuit(value, mode, request.userAnswers)) {
            Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
          } else {
            buildFinalAnswersTry(request.userAnswers, value) match {
              case Failure(_) => Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad()))
              case Success(finalAnswers) =>
                checkDuplicate(value, finalAnswers, mode).flatMap {
                  case Left(res)      => Future.successful(res)
                  case Right(cleaned) => persistAndRedirect(Success(cleaned), mode)
                }
            }
          }
        }
      )
  }

  private def shouldShortCircuit(value: String, mode: Mode, answers: UserAnswers): Boolean = {
    val isCheckMode = mode == CheckMode
    val isSupplierVatRegistrationNumber = answers.get(SupplierVatRegistrationNumberPage).contains(value)
    val isSupplierVatRegistrationArrivedFromInvoicePage = answers.get(SupplierVatRegistrationArrivedFromInvoicePage).contains(true)
    isCheckMode && isSupplierVatRegistrationNumber && !isSupplierVatRegistrationArrivedFromInvoicePage
  }

  private def buildFinalAnswersTry(answers: UserAnswers, value: String): Try[UserAnswers] = {
    val changed = !answers.get(SupplierVatRegistrationNumberPage).contains(value)

    for {
      updated      <- answers.set(SupplierVatRegistrationNumberPage, value)
      withFlag     <- if (answers.get(VrnWarningFlowPage).isDefined && changed) updated.set(VrnWarningFlowPage, false) else Success(updated)
      finalAnswers <- withFlag.remove(pages.SupplierVatRegistrationArrivedFromInvoicePage)
    } yield finalAnswers
  }

  private def okView(preparedForm: Form[String], mode: Mode, isGermany: Boolean)(implicit request: DataRequest[?]) =
    Ok(view(preparedForm, mode, backLink(mode), isGermany))

  private def badRequestView(formWithErrors: Form[String], mode: Mode, isGermany: Boolean)(implicit request: DataRequest[?]) =
    BadRequest(view(formWithErrors, mode, backLink(mode), isGermany))

  private def persistAndRedirect(userAnswersTry: Try[UserAnswers], mode: Mode)(implicit
    request: DataRequest[?]
  ): Future[Result] =
    for {
      persisted <- Future.fromTry(userAnswersTry)
      _         <- sessionRepository.set(persisted)
    } yield {
      if (mode == CheckMode) {
        Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad())
      } else {
        Redirect(navigator.nextPage(SupplierVatRegistrationNumberPage, mode, persisted))
      }
    }

  private def buildSupplierVrnCountRequest(answers: UserAnswers, vatNumber: String): Option[SupplierVrnCountRequest] =
    for {
      applicationId <- answers.get(ClaimApplicationResponseQuery).map(_.applicationId)
      itemNumber    <- answers.get(AddPurchaseResponsePage).map(_.itemNumber)
      invoiceNumber <- answers.get(InvoiceNumberPage)
    } yield SupplierVrnCountRequest(applicationId.toLong, itemNumber, vatNumber, invoiceNumber)

  private def checkDuplicate(vatNumber: String, answers: UserAnswers, mode: Mode)(implicit
    request: DataRequest[?]
  ): Future[Either[Result, UserAnswers]] = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)
    buildSupplierVrnCountRequest(answers, vatNumber) match {
      case Some(req) =>
        euVatRefundsService
          .getSupplierVrnCount(req)
          .flatMap { response =>
            if (response.duplicateCount > 0) {
              // Duplicate: remove transient arrived flag and persist, then redirect to warning
              val removeArrivedTry = answers.remove(SupplierVatRegistrationArrivedFromInvoicePage)
              Future.fromTry(removeArrivedTry).flatMap { ua =>
                sessionRepository.set(ua).map(_ => Left(Redirect(routes.SupplierVrnWarningController.onPageLoad(mode))))
              }
            } else {
              // Not duplicate: compute cleaned answers (do not persist here), delegate persistence to caller
              val clearedTry = for {
                cleared <- answers.remove(VrnWarningFlowPage)
                removed <- cleared.remove(SupplierVatRegistrationArrivedFromInvoicePage)
              } yield removed

              clearedTry match {
                case Success(cleaned) => Future.successful(Right(cleaned))
                case Failure(_)       => Future.successful(Left(Redirect(routes.JourneyRecoveryController.onPageLoad())))
              }
            }
          }
          .recover { case ex: Exception =>
            logger.error("Error retrieving supplier VRN count", ex)
            Left(Redirect(routes.JourneyRecoveryController.onPageLoad()))
          }
      case None =>
        logger.warn("Missing data for duplicate VRN check; skipping external check and persisting answers")
        Future.successful(Right(answers))
    }
  }
}
