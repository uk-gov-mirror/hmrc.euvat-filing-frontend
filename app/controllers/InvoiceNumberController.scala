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
import forms.InvoiceNumberFormProvider
import models.{CheckMode, Mode, NormalMode, UserAnswers}
import navigation.Navigator
import play.api.data.Form
import pages.{InvoiceNumberPage, VrnWarningFlowPage}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.*
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.ControllerHelpers.*
import utils.CheckModeShortCircuit
import views.html.InvoiceNumberView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class InvoiceNumberController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: InvoiceNumberFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: InvoiceNumberView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  val form: Form[String] = formProvider()

  private def backLink(mode: Mode): Call = routes.InvoiceTypeController.onPageLoad(mode)

  private def badRequestView(formWithErrors: Form[?], mode: Mode)(implicit request: Request[AnyContent]) = {
    val html = view(formWithErrors, mode, backLink(mode))(request, messagesApi.preferred(request))
    Future.successful(BadRequest(html))
  }

  private def persistAndRedirect(userAnswersTry: scala.util.Try[UserAnswers], redirectTo: Call): Future[Result] =
    Future
      .fromTry(userAnswersTry)
      .flatMap(ua => sessionRepository.set(ua).map(_ => Redirect(redirectTo)))

  private def redirectForGermanSupplierTax(updated: UserAnswers, mode: Mode): Result =
    updated.get(pages.SupplierTaxNumberPage) match {
      case Some(models.SupplierTaxNumber.Vatregistrationnumber) =>
        Redirect(routes.SupplierVatRegistrationNumberController.onPageLoad(mode))
      case Some(models.SupplierTaxNumber.Taxidentifiernumber) =>
        Redirect(routes.SupplierTaxIdentifierNumberController.onPageLoad(mode))
      case _ =>
        updated.get(pages.SupplierVatRegistrationNumberPage) match {
          case Some(_) => Redirect(routes.SupplierVatRegistrationNumberController.onPageLoad(mode))
          case None =>
            updated.get(pages.SupplierTaxIdentifierNumberPage) match {
              case Some(_) => Redirect(routes.SupplierTaxIdentifierNumberController.onPageLoad(mode))
              case None    => Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad())
            }
        }
    }

  private def persistCheckModeInvoiceNumber(value: String, mode: Mode, userAnswers: UserAnswers, isGermany: Boolean): Future[Result] = {
    val userAnswersTry =
      if (isGermany) {
        for {
          setVal  <- userAnswers.set(InvoiceNumberPage, value)
          marked1 <- setVal.set(pages.SupplierTaxIdentifierArrivedFromInvoicePage, true)
          marked2 <- marked1.set(pages.SupplierVatRegistrationArrivedFromInvoicePage, true)
        } yield marked2
      } else {
        userAnswers.set(InvoiceNumberPage, value)
      }

    Future
      .fromTry(userAnswersTry)
      .flatMap { updated =>
        sessionRepository.set(updated).map { _ =>
          if (isGermany) {
            redirectForGermanSupplierTax(updated, mode)
          } else {
            Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad())
          }
        }
      }
  }

  private def handleCheckModeWithoutWarning(value: String, mode: Mode, userAnswers: UserAnswers): Future[Result] = {
    if (mode == CheckMode && userAnswers.isAnswerUnchanged(InvoiceNumberPage, value)) {
      Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
    } else {
      val isGermany = userAnswers.get(pages.RefundingCountryPage).exists(_.equalsIgnoreCase("DE"))
      persistCheckModeInvoiceNumber(value, mode, userAnswers, isGermany)
    }
  }

  private def handleNormalModeWithoutWarning(value: String, mode: Mode, userAnswers: UserAnswers): Future[Result] =
    CheckModeShortCircuit(
      CheckModeShortCircuit.ShortCircuitArgs(
        InvoiceNumberPage,
        value,
        mode,
        userAnswers,
        sessionRepository,
        navigator.nextPage(InvoiceNumberPage, mode, userAnswers),
        updated => Future.successful(Redirect(navigator.nextPage(InvoiceNumberPage, mode, updated)))
      )
    )

  private def handleWithoutWarning(value: String, mode: Mode, userAnswers: UserAnswers): Future[Result] =
    if (mode == CheckMode) {
      handleCheckModeWithoutWarning(value, mode, userAnswers)
    } else {
      handleNormalModeWithoutWarning(value, mode, userAnswers)
    }

  private def handleWhenWarningAlreadyShown(value: String, mode: Mode, userAnswers: UserAnswers): Future[Result] = {
    val previousInvoice = userAnswers.get(InvoiceNumberPage)

    if (previousInvoice.contains(value)) {
      // If invoice unchanged but a warning flow was active, route back to the
      // appropriate warning page based on which flag is set.
      if (userAnswers.get(pages.VrnWarningFlowPage).contains(true)) {
        Future.successful(Redirect(routes.SupplierVrnWarningController.onPageLoad(mode)))
      } else {
        Future.successful(Redirect(routes.SupplierTaxIdentifierWarningController.onPageLoad(mode)))
      }
    } else {
      // Decide which warning flow was active and route accordingly. There are
      // two warning flags used in the app:
      // - `SupplierTaxIdentifierWarningShownPage` → route to SupplierTaxIdentifierNumber
      // - `VrnWarningFlowPage` → route to SupplierVatRegistrationNumber
      if (userAnswers.get(pages.SupplierTaxIdentifierWarningShownPage).contains(true)) {
        val clearedTry = for {
          setVal  <- userAnswers.set(InvoiceNumberPage, value)
          cleared <- setVal.remove(pages.SupplierTaxIdentifierWarningShownPage)
        } yield cleared

        persistAndRedirect(clearedTry, routes.SupplierTaxIdentifierNumberController.onPageLoad(mode))
      } else if (userAnswers.get(pages.VrnWarningFlowPage).contains(true)) {
        val clearedTry = for {
          setVal  <- userAnswers.set(InvoiceNumberPage, value)
          cleared <- setVal.remove(pages.VrnWarningFlowPage)
        } yield cleared

        persistAndRedirect(clearedTry, routes.SupplierVatRegistrationNumberController.onPageLoad(NormalMode))
      } else {
        // No recognised warning flag set — this indicates an inconsistent
        // state. Fail-safe to Journey Recovery rather than defaulting to a
        // specific supplier flow.
        Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad()))
      }
    }
  }

  private def handleInvoiceNumberSave(value: String, mode: Mode, userAnswers: UserAnswers)(implicit
    request: Request[AnyContent]
  ): Future[Result] = {
    val isSupplierTaxIdentifierWarningShownPage = userAnswers.get(pages.SupplierTaxIdentifierWarningShownPage).contains(true)
    val isVrnWarningFlowPage = userAnswers.get(pages.VrnWarningFlowPage).contains(true)
    val wasShown =
      isSupplierTaxIdentifierWarningShownPage || isVrnWarningFlowPage
    if (wasShown) {
      handleWhenWarningAlreadyShown(value, mode, userAnswers)
    } else {
      handleWithoutWarning(value, mode, userAnswers)
    }
  }

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    val preparedForm = form.preparedFromAnswers(InvoiceNumberPage, request.userAnswers)

    Ok(view(preparedForm, mode, backLink(NormalMode)))
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => badRequestView(formWithErrors, mode),
        value => handleInvoiceNumberSave(value, mode, request.userAnswers)
      )
  }
}
