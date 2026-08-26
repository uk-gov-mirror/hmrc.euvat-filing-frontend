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
import forms.SupplierTaxIdentifierNumberFormProvider
import models.{CheckMode, Mode, UserAnswers}
import models.requests.{DataRequest, SupplierTaxIdentifierCountRequest}
import models.responses.{AddPurchaseResponse, SupplierTaxIdentifierCountResponse}
import navigation.Navigator
import pages.*
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.ClaimApplicationResponseQuery
import repositories.SessionRepository
import services.EuVatRefundsService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.ControllerHelpers.*
import views.html.SupplierTaxIdentifierNumberView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class SupplierTaxIdentifierNumberController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  euVatRefundsService: EuVatRefundsService,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: SupplierTaxIdentifierNumberFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: SupplierTaxIdentifierNumberView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  val form: Form[String] = formProvider()

  private def backLink(mode: Mode) = routes.SupplierTaxNumberController.onPageLoad(mode)

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    for {
      updatedAnswers <- Future.fromTry(request.userAnswers.remove(SupplierVatRegistrationNumberPage))
      _              <- sessionRepository.set(updatedAnswers)
    } yield None
    val preparedForm = form.preparedFromAnswers(SupplierTaxIdentifierNumberPage, request.userAnswers)
    Ok(view(preparedForm, mode, backLink(mode)))
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, backLink(mode)))),
        value =>
          val cameFromInvoicePage: Boolean = request.userAnswers.get(SupplierTaxIdentifierArrivedFromInvoicePage).contains(true)

          if (mode == CheckMode && request.userAnswers.get(SupplierTaxIdentifierNumberPage).contains(value) && !cameFromInvoicePage)
            Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
          else {
            // Build the updated UserAnswers (do not persist yet)
            val userAnswersTry = request.userAnswers.set(SupplierTaxIdentifierNumberPage, value)

            userAnswersTry match {
              case scala.util.Failure(_) => Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad()))
              case scala.util.Success(updatedAnswers) =>
                val maybeAppId = updatedAnswers.get(ClaimApplicationResponseQuery).map(_.applicationId.toLong)
                val maybeItem = updatedAnswers.get(AddPurchaseResponsePage).map(_.itemNumber)
                val invoiceNum = updatedAnswers.get(InvoiceNumberPage).getOrElse("")

                (maybeAppId, maybeItem) match {
                  case (Some(appId), Some(itemNumber)) =>
                    val countF =
                      euVatRefundsService.getSupplierTaxIdentifierCount(SupplierTaxIdentifierCountRequest(appId, itemNumber, value, invoiceNum))
                    countF
                      .flatMap {
                        case SupplierTaxIdentifierCountResponse(count) if count > 0 =>
                          val removeArrivedTry = updatedAnswers.remove(SupplierTaxIdentifierArrivedFromInvoicePage)
                          Future.fromTry(removeArrivedTry).flatMap { ua =>
                            sessionRepository.set(ua).map(_ => Redirect(routes.SupplierTaxIdentifierWarningController.onPageLoad(mode)))
                          }

                        case _ =>
                          val clearedTry = for {
                            cleared <- updatedAnswers.remove(SupplierTaxIdentifierWarningShownPage)
                            removed <- cleared.remove(SupplierTaxIdentifierArrivedFromInvoicePage)
                          } yield removed

                          clearUserAnswersAndRedirectToNextPage(mode, clearedTry)
                      }
                      .recover { case _ => Redirect(routes.JourneyRecoveryController.onPageLoad()) }

                  case _ =>
                    // No external check required; clear arrived marker if present, persist and continue
                    val removedTry = updatedAnswers.remove(SupplierTaxIdentifierArrivedFromInvoicePage)
                    clearUserAnswersAndRedirectToNextPage(mode, removedTry)
                }
            }
          }
      )
  }

  private def clearUserAnswersAndRedirectToNextPage(mode: Mode, clearedTry: Try[UserAnswers])(implicit request: DataRequest[?]) = {
    Future.fromTry(clearedTry).flatMap { finalUa =>
      sessionRepository.set(finalUa).map { _ =>
        if (mode == CheckMode && request.userAnswers.get(PurchaseTypePage).isDefined)
          Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad())
        else
          Redirect(navigator.nextPage(SupplierTaxIdentifierNumberPage, mode, finalUa))
      }
    }
  }
}
