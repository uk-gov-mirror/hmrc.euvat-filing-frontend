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
import controllers.helpers.PurchaseBackLinkHelper
import forms.DescribeItemsOnInvoiceFormProvider
import models.requests.DataRequest
import models.{CheckMode, Mode, Other, PurchaseType, UserAnswers}
import navigation.Navigator
import pages.{DescribeItemsArrivedFromCheckYourAnswersPage, DescribeItemsOnInvoicePage, PurchaseSubCategoryPage, PurchaseSubTypePage, PurchaseTypePage}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.{CheckModeShortCircuit, ConfigPurchaseMapping, CountryCode}
import utils.ControllerHelpers.*
import views.html.DescribeItemsOnInvoiceView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class DescribeItemsOnInvoiceController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  configPurchaseMapping: ConfigPurchaseMapping,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: DescribeItemsOnInvoiceFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: DescribeItemsOnInvoiceView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  val form: Form[String] = formProvider()

  private def computeBackTarget(mode: Mode)(implicit request: DataRequest[?]): Call =
    if (isPurchaseTypeOther(request)) {
      determineBackForOther(mode)
    } else {
      PurchaseBackLinkHelper.computeBackTarget(mode)
    }

  private def isPurchaseTypeOther(implicit request: DataRequest[?]): Boolean =
    request.userAnswers.get(PurchaseTypePage).contains(Other)

  // Check if the parent sub-type code ends with sentinel '99' (meaning 'None').
  private def parentIndicatesNone(implicit request: DataRequest[?]): Boolean =
    request.userAnswers.get(PurchaseSubTypePage).exists(v => v.split("\\.").lastOption.contains("99"))

  // Check if the child sub-category code ends with sentinel '99'.
  private def childIndicatesNone(implicit request: DataRequest[?]): Boolean =
    request.userAnswers.get(PurchaseSubCategoryPage).exists(v => v.split("\\.").lastOption.contains("99"))

  private def hasMultipleOtherSubcodes(country: String): Boolean =
    try {
      val opts = configPurchaseMapping.subcodesFor(country, "other")
      opts.nonEmpty && opts.size > 1
    } catch { case _: Throwable => false }

  private def determineBackForOther(mode: Mode)(implicit request: DataRequest[?]): Call =
    if (parentIndicatesNone) {
      utils.CountryCode.findCountryCode(request.userAnswers).fold(controllers.routes.PurchaseTypeController.onPageLoad(mode)) { country =>
        if (hasMultipleOtherSubcodes(country))
          controllers.purchase.routes.PurchaseSubTypeController.onPageLoad(PurchaseType.urlSlugForPurchaseType(Other), mode)
        else
          controllers.routes.PurchaseTypeController.onPageLoad(mode)
        }
    } else if (childIndicatesNone) {
      controllers.routes.PurchaseTypeController.onPageLoad(mode)
    } else {
      PurchaseBackLinkHelper.computeBackTarget(mode)
    }

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    val preparedForm = form.preparedFromAnswers(DescribeItemsOnInvoicePage, request.userAnswers)

    val backTarget = computeBackTarget(mode)
    val arrived = request.userAnswers.get(DescribeItemsArrivedFromCheckYourAnswersPage).contains(true)

    if (mode == CheckMode && !arrived) {
      val markedTry = request.userAnswers.set(DescribeItemsArrivedFromCheckYourAnswersPage, true)
      Future.fromTry(markedTry).flatMap { updated =>
        sessionRepository.set(updated).map(_ => Ok(view(preparedForm, mode, backTarget)))
      }
    } else {
      Future.successful(Ok(view(preparedForm, mode, backTarget)))
    }
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          if (formWithErrors.errors.exists(_.message == "describeItemsOnInvoice.error.required")) {
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(DescribeItemsOnInvoicePage, ""))
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(routes.PurchaseWarningController.onPageLoad(mode))
          } else {
            Future.successful(BadRequest(view(formWithErrors, mode, computeBackTarget(mode))))
          },
        value =>
          if (mode == CheckMode) {
            CheckModeShortCircuit(
              CheckModeShortCircuit.ShortCircuitArgs(
                DescribeItemsOnInvoicePage,
                value,
                mode,
                request.userAnswers,
                sessionRepository,
                controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad(),
                updated => Future.successful(Redirect(navigator.nextPage(DescribeItemsOnInvoicePage, mode, updated)))
              )
            )
          } else {
            CheckModeShortCircuit(
              CheckModeShortCircuit.ShortCircuitArgs(
                DescribeItemsOnInvoicePage,
                value,
                mode,
                request.userAnswers,
                sessionRepository,
                navigator.nextPage(DescribeItemsOnInvoicePage, mode, request.userAnswers),
                updated => Future.successful(Redirect(navigator.nextPage(DescribeItemsOnInvoicePage, mode, updated)))
              )
            )
          }
      )
  }
}
