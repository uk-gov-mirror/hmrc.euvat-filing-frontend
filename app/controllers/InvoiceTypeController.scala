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
import forms.InvoiceTypeFormProvider
import models.requests.DataRequest
import models.{CheckMode, InvoiceType, Mode, Other, PurchaseType, UserAnswers}
import navigation.Navigator
import pages.*
import play.api.Logging
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.{ConfigPurchaseMapping, CountryCode}
import utils.ControllerHelpers.*
import views.html.InvoiceTypeView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class InvoiceTypeController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  configPurchaseMapping: ConfigPurchaseMapping,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: InvoiceTypeFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: InvoiceTypeView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form: Form[InvoiceType] = formProvider()

  private def badRequestView(formWithErrors: play.api.data.Form[?], mode: Mode)(implicit request: DataRequest[?]): Future[play.api.mvc.Result] = {
    val html = view(formWithErrors, mode, computeBackTarget(mode))(request, messagesApi.preferred(request))
    Future.successful(BadRequest(html))
  }

  private def computeBackTarget(mode: Mode)(implicit request: DataRequest[?]): Call = {
    def parentIsNone = request.userAnswers.get(PurchaseSubTypePage).exists(v => v.split("\\.").lastOption.contains("99"))
    def childIsNone = request.userAnswers.get(PurchaseSubCategoryPage).exists(v => v.split("\\.").lastOption.contains("99"))

    def isOther = request.userAnswers.get(PurchaseTypePage).contains(Other)

    if (!isOther) {
      PurchaseBackLinkHelper.computeBackTarget(mode)
    } else if (parentIsNone || childIsNone) {
      controllers.routes.DescribeItemsOnInvoiceController.onPageLoad(mode)
    } else {
      PurchaseBackLinkHelper.computeBackTarget(mode)
    }
  }

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    val preparedForm = form.preparedFromAnswers(InvoiceTypePage, request.userAnswers)

    val back = computeBackTarget(mode)

    Future.successful(Ok(view(preparedForm, mode, back)))
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => badRequestView(formWithErrors, mode),
        value => {
          if (mode == CheckMode && request.userAnswers.isAnswerUnchanged(InvoiceTypePage, value)) {
            Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
          } else {
            persistAndRedirect(value, mode)
          }
        }
      )
  }

  private def persistAndRedirect(value: InvoiceType, mode: Mode)(implicit request: DataRequest[?]): Future[play.api.mvc.Result] = {
    val userAnswersTry = buildUpdatedTry(value)

    for {
      builtAnswers <- Future.fromTry(userAnswersTry)
      answersWithChangeFlag <-
        if (mode == CheckMode) Future.fromTry(builtAnswers.set(InvoiceTypeChangedPage, true)) else Future.successful(builtAnswers)
      _ <- sessionRepository.set(answersWithChangeFlag)
    } yield postPersistRedirect(mode, value, builtAnswers)
  }

  private def buildUpdatedTry(value: InvoiceType)(implicit request: DataRequest[?]): Try[UserAnswers] =
    request.userAnswers.get(InvoiceTypePage) match {
      case Some(prev) if prev != value =>
        for {
          a <- request.userAnswers.remove(SupplierTaxNumberPage)
          b <- a.remove(SimplifiedInvoiceVatRegCheckPage)
          c <- b.remove(SupplierVatRegistrationNumberPage)
          d <- c.set(InvoiceTypePage, value)
        } yield d
      case _ => request.userAnswers.set(InvoiceTypePage, value)
    }

  private def postPersistRedirect(mode: Mode, value: InvoiceType, updatedAnswers: UserAnswers)(implicit request: DataRequest[?]) = {
    if (mode == CheckMode) {
      val countryOpt = CountryCode.findCountryCode(request.userAnswers)
      countryOpt match {
        case Some("DE") => Redirect(controllers.routes.SupplierTaxNumberController.onPageLoad(CheckMode))
        case _ =>
          value match {
            case InvoiceType.StandardInvoice   => Redirect(controllers.routes.SupplierVatRegistrationNumberController.onPageLoad(CheckMode))
            case InvoiceType.SimplifiedInvoice => Redirect(controllers.routes.SimplifiedInvoiceVatRegCheckController.onPageLoad(CheckMode))
          }
      }
    } else {
      Redirect(navigator.nextPage(InvoiceTypePage, mode, updatedAnswers))
    }
  }
}
