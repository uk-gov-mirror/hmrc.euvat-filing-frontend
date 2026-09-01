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

package controllers.purchase

import controllers.actions.{DataRequiredAction, DataRetrievalAction, IdentifierAction}
import pages.*
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.EuVatRefundsService
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import models.requests.UpdatePurchaseRequest
import models.responses.AddPurchaseResponse
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.{ConfigPurchaseMapping, CountryCode, CurrencyConfig}
import viewmodels.checkAnswers.CheckYourPurchaseDetailsSummary
import views.html.CheckYourPurchaseDetailsView

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class CheckYourPurchaseDetailsController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  view: CheckYourPurchaseDetailsView,
  currencyConfig: CurrencyConfig,
  configPurchaseMapping: ConfigPurchaseMapping,
  sessionRepository: SessionRepository,
  euVatRefundsService: EuVatRefundsService
)(using ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  def onPageLoad(): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    implicit val msgs: Messages = messagesApi.preferred(request)
    lazy val currencyList =
      CountryCode
        .findCountryCode(request.userAnswers)
        .map(currencyConfig.currencyConfig(_))
        .getOrElse(currencyConfig.default)

    val (maybeCurrencyDisplayName, maybeCurrencySymbol): (Option[String], Option[String]) =
      request.userAnswers
        .get(RefundingCurrencyPage)
        .flatMap(code => currencyList.find(_.code == code))
        .map(currency => Some(msgs(s"refundingCurrency.${currency.name}", currency.symbol)) -> Some(currency.symbol))
        .orElse(Option.when(currencyList.lengthCompare(1) > 0)(Some(msgs("site.notProvided")) -> None))
        .getOrElse(None -> None)

    Ok(
      view(
        CheckYourPurchaseDetailsSummary
          .sections(
            request.userAnswers,
            maybeCurrencyDisplayName,
            maybeCurrencySymbol,
            configPurchaseMapping,
            currencyList.size > 1
          ),
        isPostSubmission = false,
        isAmended        = false
      )
    )
  }

  def onSubmit(): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    implicit val hc = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

    val maybeAppId = request.userAnswers.get(queries.ClaimApplicationResponseQuery).map(_.applicationId.toLong)
    val maybeAddResp = request.userAnswers.get(AddPurchaseResponsePage)

    (maybeAppId, maybeAddResp) match {
      case (Some(appId), Some(addResp)) =>
        val goodsCategory = request.userAnswers.get(pages.PurchaseSubTypePage).getOrElse("")
        val goodsDescriptionSubCategory = request.userAnswers.get(pages.PurchaseSubCategoryPage)
        val goodsText = request.userAnswers.get(pages.DescribeItemsOnInvoicePage)
        val simplified = request.userAnswers.get(pages.SimplifiedInvoiceVatRegCheckPage) match {
          case Some(b) => Some(if (b) "true" else "false")
          case None => // fall back to invoice type when the explicit flag isn't present
            request.userAnswers.get(pages.InvoiceTypePage) match {
              case Some(models.InvoiceType.SimplifiedInvoice) => Some("true")
              case Some(_)                                     => Some("false")
              case None                                        => None
            }
        }
        val supplierName = request.userAnswers.get(pages.SuppliersNamePage)
        val supplierAddr = request.userAnswers.get(pages.SupplierAddressPage)
        val supplierAddress1 = supplierAddr.map(_.line1)
        val supplierAddress2 = supplierAddr.flatMap(_.line2)
        val supplierAddress3 = supplierAddr.flatMap(_.line3)
        val supplierVatRegNumber = request.userAnswers.get(pages.SupplierVatRegistrationNumberPage)
        val supplierTaxId = request.userAnswers.get(pages.SupplierTaxIdentifierNumberPage)
        val invoiceDate = request.userAnswers.get(pages.InvoiceDatePage).map(_.atStartOfDay())
        val invoiceNumber = request.userAnswers.get(pages.InvoiceNumberPage)
        val currencyCode = request.userAnswers.get(pages.RefundingCurrencyPage)
        val taxableAmount = request.userAnswers.get(pages.TotalPurchaseAmountBeforeVatPage)
        val vatAmount = request.userAnswers.get(pages.TotalVatPaidPage)
        val deductibleVatAmount = request.userAnswers.get(pages.TotalVatClaimPage)

        val updateReq = UpdatePurchaseRequest(
          applicationId = appId,
          itemNumber = addResp.itemNumber,
          goodsDescriptionCategory = goodsCategory,
          goodsDescriptionSubCategory = goodsDescriptionSubCategory,
          goodsDescriptionText = goodsText,
          simplifiedInvoiceIndicator = simplified,
          supplierName = supplierName,
          supplierAddress1 = supplierAddress1,
          supplierAddress2 = supplierAddress2,
          supplierAddress3 = supplierAddress3,
          supplierVatRegNumber = supplierVatRegNumber,
          supplierTaxIdentifier = supplierTaxId,
          invoiceDate = invoiceDate,
          invoiceNumber = invoiceNumber,
          currencyCode = currencyCode,
          taxableAmount = taxableAmount,
          vatAmount = vatAmount,
          deductibleVatAmount = deductibleVatAmount,
          updateSequenceNumber = addResp.updateSequenceNumber
        )

        euVatRefundsService
          .updatePurchase(updateReq)
          .flatMap { resp =>
            val updatedAddResp = AddPurchaseResponse(itemNumber = addResp.itemNumber, updateSequenceNumber = resp.updateSequenceNumber)
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(AddPurchaseResponsePage, updatedAddResp))
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(controllers.routes.TaskListDashboardController.onPageLoad())
          }
          .recover { case ex =>
            play.api.Logger(this.getClass).error("Error updating purchase details", ex)
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
          }

      case _ =>
        play.api.Logger(this.getClass).warn("Missing applicationId or itemNumber for update-purchase-details")
        Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
    }
  }
}
