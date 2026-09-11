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

package viewmodels.checkAnswers

import controllers.purchase.routes
import models.{CheckMode, PurchaseOrImportType, UserAnswers}
import pages.*
import play.api.i18n.{Lang, Messages}
import play.api.mvc.RequestHeader
import utils.{ConfigPurchaseOrImportMapping, MountPrefix}

object CheckYourPurchaseDetailsSummary {

  type Row = (String, Option[String], Seq[(String, String, String)])

  def rowPurchaseType(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(PurchaseTypePage).map { pt =>
      val value = messages(s"purchaseType.${pt.toString}")
      val url = routes.PurchaseTypeController.onPageLoad(CheckMode).url
      (
        messages("purchaseType.checkYourAnswersLabel"),
        Some(value),
        Seq((url, "site.change", "purchaseType.change.hidden"))
      )
    }

  def rowPurchaseSubTypeLabel(answers: UserAnswers, config: ConfigPurchaseOrImportMapping)(implicit messages: Messages): Option[Row] = {
    answers.get(PurchaseTypePage) match {
      case None => None
      case Some(pt) =>
        val parentKey = pt.toString

        val countryOpt =
          answers
            .get(RefundingCountryPage)
            .orElse {
              answers
                .get(RefundingCountryNamePage)
                .map(_.split(",", 2).last.trim)
            }

        val hasSubcodes = countryOpt
          .flatMap { c =>
            try Some(config.subcodesFor(c, parentKey).nonEmpty)
            catch {
              case _: Throwable => None
            }
          }
          .getOrElse(true)

        if (!hasSubcodes) {
          None
        } else {
          answers.get(PurchaseSubTypePage) match {
            case Some(v) if v == ConfigPurchaseOrImportMapping.NoneValue || v.split("\\.").lastOption.contains("99") =>
              val singleBypass = countryOpt.flatMap { c =>
                try {
                  val opts = config.subcodesFor(c, parentKey)
                  if (opts.nonEmpty && opts.size == 1) Some(opts.head._1) else None
                } catch {
                  case _: Throwable => None
                }
              }

              singleBypass match {
                case Some(singleCode) if singleCode.split("\\.").lastOption.contains("99") => None
                case _                                                                     => renderSubTypeRow(answers, pt)
              }

            case _ => answers.get(PurchaseTypePage).flatMap(renderSubTypeRow(answers, _))
          }
        }
    }
  }

  private def renderSubTypeRow(answers: UserAnswers, pt: models.PurchaseOrImportType)(implicit messages: Messages): Option[Row] = {
    val parentSlug = PurchaseOrImportType.urlSlugForPurchaseType(pt)
    val msgKey = s"purchase.subType.$parentSlug"
    val keyLabel = if (messages.isDefinedAt(msgKey)) messages(msgKey) else parentSlug.replace('-', ' ').capitalize

    val valueOpt: Option[String] = answers.get(PurchaseSubTypeLabelPage)
    val displayValueOpt: Option[String] = answers
      .get(PurchaseSubTypeLabelPage)
      .map {
        case ConfigPurchaseOrImportMapping.NoneValue => messages("site.none")
        case value                                   => value
      }
    val changeUrl = routes.PurchaseSubTypeController.onPageLoad(parentSlug, CheckMode).url

    Some((keyLabel, displayValueOpt, Seq((changeUrl, "site.change", "purchase.subType.change.hidden"))))
  }

  def rowPurchaseSubCategoryLabel(answers: UserAnswers)(implicit messages: Messages, request: RequestHeader): Option[Row] =
    for {
      pt    <- answers.get(PurchaseTypePage)
      code  <- answers.get(PurchaseSubCategoryPage)
      label <- answers.get(PurchaseSubCategoryLabelPage)
    } yield {
      val parentKey = pt.toString

      def findSlug(pk: String, c: String): String = {
        def loop(curr: String): Option[String] =
          models.PurchaseOrImportSubCategoryType.purchaseOrImportSubCategoryUrlSlugFor(pk, curr) match {
            case s @ Some(_) => s
            case None        => if (curr.contains('.')) loop(curr.substring(0, curr.lastIndexOf('.'))) else None
          }

        loop(c).getOrElse(models.PurchaseOrImportSubCategoryType.pathFor(pk, c))
      }

      val codeToResolve = if (code == ConfigPurchaseOrImportMapping.NoneValue) answers.get(PurchaseSubTypePage).getOrElse(code) else code
      val slug = findSlug(parentKey, codeToResolve)
      val msgKey = s"purchase.subCategory.$slug"
      val keyLabel = if (messages.isDefinedAt(msgKey)) messages(msgKey) else slug.replace('-', ' ').capitalize
      val displayValue = if (label == ConfigPurchaseOrImportMapping.NoneValue) messages("site.none") else label
      val mount = MountPrefix.getFromRequest
      val url = if (mount.isEmpty) s"/change-$slug" else s"$mount/change-$slug"

      (keyLabel, Some(displayValue), Seq((url, "site.change", "purchase.subCategory.change.hidden")))
    }

  def rowInvoiceType(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(InvoiceTypePage).map { it =>
      val url = routes.InvoiceTypeController.onPageLoad(CheckMode).url
      val parts = it.toString.split("\\s+").toSeq.filter(_.nonEmpty)
      val keySuffix = parts.headOption
        .map { first =>
          first + parts.drop(1).map(_.capitalize).mkString("")
        }
        .getOrElse(it.toString)

      val display = if (messages.isDefinedAt(s"invoiceType.$keySuffix")) {
        messages(s"invoiceType.$keySuffix")
      } else {
        parts.map(_.capitalize).mkString(" ")
      }

      (messages("invoiceType.checkYourAnswersLabel"), Some(display), Seq((url, "site.change", "invoiceType.change.hidden")))
    }

  def rowInvoiceNumber(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(InvoiceNumberPage).map { num =>
      val url = routes.InvoiceNumberController.onPageLoad(CheckMode).url
      (messages("invoiceNumber.checkYourAnswersLabel"), Some(num), Seq((url, "site.change", "invoiceNumber.change.hidden")))
    }

  def rowInvoiceDate(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(InvoiceDatePage).map { date =>
      val url = routes.InvoiceDateController.onPageLoad(CheckMode).url
      implicit val lang: Lang = messages.lang
      (messages("invoiceDate.checkYourAnswersLabel"),
       Some(date.format(utils.DateTimeFormats.dateTimeFormat())),
       Seq((url, "site.change", "invoiceDate.change.hidden"))
      )
    }

  def rowDescribeItems(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(DescribeItemsOnInvoicePage).map { desc =>
      val url = routes.DescribeItemsOnInvoiceController.onPageLoad(CheckMode).url
      val display = if (desc == null || desc.trim.isEmpty) messages("site.notProvided") else desc
      (messages("describeItemsOnInvoice.checkYourAnswersLabel"), Some(display), Seq((url, "site.change", "describeItemsOnInvoice.change.hidden")))
    }

  def rowSupplierName(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(SuppliersNamePage).map { name =>
      val url = routes.SuppliersNameController.onPageLoad(CheckMode).url
      (messages("suppliersName.checkYourAnswersLabel"), Some(name), Seq((url, "site.change", "suppliersName.change.hidden")))
    }

  def rowSupplierAddress(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(SupplierAddressPage).map { addr =>
      val url = routes.SupplierAddressController.onPageLoad(CheckMode).url
      val lines = Seq(Some(addr.line1), addr.line2, addr.line3).flatten.mkString("<br>")
      (messages("supplierAddress.checkYourAnswersLabel"), Some(lines), Seq((url, "site.change", "supplierAddress.change.hidden")))
    }

  def rowSupplierVatRegCheck(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(SimplifiedInvoiceVatRegCheckPage).map { v =>
      val url = routes.SimplifiedInvoiceVatRegCheckController.onPageLoad(CheckMode).url
      (messages("simplifiedInvoiceVatRegCheck.checkYourAnswersLabel"),
       Some(if (v) messages("site.yes") else messages("site.no")),
       Seq((url, "site.change", "simplifiedInvoiceVatRegCheck.change.hidden"))
      )
    }

  def rowSupplierVatRegNumber(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(SupplierVatRegistrationNumberPage).map { num =>
      val url = routes.SupplierVatRegistrationNumberController.onPageLoad(CheckMode).url
      (messages("supplierVatRegistrationNumber.checkYourAnswersLabel"),
       Some(num),
       Seq((url, "site.change", "supplierVatRegistrationNumber.change.hidden"))
      )
    }

  def rowSupplierTaxIdentifierNumber(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers.get(SupplierTaxIdentifierNumberPage).map { num =>
      val url = routes.SupplierTaxIdentifierNumberController.onPageLoad(CheckMode).url
      (messages("supplierTaxIdentifierNumber.checkYourAnswersLabel"),
       Some(num),
       Seq((url, "site.change", "supplierTaxIdentifierNumber.change.hidden"))
      )
    }

  def rowCurrency(displayName: Option[String])(implicit messages: Messages): Option[Row] =
    displayName.map { name =>
      val url = routes.RefundingCurrencyController.onPageLoad(CheckMode).url
      (messages("checkYourPurchaseDetails.refundingCurrency.label"),
       Some(name),
       Seq((url, "site.change", "checkYourPurchaseDetails.refundingCurrency.change.hidden"))
      )
    }

  def rowAmountBeforeVat(answers: UserAnswers, maybeSymbol: Option[String])(implicit messages: Messages): Option[Row] =
    answers.get(TotalPurchaseAmountBeforeVatPage).map { amt =>
      val url = routes.TotalPurchaseAmountBeforeVatController.onPageLoad(CheckMode).url
      val formattedNumber = f"$amt%,1.2f".replace(".00", "")
      val display = maybeSymbol.map(_ + formattedNumber).getOrElse(formattedNumber)
      (messages("totalPurchaseAmountBeforeVat.checkYourAnswersLabel"),
       Some(display),
       Seq((url, "site.change", "totalPurchaseAmountBeforeVat.change.hidden"))
      )
    }

  def rowVatPaid(answers: UserAnswers, maybeSymbol: Option[String])(implicit messages: Messages): Option[Row] =
    answers.get(pages.TotalVatPaidPage).map { amt =>
      val url = routes.TotalVatPaidController.onPageLoad(CheckMode).url
      val formattedNumber = f"$amt%,1.2f".replace(".00", "")
      val display = maybeSymbol.map(_ + formattedNumber).getOrElse(formattedNumber)
      (messages("totalVatPaid.checkYourAnswersLabel"), Some(display), Seq((url, "site.change", "totalVatPaid.change.hidden")))
    }

  def rowVatClaim(answers: UserAnswers, maybeSymbol: Option[String])(implicit messages: Messages): Option[Row] =
    answers.get(TotalVatClaimPage).map { amt =>
      val url = routes.TotalVatClaimController.onPageLoad(CheckMode).url
      val formattedNumber = f"$amt%,1.2f".replace(".00", "")
      val display = maybeSymbol.map(_ + formattedNumber).getOrElse(formattedNumber)
      (messages("totalVatClaim.checkYourAnswersLabel"), Some(display), Seq((url, "site.change", "totalVatClaim.change.hidden")))
    }

  def rowSupplierTaxNumbers(answers: UserAnswers)(implicit messages: Messages): Option[Row] =
    answers
      .get(SupplierVatRegistrationNumberPage)
      .map { _num =>
        val url = routes.SupplierTaxNumberController.onPageLoad(CheckMode).url
        (messages("supplierTaxNumber.checkYourAnswersLabel"),
         Some(messages("supplierVatRegistrationNumber.checkYourAnswersLabel")),
         Seq((url, "site.change", "supplierVatRegistrationNumber.change.hidden"))
        )
      }
      .orElse(
        answers.get(SupplierTaxIdentifierNumberPage).map { _num =>
          val url = routes.SupplierTaxNumberController.onPageLoad(CheckMode).url
          (messages("supplierTaxNumber.checkYourAnswersLabel"),
           Some(messages("supplierTaxIdentifierNumber.checkYourAnswersLabel")),
           Seq((url, "site.change", "supplierTaxIdentifierNumber.change.hidden"))
          )
        }
      )
      .orElse(
        answers.get(SupplierTaxNumberPage) match {
          case Some(models.SupplierTaxNumber.Neither) =>
            Some(
              (messages("supplierTaxNumber.checkYourAnswersLabel"),
               Some(messages("site.notProvided")),
               Seq((routes.SupplierTaxNumberController.onPageLoad(CheckMode).url, "site.change", "supplierTaxNumber.change.hidden"))
              )
            )
          case _ => None
        }
      )

  def sections(answers: UserAnswers,
               maybeCurrencyDisplayName: Option[String],
               maybeCurrencySymbol: Option[String],
               config: ConfigPurchaseOrImportMapping,
               showCurrencyRow: Boolean
              )(implicit messages: Messages, request: RequestHeader): Seq[(String, Seq[Row])] = {
    val purchaseCategoryRows =
      Seq(rowPurchaseType(answers), rowPurchaseSubTypeLabel(answers, config), rowPurchaseSubCategoryLabel(answers), rowDescribeItems(answers)).flatten

    val invoiceRows = Seq(rowInvoiceType(answers), rowInvoiceNumber(answers), rowInvoiceDate(answers)).flatten
    val isGermany = answers.get(RefundingCountryPage).contains("DE")

    val supplierRows = (
      Seq(rowSupplierName(answers), rowSupplierAddress(answers)) ++
        (if (isGermany)
           Seq(rowSupplierTaxNumbers(answers), rowSupplierVatRegNumber(answers), rowSupplierTaxIdentifierNumber(answers))
         else Seq(rowSupplierVatRegCheck(answers), rowSupplierVatRegNumber(answers)))
    ).flatten

    val amountsRows = Seq(
      if (showCurrencyRow) rowCurrency(maybeCurrencyDisplayName) else None,
      rowAmountBeforeVat(answers, maybeCurrencySymbol),
      rowVatPaid(answers, maybeCurrencySymbol),
      rowVatClaim(answers, maybeCurrencySymbol)
    ).flatten

    Seq(
      ("purchase.checkYourPurchase.purchaseCategory", purchaseCategoryRows),
      ("purchase.checkYourPurchase.invoiceDetails", invoiceRows),
      ("purchase.checkYourPurchase.supplierDetails", supplierRows),
      ("purchase.checkYourPurchase.purchaseAmounts", amountsRows)
    )
  }

}
