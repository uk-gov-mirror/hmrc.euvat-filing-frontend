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

package navigation

import base.SpecBase
import com.typesafe.config.ConfigFactory
import controllers.claim.routes as claimRoutes
import controllers.purchase.routes as purchaseRoutes
import models.*
import models.PurchaseOrImport.{Import, Purchase}
import pages.*
import play.api.Configuration
import play.api.mvc.Call
import utils.{ConfigLanguageMapping, ConfigPurchaseOrImportMapping, CurrencyConfig}

class NavigatorSpec extends SpecBase {

  val navigator = new Navigator(
    new CurrencyConfig(
      Configuration(
        ConfigFactory.parseString("""
          currency.mapping {
            BG = ["bulgarianLev|BGN|лв"]
            EE = ["euro|EUR|€", "estonianKroon|EEK|kr"]
            AT = ["euro|EUR|€"]
          }
        """)
      )
    ),
    new ConfigLanguageMapping(
      Configuration(
        ConfigFactory.parseString("""
          language.mapping = {
            AT = ["german", "english"]
            BE = ["english", "german", "french", "dutch"]
            CZ = ["czech"]
          }
        """)
      )
    ),
    new utils.ConfigPurchaseOrImportMapping()
  )
  val userAnswers: UserAnswers = UserAnswers("id")

  "Navigator" - {

    "in Normal mode" - {
      "must go from a page that doesn't exist in the route map to Index" in {
        case object UnknownPage extends Page
        navigator.nextPage(UnknownPage, NormalMode, userAnswers) mustBe controllers.routes.IndexController.onPageLoad()
      }

      "must go from RefundingCountryPage to RefundingLanguageController" in {
        navigator.nextPage(pages.RefundingCountryPage, NormalMode, userAnswers) mustBe
          claimRoutes.RefundingLanguageController.onPageLoad(NormalMode)
      }

      "must go from RefundingLanguagePage to JourneyRecoveryController if country is missing" in {
        navigator.nextPage(pages.RefundingLanguagePage, NormalMode, userAnswers) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from RefundingCurrencyPage to TotalPurchaseAmountBeforeVatController" in {
        navigator.nextPage(pages.RefundingCurrencyPage, NormalMode, userAnswers) mustBe
          purchaseRoutes.TotalPurchaseAmountBeforeVatController.onPageLoad(NormalMode)
      }

      "must go from RefundPeriodPage to ContactDetailsController" in {
        navigator.nextPage(pages.RefundPeriodPage, NormalMode, userAnswers) mustBe
          claimRoutes.ContactDetailsController.onPageLoad(NormalMode)
      }

      "must go from ContactDetailsPage to BusinessActivityController" in {
        navigator.nextPage(ContactDetailsPage, NormalMode, userAnswers) mustBe
          claimRoutes.BusinessActivityController.onPageLoad(NormalMode)
      }

      "must go from BusinessActivityPage to BusinessActivityCodeTwoController if yes selected" in {
        val ua = userAnswers.set(BusinessActivityPage, true).success.value
        navigator.nextPage(BusinessActivityPage, NormalMode, ua) mustBe
          claimRoutes.BusinessActivityCodeTwoController.onPageLoad(NormalMode)
      }

      "must go from BusinessActivityPage to CheckYourClaimDetailsPage if no selected" in {
        val ua = userAnswers.set(BusinessActivityPage, false).success.value
        navigator.nextPage(BusinessActivityPage, NormalMode, ua) mustBe
          claimRoutes.CheckYourClaimDetailsController.onPageLoad()
      }

      "must go from BusinessActivityTwoPage to BusinessActivityCodeThreeController if yes selected" in {
        val ua = userAnswers.set(BusinessActivityTwoPage, true).success.value
        navigator.nextPage(BusinessActivityTwoPage, NormalMode, ua) mustBe
          claimRoutes.BusinessActivityCodeThreeController.onPageLoad(NormalMode)
      }

      "must go from InvoiceNumberPage to InvoiceDateController in normal flow (no warning marker)" in {
        val ua = userAnswers.remove(SupplierVatRegistrationWarningPage).success.value
        navigator.nextPage(InvoiceNumberPage, NormalMode, ua) mustBe
          purchaseRoutes.InvoiceDateController.onPageLoad(NormalMode)
      }

      "must go from BusinessActivityTwoPage to CheckYourClaimDetailsPage if no selected" in {
        val ua = userAnswers.set(BusinessActivityTwoPage, false).success.value
        navigator.nextPage(BusinessActivityTwoPage, NormalMode, ua) mustBe
          claimRoutes.CheckYourClaimDetailsController.onPageLoad()
      }

      "must go from BusinessActivityCodeThreePage to BusinessActivityThreeController" in {
        navigator.nextPage(BusinessActivityCodeThreePage, NormalMode, userAnswers) mustBe
          claimRoutes.BusinessActivityThreeController.onPageLoad()
      }

      "must go from PurchaseOrImportPage to PurchaseTypeController when Purchase is selected" in {
        val answers = userAnswers.set(PurchaseOrImportPage, Purchase).success.value

        navigator.nextPage(PurchaseOrImportPage, NormalMode, answers) mustBe
          purchaseRoutes.PurchaseTypeController.onPageLoad(NormalMode)
      }

      "must go from PurchaseOrImportPage to ImportTypeController when Import is selected" in {
        val answers = userAnswers.set(PurchaseOrImportPage, Import).success.value

        navigator.nextPage(PurchaseOrImportPage, NormalMode, answers) mustBe
          controllers.imports.routes.ImportTypeController.onPageLoad(NormalMode)
      }

      "must go from ImportTypePage to JourneyRecovery when ImportType present but no country" in {
        val ua = userAnswers.set(ImportTypePage, Fuel).success.value
        navigator.nextPage(ImportTypePage, NormalMode, ua) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from PurchaseOrImportPage to ImportTypeController when Import selected" in {
        val ua = userAnswers.set(PurchaseOrImportPage, PurchaseOrImport.Import).success.value
        navigator.nextPage(PurchaseOrImportPage, NormalMode, ua) mustBe
          controllers.imports.routes.ImportTypeController.onPageLoad(NormalMode)
      }

      "must go from ImportTypePage to the import sub code page for that type when the country has sub codes" in {
        val fakePurchaseConfig = new utils.ConfigPurchaseOrImportMapping() {
          override def subcodesFor(country: String, parentKey: String): Seq[(String, String)] =
            if (country == "BG" && parentKey == Fuel.toString) Seq(("1.1", "purchase.sub.fuel.1.1"), ("1.1.1", "purchase.sub.fuel.1.1.1"))
            else Seq.empty
        }
        val nav = new Navigator(
          new CurrencyConfig(Configuration(ConfigFactory.parseString("""currency.mapping = {}"""))),
          new ConfigLanguageMapping(Configuration(ConfigFactory.parseString("""language.mapping = {}"""))),
          fakePurchaseConfig
        )
        val ua = userAnswers.set(pages.RefundingCountryPage, "BG").success.value.set(ImportTypePage, Fuel).success.value

        nav.nextPage(ImportTypePage, NormalMode, ua) mustBe
          controllers.imports.routes.ImportSubCodeController.onPageLoad(Fuel.toString)
      }

      "must go from ImportTypePage to TaskListDashboardController when the country has no sub codes for that type" in {
        val fakePurchaseConfig = new utils.ConfigPurchaseOrImportMapping() {
          override def subcodesFor(country: String, parentKey: String): Seq[(String, String)] = Seq.empty
        }
        val nav = new Navigator(
          new CurrencyConfig(Configuration(ConfigFactory.parseString("""currency.mapping = {}"""))),
          new ConfigLanguageMapping(Configuration(ConfigFactory.parseString("""language.mapping = {}"""))),
          fakePurchaseConfig
        )
        val ua = userAnswers.set(pages.RefundingCountryPage, "AT").success.value.set(ImportTypePage, Transport).success.value

        nav.nextPage(ImportTypePage, NormalMode, ua) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from ImportTypePage to TaskListDashboardController when the only sub code is 10.99" in {
        val fakePurchaseConfig = new utils.ConfigPurchaseOrImportMapping() {
          override def subcodesFor(country: String, parentKey: String): Seq[(String, String)] =
            if (parentKey == Other.toString) Seq(("10.99", "purchase.sub.other.10.99")) else Seq.empty
        }
        val nav = new Navigator(
          new CurrencyConfig(Configuration(ConfigFactory.parseString("""currency.mapping = {}"""))),
          new ConfigLanguageMapping(Configuration(ConfigFactory.parseString("""language.mapping = {}"""))),
          fakePurchaseConfig
        )
        val ua = userAnswers.set(pages.RefundingCountryPage, "AT").success.value.set(ImportTypePage, Other).success.value

        nav.nextPage(ImportTypePage, NormalMode, ua) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from PurchaseOrImportPage to SAD reference page when Import selected" in {
        val ua = userAnswers.set(PurchaseOrImportPage, PurchaseOrImport.Import).success.value
        navigator.nextPage(PurchaseOrImportPage, NormalMode, ua) mustBe
          controllers.imports.routes.SadReferenceController.onPageLoad
      }

        //TODO: replace with "When is the import date?" page controller once built
       "must go from ImportDetailsInfoPage to Journey Recovery in Normal Mode" in {
         navigator.nextPage(ImportDetailsInfoPage, NormalMode, emptyUserAnswers) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
       }

        //TODO: replace with imports CYA controller once built
       "must go from ImportDetailsInfoPage to Journey Recovery in Check Mode" in {
         navigator.nextPage(ImportDetailsInfoPage, CheckMode, emptyUserAnswers) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
       }

      "must go from PurchaseTypePage to DescribeItemsOnInvoiceController" in {
        val ua = userAnswers.set(PurchaseTypePage, PurchaseOrImportType.values.head).success.value
        navigator.nextPage(PurchaseTypePage, NormalMode, ua) mustBe
          purchaseRoutes.DescribeItemsOnInvoiceController.onPageLoad(NormalMode)
      }

      "must go from PurchaseTypePage to JourneyRecoveryController if no answer is present" in {
        navigator.nextPage(PurchaseTypePage, NormalMode, userAnswers) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from PurchaseTypePage to PurchaseSubTypeController when mapping exists for country" in {
        val fakePurchaseConfig = new utils.ConfigPurchaseOrImportMapping() {
          override def subcodesFor(country: String, parentKey: String): Seq[(String, String)] =
            if (country == "AT" && parentKey == Fuel.toString) Seq(("1", "purchase.sub.fuel.1")) else Seq.empty
        }

        val nav = new Navigator(
          new CurrencyConfig(Configuration(ConfigFactory.parseString("""currency.mapping = {}"""))),
          new ConfigLanguageMapping(Configuration(ConfigFactory.parseString("""language.mapping = {}"""))),
          fakePurchaseConfig
        )

        val ua = userAnswers.set(pages.RefundingCountryPage, "AT").success.value.set(PurchaseTypePage, Fuel).success.value

        nav.nextPage(PurchaseTypePage, NormalMode, ua) mustBe
          play.api.mvc.Call("GET", s"/${PurchaseOrImportType.urlSlugForPurchaseType(Fuel)}")
      }

      "must go from PurchaseTypePage to InvoiceTypeController when mapping is empty for country" in {
        val fakePurchaseConfig: ConfigPurchaseOrImportMapping = new utils.ConfigPurchaseOrImportMapping() {
          override def subcodesFor(country: String, parentKey: String): Seq[Nothing] = Seq.empty
        }

        val nav = new Navigator(
          new CurrencyConfig(Configuration(ConfigFactory.parseString("""currency.mapping = {}"""))),
          new ConfigLanguageMapping(Configuration(ConfigFactory.parseString("""language.mapping = {}"""))),
          fakePurchaseConfig
        )

        val ua = userAnswers.set(pages.RefundingCountryPage, "AT").success.value.set(PurchaseTypePage, Fuel).success.value

        nav.nextPage(PurchaseTypePage, NormalMode, ua) mustBe
          purchaseRoutes.InvoiceTypeController.onPageLoad(NormalMode)
      }

      "must go from PurchaseTypePage to JourneyRecoveryController when country code stored as name+code string is used" in {
        val fakePurchaseConfig = new utils.ConfigPurchaseOrImportMapping() {
          override def subcodesFor(country: String, parentKey: String): Seq[(String, String)] = Seq(("1", "purchase.sub.fuel.1"))
        }

        val nav = new Navigator(
          new CurrencyConfig(Configuration(ConfigFactory.parseString("""currency.mapping = {}"""))),
          new ConfigLanguageMapping(Configuration(ConfigFactory.parseString("""language.mapping = {}"""))),
          fakePurchaseConfig
        )

        val ua = userAnswers.set(pages.RefundingCountryNamePage, "Austria,AT").success.value.set(PurchaseTypePage, Fuel).success.value

        nav.nextPage(PurchaseTypePage, NormalMode, ua) mustBe
          play.api.mvc.Call("GET", s"/${PurchaseOrImportType.urlSlugForPurchaseType(Fuel)}")
      }

      "must go from PurchaseTypePage to PurchaseSubTypeController when country stored as name-only string is used" in {
        val fakePurchaseConfig = new utils.ConfigPurchaseOrImportMapping() {
          override def subcodesFor(country: String, parentKey: String): Seq[(String, String)] =
            if (country == "Austria" && parentKey == Fuel.toString) Seq(("1", "purchase.sub.fuel.1")) else Seq.empty
        }

        val nav = new Navigator(
          new CurrencyConfig(Configuration(ConfigFactory.parseString("""currency.mapping = {}"""))),
          new ConfigLanguageMapping(Configuration(ConfigFactory.parseString("""language.mapping = {}"""))),
          fakePurchaseConfig
        )

        val ua = userAnswers.set(pages.RefundingCountryNamePage, "Austria").success.value.set(PurchaseTypePage, Fuel).success.value

        nav.nextPage(PurchaseTypePage, NormalMode, ua) mustBe
          play.api.mvc.Call("GET", s"/${PurchaseOrImportType.urlSlugForPurchaseType(Fuel)}")
      }

      "must go from PurchaseTypePage to InvoiceTypeController when country stored as name-only and mapping empty" in {
        val fakePurchaseConfig: ConfigPurchaseOrImportMapping = new utils.ConfigPurchaseOrImportMapping() {
          override def subcodesFor(country: String, parentKey: String): Seq[Nothing] = Seq.empty
        }

        val nav = new Navigator(
          new CurrencyConfig(Configuration(ConfigFactory.parseString("""currency.mapping = {}"""))),
          new ConfigLanguageMapping(Configuration(ConfigFactory.parseString("""language.mapping = {}"""))),
          fakePurchaseConfig
        )

        val ua = userAnswers.set(pages.RefundingCountryNamePage, "Austria").success.value.set(PurchaseTypePage, Fuel).success.value

        nav.nextPage(PurchaseTypePage, NormalMode, ua) mustBe
          purchaseRoutes.InvoiceTypeController.onPageLoad(NormalMode)
      }

      "must go from PurchaseSubCategoryPage to InvoiceTypeController when PurchaseType is Other and subcategory ends with 99" in {
        val ua = userAnswers.set(PurchaseTypePage, Other).success.value.set(PurchaseSubCategoryPage, "1.99").success.value
        navigator.nextPage(PurchaseSubCategoryPage, NormalMode, ua) mustBe
          purchaseRoutes.InvoiceTypeController.onPageLoad(NormalMode)
      }

      "must go from PurchaseSubCategoryPage to InvoiceTypeController when PurchaseType is not Other" in {
        val ua = userAnswers.set(PurchaseTypePage, Fuel).success.value.set(PurchaseSubCategoryPage, "1").success.value
        navigator.nextPage(PurchaseSubCategoryPage, NormalMode, ua) mustBe
          purchaseRoutes.InvoiceTypeController.onPageLoad(NormalMode)
      }

      "must go from DescribeItemsOnInvoicePage to InvoiceTypeController" in {
        navigator.nextPage(DescribeItemsOnInvoicePage, NormalMode, userAnswers) mustBe
          purchaseRoutes.InvoiceTypeController.onPageLoad(NormalMode)
      }

      "must go from InvoiceNumberPage to InvoiceDateController" in {
        navigator.nextPage(InvoiceNumberPage, NormalMode, userAnswers) mustBe
          purchaseRoutes.InvoiceDateController.onPageLoad(NormalMode)
      }

      "must go from InvoiceDatePage to SuppliersNameController" in {
        navigator.nextPage(InvoiceDatePage, NormalMode, userAnswers) mustBe
          purchaseRoutes.SuppliersNameController.onPageLoad(NormalMode)
      }

      "must go from SuppliersNamePage to SupplierAddressController" in {
        navigator.nextPage(SuppliersNamePage, NormalMode, userAnswers) mustBe purchaseRoutes.SupplierAddressController.onPageLoad(NormalMode)
      }

      "must go from SupplierAddressPage to SupplierTaxNumberController if country is Germany" in {
        val ua = userAnswers.set(RefundingCountryPage, "DE").success.value
        navigator.nextPage(SupplierAddressPage, NormalMode, ua) mustBe purchaseRoutes.SupplierTaxNumberController.onPageLoad(NormalMode)
      }

      "must go from SupplierAddressPage to SimplifiedInvoiceVatRegCheckController if country is not Germany and invoice type is simplified" in {
        val ua = userAnswers.set(RefundingCountryPage, "FR").success.value.set(InvoiceTypePage, InvoiceType.SimplifiedInvoice).success.value
        navigator.nextPage(SupplierAddressPage, NormalMode, ua) mustBe purchaseRoutes.SimplifiedInvoiceVatRegCheckController.onPageLoad(NormalMode)
      }

      "must go from SupplierAddressPage to SupplierVatRegistrationNumberController if country is not Germany and invoice type is standard" in {
        val ua = userAnswers.set(RefundingCountryPage, "FR").success.value.set(InvoiceTypePage, InvoiceType.StandardInvoice).success.value
        navigator.nextPage(SupplierAddressPage, NormalMode, ua) mustBe purchaseRoutes.SupplierVatRegistrationNumberController.onPageLoad(NormalMode)
      }

      "must go from SupplierAddressPage to SupplierVatRegistrationNumberController if country is not Germany and standard invoice" in {
        val ua = userAnswers
          .set(RefundingCountryPage, "FR")
          .success
          .value
          .set(InvoiceTypePage, InvoiceType.StandardInvoice)
          .success
          .value
        navigator.nextPage(SupplierAddressPage, NormalMode, ua) mustBe purchaseRoutes.SupplierVatRegistrationNumberController.onPageLoad(NormalMode)
      }

      "must go from SupplierAddressPage to SimplifiedInvoiceVatRegCheckController if country is missing and no invoice type" in {
        navigator.nextPage(SupplierAddressPage, NormalMode, userAnswers) mustBe
          purchaseRoutes.SimplifiedInvoiceVatRegCheckController.onPageLoad(NormalMode)
      }

      "must go from SupplierTaxNumberPage to SupplierVatRegistrationController if VAT registration number is selected" in {
        val ua = userAnswers.set(SupplierTaxNumberPage, SupplierTaxNumber.Vatregistrationnumber).success.value
        navigator.nextPage(SupplierTaxNumberPage, NormalMode, ua) mustBe
          purchaseRoutes.SupplierVatRegistrationNumberController.onPageLoad(NormalMode)
      }

      "must go from SupplierTaxNumberPage to SupplierTaxIdentifierNumberController if tax identifier number is selected" in {
        val ua = userAnswers.set(SupplierTaxNumberPage, SupplierTaxNumber.Taxidentifiernumber).success.value
        navigator.nextPage(SupplierTaxNumberPage, NormalMode, ua) mustBe purchaseRoutes.SupplierTaxIdentifierNumberController.onPageLoad(NormalMode)
      }

      "must go from SupplierTaxIdentifierNumberPage to TotalPurchaseAmountBeforeVatController in NormalMode" in {
        navigator.nextPage(SupplierTaxIdentifierNumberPage, NormalMode, userAnswers) mustBe
          purchaseRoutes.TotalPurchaseAmountBeforeVatController.onPageLoad(NormalMode)
      }

      "must go from SupplierTaxIdentifierNumberPage to CheckYourPurchaseDetailsController in CheckMode" in {
        navigator.nextPage(SupplierTaxIdentifierNumberPage, CheckMode, userAnswers) mustBe
          purchaseRoutes.CheckYourPurchaseDetailsController.onPageLoad()
      }

      "must go from SupplierTaxNumberPage to TotalPurchaseAmountBeforeVatController if neither is selected" in {
        val ua = userAnswers
          .set(SupplierTaxNumberPage, SupplierTaxNumber.Neither)
          .success
          .value
          .set(pages.RefundingCountryPage, "AT")
          .success
          .value

        navigator.nextPage(SupplierTaxNumberPage, NormalMode, ua) mustBe
          purchaseRoutes.TotalPurchaseAmountBeforeVatController.onPageLoad(NormalMode)
      }

      "must go from SupplierTaxNumberPage to RefundingCurrencyController if neither is selected and the country has more than one currency" in {
        val ua = userAnswers
          .set(SupplierTaxNumberPage, SupplierTaxNumber.Neither)
          .success
          .value
          .set(pages.RefundingCountryPage, "EE")
          .success
          .value

        navigator.nextPage(SupplierTaxNumberPage, NormalMode, ua) mustBe
          purchaseRoutes.RefundingCurrencyController.onPageLoad(NormalMode)
      }

      "must go from SupplierTaxNumberPage to JourneyRecoveryController if no answer is present" in {
        navigator.nextPage(SupplierTaxNumberPage, NormalMode, userAnswers) mustBe controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from SupplierVatRegistrationNumberPage to TotalPurchaseAmountBeforeVatController" in {
        val ua = userAnswers.set(pages.RefundingCountryPage, "AT").success.value
        navigator.nextPage(SupplierVatRegistrationNumberPage, NormalMode, ua) mustBe
          purchaseRoutes.TotalPurchaseAmountBeforeVatController.onPageLoad(NormalMode)
      }

      "must go from SupplierVatRegistrationNumberPage to RefundingCurrencyController when country has more than one currency" in {
        val ua = userAnswers.set(pages.RefundingCountryPage, "EE").success.value
        navigator.nextPage(SupplierVatRegistrationNumberPage, NormalMode, ua) mustBe
          purchaseRoutes.RefundingCurrencyController.onPageLoad(NormalMode)
      }

      "must go from SimplifiedInvoiceVatRegCheckPage to RefundingCurrencyController if no selected and the country has more than one currency" in {
        val ua = userAnswers
          .set(pages.SimplifiedInvoiceVatRegCheckPage, false)
          .success
          .value
          .set(pages.RefundingCountryPage, "EE")
          .success
          .value
        navigator.nextPage(SimplifiedInvoiceVatRegCheckPage, NormalMode, ua) mustBe
          purchaseRoutes.RefundingCurrencyController.onPageLoad(NormalMode)
      }

      "must go from TotalPurchaseAmountBeforeVatPage to TotalVatPaidController" in {
        navigator.nextPage(TotalPurchaseAmountBeforeVatPage, NormalMode, userAnswers) mustBe
          purchaseRoutes.TotalVatPaidController.onPageLoad(NormalMode)
      }

      "must go from TotalVatPaidPage to TotalVatClaimController" in {
        navigator.nextPage(TotalVatPaidPage, NormalMode, userAnswers) mustBe
          purchaseRoutes.TotalVatClaimController.onPageLoad(NormalMode)
      }

      "must go from TotalVatClaimPage to CheckYourPurchaseDetailsController" in {
        navigator.nextPage(TotalVatClaimPage, NormalMode, userAnswers) mustBe
          purchaseRoutes.CheckYourPurchaseDetailsController.onPageLoad()
      }

      "must go from SimplifiedInvoiceVatRegCheckPage to TotalPurchaseAmountBeforeVatController if no selected" in {
        val ua = userAnswers
          .set(SimplifiedInvoiceVatRegCheckPage, false)
          .success
          .value
          .set(pages.RefundingCountryPage, "AT")
          .success
          .value
        navigator.nextPage(SimplifiedInvoiceVatRegCheckPage, NormalMode, ua) mustBe
          purchaseRoutes.TotalPurchaseAmountBeforeVatController.onPageLoad(NormalMode)
      }

      "must go from SimplifiedInvoiceVatRegCheckPage to SupplierVatRegistrationNumberController if yes selected and invoice type is simplified" in {
        val ua =
          userAnswers.set(SimplifiedInvoiceVatRegCheckPage, true).success.value.set(InvoiceTypePage, InvoiceType.SimplifiedInvoice).success.value
        navigator.nextPage(SimplifiedInvoiceVatRegCheckPage, NormalMode, ua) mustBe
          purchaseRoutes.SupplierVatRegistrationNumberController.onPageLoad(NormalMode)
      }

      "must go from CheckYourStateDetailsPage to CheckYourClaimDetailsController if no selected" in {
        val ua = userAnswers.set(CheckYourStateDetailsPage, false).success.value
        navigator.nextPage(CheckYourStateDetailsPage, NormalMode, ua) mustBe
          claimRoutes.CheckYourClaimDetailsController.onPageLoad()
      }

      "must go from CheckYourStateDetailsPage to JourneyRecoveryController if yes selected" in {
        val ua = userAnswers.set(CheckYourStateDetailsPage, true).success.value
        navigator.nextPage(CheckYourStateDetailsPage, NormalMode, ua) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from CheckYourStateDetailsPage to JourneyRecoveryController if no answer is given" in {
        navigator.nextPage(CheckYourStateDetailsPage, NormalMode, userAnswers) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }
    }

    "in Check mode" - {
      "must go from a page that doesn't exist in the edit route map to IndexController" in {
        case object UnknownPage extends Page
        navigator.nextPage(UnknownPage, CheckMode, userAnswers) mustBe controllers.routes.IndexController.onPageLoad()
      }

      "must go from RefundingCountryPage to RefundingLanguageController" in {
        navigator.nextPage(pages.RefundingCountryPage, CheckMode, userAnswers) mustBe
          claimRoutes.RefundingLanguageController.onPageLoad(CheckMode)
      }

      "must go from RefundingLanguagePage to CheckYourClaimDetailsController if country has one currency" in {
        val ua = userAnswers.set(pages.RefundingCountryPage, "AT").success.value
        navigator.nextPage(pages.RefundingLanguagePage, CheckMode, ua) mustBe
          claimRoutes.CheckYourClaimDetailsController.onPageLoad()
      }

      "must go from RefundPeriodPage to CheckYourClaimDetailsController" in {
        navigator.nextPage(pages.RefundPeriodPage, CheckMode, userAnswers) mustBe
          claimRoutes.CheckYourClaimDetailsController.onPageLoad()
      }

      "must go from RefundingCurrencyPage to RefundPeriodController in CheckMode if CountryChangedPage is true" in {
        val ua = userAnswers.set(pages.CountryChangedPage, true).success.value
        navigator.nextPage(pages.RefundingCurrencyPage, CheckMode, ua) mustBe
          purchaseRoutes.CheckYourPurchaseDetailsController.onPageLoad()
      }

      "must go from RefundingCurrencyPage to CheckYourPurchaseDetailsController in CheckMode if CountryChangedPage is not set" in {
        navigator.nextPage(pages.RefundingCurrencyPage, CheckMode, userAnswers) mustBe
          purchaseRoutes.CheckYourPurchaseDetailsController.onPageLoad()
      }

      "must go from RefundingCurrencyPage to TotalPurchaseAmountBeforeVatController in CheckMode when country is EE and currency changed" in {
        val ua = userAnswers
          .set(pages.RefundingCountryPage, "EE")
          .success
          .value
          .set(pages.CurrencyChangedPage, true)
          .success
          .value

        navigator.nextPage(pages.RefundingCurrencyPage, CheckMode, ua) mustBe
          purchaseRoutes.TotalPurchaseAmountBeforeVatController.onPageLoad(CheckMode)
      }

      "must go from RefundingLanguagePage to RefundPeriodController in CheckMode if country has one currency and CountryChangedPage is true" in {
        val ua = userAnswers
          .set(pages.RefundingCountryPage, "AT")
          .success
          .value
          .set(pages.CountryChangedPage, true)
          .success
          .value
        navigator.nextPage(pages.RefundingLanguagePage, CheckMode, ua) mustBe
          claimRoutes.RefundPeriodController.onPageLoad(CheckMode)
      }

      "must go from ContactDetailsPage to CheckYourClaimDetailsController" in {
        navigator.nextPage(ContactDetailsPage, CheckMode, userAnswers) mustBe
          claimRoutes.CheckYourClaimDetailsController.onPageLoad()
      }

      "must go from BusinessActivityPage to BusinessActivityCodeTwoController if yes selected" in {
        val ua = userAnswers.set(BusinessActivityPage, true).success.value
        navigator.nextPage(BusinessActivityPage, CheckMode, ua) mustBe
          claimRoutes.BusinessActivityCodeTwoController.onPageLoad(CheckMode)
      }

      "must go from BusinessActivityPage to CheckYourClaimDetailsPage if no selected" in {
        val ua = userAnswers.set(BusinessActivityPage, false).success.value
        navigator.nextPage(BusinessActivityPage, CheckMode, ua) mustBe
          claimRoutes.CheckYourClaimDetailsController.onPageLoad()
      }

      "must go from BusinessActivityTwoPage to BusinessActivityCodeThreeController if yes selected" in {
        val ua = userAnswers.set(BusinessActivityTwoPage, true).success.value
        navigator.nextPage(BusinessActivityTwoPage, CheckMode, ua) mustBe
          claimRoutes.BusinessActivityCodeThreeController.onPageLoad(CheckMode)
      }

      "must go from BusinessActivityTwoPage to CheckYourClaimDetailsPage if no selected" in {
        val ua = userAnswers.set(BusinessActivityTwoPage, false).success.value
        navigator.nextPage(BusinessActivityTwoPage, CheckMode, ua) mustBe
          claimRoutes.CheckYourClaimDetailsController.onPageLoad()
      }

      "must go from BusinessActivityCodeThreePage to BusinessActivityThreeController" in {
        navigator.nextPage(BusinessActivityCodeThreePage, CheckMode, userAnswers) mustBe
          claimRoutes.BusinessActivityThreeController.onPageLoad()
      }

      "must go from ImportTypePage to JourneyRecovery in CheckMode when ImportType present but no country" in {
        val ua = userAnswers.set(ImportTypePage, Fuel).success.value
        navigator.nextPage(ImportTypePage, CheckMode, ua) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from PurchaseTypePage to DescribeItemsOnInvoiceController" in {
        val ua = userAnswers.set(PurchaseTypePage, PurchaseOrImportType.values.head).success.value
        navigator.nextPage(PurchaseTypePage, CheckMode, ua) mustBe
          purchaseRoutes.DescribeItemsOnInvoiceController.onPageLoad(CheckMode)
      }

      "must go from PurchaseTypePage to JourneyRecoveryController if no answer is present" in {
        navigator.nextPage(PurchaseTypePage, CheckMode, userAnswers) mustBe
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from DescribeItemsOnInvoicePage to CheckYourPurchaseDetailsController in CheckMode" in {
        navigator.nextPage(DescribeItemsOnInvoicePage, CheckMode, userAnswers) mustBe
          purchaseRoutes.CheckYourPurchaseDetailsController.onPageLoad()
      }

      "must go from InvoiceNumberPage to CYA Purchase details in CheckMode" in {
        val ua = userAnswers.remove(SupplierVatRegistrationWarningPage).success.value
        navigator.nextPage(InvoiceNumberPage, CheckMode, ua) mustBe
          purchaseRoutes.CheckYourPurchaseDetailsController.onPageLoad()
      }

      "must go from InvoiceNumberPage to the supplier VRN number page in CheckMode when marker is true" in {
        val ua = userAnswers.set(SupplierVatRegistrationNumberPage, "123").success.value
        navigator.nextPage(InvoiceNumberPage, CheckMode, ua) mustBe
          purchaseRoutes.SupplierVatRegistrationNumberController.onPageLoad(CheckMode)
      }

      "must go from InvoiceNumberPage to CYA purchase page in CheckMode" in {
        navigator.nextPage(InvoiceNumberPage, CheckMode, userAnswers) mustBe
          purchaseRoutes.CheckYourPurchaseDetailsController.onPageLoad()
      }

      "must go from InvoiceDatePage to SuppliersNameController in CheckMode" in {
        navigator.nextPage(InvoiceDatePage, CheckMode, userAnswers) mustBe
          purchaseRoutes.SuppliersNameController.onPageLoad(CheckMode)
      }

      "must go from SuppliersNamePage to SupplierAddressController in CheckMode" in {
        navigator.nextPage(SuppliersNamePage, CheckMode, userAnswers) mustBe purchaseRoutes.SupplierAddressController.onPageLoad(CheckMode)
      }

      "must go from SupplierAddressPage to SupplierTaxNumberController in CheckMode if country is Germany" in {
        val ua = userAnswers.set(RefundingCountryPage, "DE").success.value
        navigator.nextPage(SupplierAddressPage, CheckMode, ua) mustBe purchaseRoutes.SupplierTaxNumberController.onPageLoad(CheckMode)
      }

      "must go from SupplierAddressPage to SimplifiedInvoiceVatRegCheckController in CheckMode if country is not Germany and simplified invoice" in {
        val ua = userAnswers
          .set(RefundingCountryPage, "AT")
          .success
          .value
          .set(InvoiceTypePage, InvoiceType.SimplifiedInvoice)
          .success
          .value
        navigator.nextPage(SupplierAddressPage, CheckMode, ua) mustBe purchaseRoutes.SimplifiedInvoiceVatRegCheckController.onPageLoad(CheckMode)
      }

      "must go from SupplierAddressPage to SupplierVatRegistrationNumberController in CheckMode when country is not DE and invoice type is standard" in {
        val ua = userAnswers.set(RefundingCountryPage, "FR").success.value.set(InvoiceTypePage, InvoiceType.StandardInvoice).success.value
        navigator.nextPage(SupplierAddressPage, CheckMode, ua) mustBe purchaseRoutes.SupplierVatRegistrationNumberController.onPageLoad(CheckMode)
      }

      "must go from SupplierAddressPage to SimplifiedInvoiceVatRegCheckController if country is missing and no invoice type" in {
        navigator.nextPage(SupplierAddressPage, CheckMode, userAnswers) mustBe
          purchaseRoutes.SimplifiedInvoiceVatRegCheckController.onPageLoad(CheckMode)
      }

      "must go from SupplierAddressPage to SupplierVatRegistrationNumberController in CheckMode if country is not Germany and standard invoice" in {
        val ua = userAnswers
          .set(RefundingCountryPage, "AT")
          .success
          .value
          .set(InvoiceTypePage, InvoiceType.StandardInvoice)
          .success
          .value
        navigator.nextPage(SupplierAddressPage, CheckMode, ua) mustBe purchaseRoutes.SupplierVatRegistrationNumberController.onPageLoad(CheckMode)
      }

      "must go from SupplierTaxNumberPage to SupplierVatRegistrationNumberController if VAT registration number is selected" in {
        val ua = userAnswers.set(SupplierTaxNumberPage, SupplierTaxNumber.Vatregistrationnumber).success.value
        navigator.nextPage(SupplierTaxNumberPage, CheckMode, ua) mustBe
          purchaseRoutes.SupplierVatRegistrationNumberController.onPageLoad(CheckMode)
      }

      "must go from SupplierTaxNumberPage to SupplierTaxIdentifierNumberController if tax identifier number is selected" in {
        val ua = userAnswers.set(SupplierTaxNumberPage, SupplierTaxNumber.Taxidentifiernumber).success.value
        navigator.nextPage(SupplierTaxNumberPage, CheckMode, ua) mustBe
          purchaseRoutes.SupplierTaxIdentifierNumberController.onPageLoad(CheckMode)
      }

      "must go from SupplierTaxNumberPage to JourneyRecoveryController in CheckMode when no answer present" in {
        navigator.nextPage(SupplierTaxNumberPage, CheckMode, userAnswers) mustBe controllers.routes.JourneyRecoveryController.onPageLoad()
      }

      "must go from SupplierVatRegistrationNumberPage to TotalPurchaseAmountBeforeVatController in CheckMode" in {
        val ua = userAnswers.set(pages.RefundingCountryPage, "AT").success.value
        navigator.nextPage(SupplierVatRegistrationNumberPage, CheckMode, ua) mustBe
          purchaseRoutes.TotalPurchaseAmountBeforeVatController.onPageLoad(CheckMode)
      }

      "must go from SupplierVatRegistrationNumberPage to RefundingCurrencyController in CheckMode when country has more than one currency" in {
        val ua = userAnswers.set(pages.RefundingCountryPage, "EE").success.value
        navigator.nextPage(SupplierVatRegistrationNumberPage, CheckMode, ua) mustBe
          purchaseRoutes.RefundingCurrencyController.onPageLoad(CheckMode)
      }

      "must go from SimplifiedInvoiceVatRegCheckPage to RefundingCurrencyController in CheckMode if no selected and the country has more than one currency" in {
        val ua = userAnswers
          .set(pages.SimplifiedInvoiceVatRegCheckPage, false)
          .success
          .value
          .set(pages.RefundingCountryPage, "EE")
          .success
          .value
        navigator.nextPage(SimplifiedInvoiceVatRegCheckPage, CheckMode, ua) mustBe
          purchaseRoutes.RefundingCurrencyController.onPageLoad(CheckMode)
      }

      "must go from TotalPurchaseAmountBeforeVatPage to TotalVatPaidController in CheckMode" in {
        navigator.nextPage(TotalPurchaseAmountBeforeVatPage, CheckMode, userAnswers) mustBe
          purchaseRoutes.TotalVatPaidController.onPageLoad(CheckMode)
      }

      "must go from TotalVatPaidPage to TotalVatClaimController in CheckMode" in {
        navigator.nextPage(TotalVatPaidPage, CheckMode, userAnswers) mustBe
          purchaseRoutes.TotalVatClaimController.onPageLoad(CheckMode)
      }

      "must go from TotalVatClaimPage to CheckYourPurchaseDetailsController in CheckMode" in {
        navigator.nextPage(TotalVatClaimPage, CheckMode, userAnswers) mustBe
          purchaseRoutes.CheckYourPurchaseDetailsController.onPageLoad()
      }

      "must go from SimplifiedInvoiceVatRegCheckPage to TotalPurchaseAmountBeforeVatController if no selected" in {
        val ua = userAnswers.set(SimplifiedInvoiceVatRegCheckPage, false).success.value
        navigator.nextPage(SimplifiedInvoiceVatRegCheckPage, CheckMode, ua) mustBe
          purchaseRoutes.TotalPurchaseAmountBeforeVatController.onPageLoad(CheckMode)
      }

      "must go from CheckYourStateDetailsPage to CheckYourClaimDetailsController in CheckMode if no selected" in {
        val ua = userAnswers.set(CheckYourStateDetailsPage, false).success.value
        navigator.nextPage(CheckYourStateDetailsPage, CheckMode, ua) mustBe
          claimRoutes.CheckYourClaimDetailsController.onPageLoad()
      }
    }
  }
}
