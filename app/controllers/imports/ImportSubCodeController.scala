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

package controllers.imports

import controllers.actions.*
import forms.purchase.PurchaseSubTypeFormProvider
import forms.imports.SadReferenceFormProvider
import controllers.routes
import forms.PurchaseOrImportSubTypeFormProvider
import models.requests.DataRequest
import models.{NormalMode, PurchaseOrImportType}
import navigation.Navigator
import pages.{ImportSubCodePage, ImportTypePage}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import repositories.SessionRepository
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.{ConfigPurchaseOrImportMapping, CountryCode}
import views.html.PurchaseOrImportSubTypeView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ImportSubCodeController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: PurchaseOrImportSubTypeFormProvider,
  config: ConfigPurchaseOrImportMapping,
  sadFormProvider: SadReferenceFormProvider,
  sadView: views.html.imports.SadReferenceView,
  val controllerComponents: MessagesControllerComponents,
  view: PurchaseOrImportSubTypeView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  private def backUrl: String = routes.ImportTypeController.onPageLoad(NormalMode).url

  private def withPageData(importTypeKey: String)(
    block: (PurchaseOrImportType, Seq[(String, String)]) => Future[Result]
  )(implicit request: DataRequest[AnyContent]): Future[Result] = {
    val importTypeOpt = PurchaseOrImportType.values.find(_.toString == importTypeKey)
    val answeredImportOpt = request.userAnswers.get(ImportTypePage)
    val countryOpt = CountryCode.findCountryCode(request.userAnswers)

    val resolved = for {
      importType <- importTypeOpt
      answered   <- answeredImportOpt if answered == importType
      country    <- countryOpt
      options    <- config.selectableSubcodes(country, importType.toString)
    } yield (importType, options)

    resolved match {
      case Some((importType, options)) => block(importType, options)
      case None                        =>
        // Render SAD question directly so GET returns OK with SAD content
        // TODO: perhaps to change again after level 3 is done
        val preparedForm = sadFormProvider()
        Future.successful(Ok(sadView(preparedForm, controllers.imports.routes.ImportTypeController.onPageLoad(models.NormalMode))))
    }
  }

  private def radioItems(options: Seq[(String, String)])(implicit request: DataRequest[AnyContent]): Seq[RadioItem] = {
    val items = config.buildRadioItems(options, request2Messages)
    if (options.map(_._1).contains(ConfigPurchaseOrImportMapping.NoneOfTheseSubCode)) {
      items.filterNot(_.value.contains(ConfigPurchaseOrImportMapping.NoneValue))
    } else {
      items
    }
  }

  private def allowedValues(options: Seq[(String, String)]): Seq[String] = {
    val codes = options.map(_._1)
    if (codes.contains(ConfigPurchaseOrImportMapping.NoneOfTheseSubCode)) codes else codes :+ ConfigPurchaseOrImportMapping.NoneValue
  }

  private def renderView(importType: PurchaseOrImportType, options: Seq[(String, String)], form: Form[String])(implicit
    request: DataRequest[AnyContent]
  ) = {
    val messages = request2Messages
    view(
      form,
      radioItems(options),
      messages(s"importSubCode.$importType.title"),
      messages(s"importSubCode.$importType.heading"),
      "import.caption",
      routes.ImportSubCodeController.onSubmit(importType.toString),
      backUrl
    )
  }

  def onPageLoad(importTypeKey: String): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    withPageData(importTypeKey) { (importType, options) =>
      val form = formProvider(s"importSubCode.$importType.error.required")
      val preparedForm = request.userAnswers.get(ImportSubCodePage) match {
        case Some(value) => form.fill(value)
        case None        => form
      }

      Future.successful(Ok(renderView(importType, options, preparedForm)))
    }
  }

  def onSubmit(importTypeKey: String): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    withPageData(importTypeKey) { (importType, options) =>
      formProvider(s"importSubCode.$importType.error.required")
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(renderView(importType, options, formWithErrors))),
          value =>
            if (allowedValues(options).contains(value)) {
              for {
                updatedAnswers <- Future.fromTry(request.userAnswers.set(ImportSubCodePage, value))
                _              <- sessionRepository.set(updatedAnswers)
              } yield Redirect(navigator.nextPage(ImportSubCodePage, NormalMode, updatedAnswers))
            } else {
              Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
            }
        )
    }
  }
}
