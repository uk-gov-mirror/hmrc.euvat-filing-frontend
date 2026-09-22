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
import controllers.routes
import forms.PurchaseOrImportSubTypeFormProvider
import models.requests.DataRequest
import models.{Mode, PurchaseOrImportType}
import navigation.Navigator
import pages.{ImportSubCategoryPage, ImportSubCodePage, ImportTypePage}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import queries.ImportSubCategoryLabelQuery
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.PurchaseOrImportHelpers.*
import utils.{ConfigPurchaseOrImportMapping, CountryCode}
import views.html.PurchaseOrImportSubTypeView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ImportSubCategoryController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: PurchaseOrImportSubTypeFormProvider,
  config: ConfigPurchaseOrImportMapping,
  val controllerComponents: MessagesControllerComponents,
  view: PurchaseOrImportSubTypeView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  private case class PageData(importType: PurchaseOrImportType, subCode: String, options: Seq[(String, String)]) {
    val parentKey: String = importType.toString
  }

  private def withPageData(block: PageData => Future[Result])(implicit request: DataRequest[AnyContent]): Future[Result] = {
    val resolved = for {
      importType <- request.userAnswers.get(ImportTypePage)
      subCode    <- request.userAnswers.get(ImportSubCodePage)
      country    <- CountryCode.findCountryCode(request.userAnswers)
      options    <- Some(config.subcategoriesFor(country, importType.toString, subCode)).filter(_.nonEmpty)
    } yield PageData(importType, subCode, options)

    resolved.fold(Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad())))(block)
  }

  private def requiredKey(data: PageData)(implicit request: DataRequest[AnyContent]): String =
    requiredErrorKey(data.parentKey, Some(data.subCode))

  private def renderView(data: PageData, form: Form[String], mode: Mode)(implicit request: DataRequest[AnyContent]) = {
    val title = subCategoryTitle(data.parentKey, data.subCode, data.options)
    view(
      form,
      radioItems(config, data.options),
      title,
      title,
      "import.caption",
      controllers.imports.routes.ImportSubCategoryController.onSubmit(mode),
      controllers.imports.routes.ImportSubCodeController.onPageLoad(data.parentKey).url
    )
  }

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    withPageData { data =>
      val form = preparedForm(formProvider, requiredKey(data), request.userAnswers.get(ImportSubCategoryPage))
      Future.successful(Ok(renderView(data, form, mode)))
    }
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    withPageData { data =>
      formProvider(requiredKey(data))
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(renderView(data, formWithErrors, mode))),
          value =>
            if (allowedValues(data.options).contains(value)) {
              val label = labelFor(value, data.options)
              for {
                updatedAnswers <- Future.fromTry(
                                    setSelection(request.userAnswers, ImportSubCategoryPage, ImportSubCategoryLabelQuery, value, label)
                                  )
                _ <- sessionRepository.set(updatedAnswers)
              } yield Redirect(navigator.nextPage(ImportSubCategoryPage, mode, updatedAnswers))
            } else {
              Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad()))
            }
        )
    }
  }
}
