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
import forms.imports.ImportDetailsInfoFormProvider
import models.requests.{DataRequest, ReferenceInfoCountRequest}
import models.{Mode, UserAnswers}
import navigation.Navigator
import pages.AddImportResponsePage
import pages.imports.{ImportDetailsInfoPage, SadReferenceCheckPage, SadReferenceNumberPage}
import pages.purchase.AddPurchaseResponsePage
import play.api.Logging
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.ClaimApplicationResponseQuery
import repositories.SessionRepository
import services.EuVatRefundsService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.imports.ImportDetailsInfoView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ImportDetailsInfoController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: ImportDetailsInfoFormProvider,
  val controllerComponents: MessagesControllerComponents,
  euVatRefundsService: EuVatRefundsService,
  view: ImportDetailsInfoView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with Logging
    with I18nSupport {

  val form: Form[String] = formProvider()

  private def backLink(mode: Mode)(implicit request: DataRequest[?]) =
    request.userAnswers.get(SadReferenceCheckPage) match {
      case Some(true) =>
        request.userAnswers.get(SadReferenceNumberPage) match {
          case Some(_) => controllers.imports.routes.SadReferenceNumberController.onPageLoad(mode)
          case None    => controllers.imports.routes.SadReferenceCheckController.onPageLoad(mode)
        }
      case _ => controllers.imports.routes.SadReferenceCheckController.onPageLoad(mode)
    }

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>

    val preparedForm = request.userAnswers.get(ImportDetailsInfoPage) match {
      case None        => form
      case Some(value) => form.fill(value)
    }

    Ok(view(preparedForm, mode, backLink(mode)))
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>

    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, backLink(mode)))),
        value => {
          buildRequest(request.userAnswers) match {
            case Some(referenceInfoCountRequest) =>
              euVatRefundsService
                .getReferenceInfoCount(referenceInfoCountRequest)
                .flatMap { response =>
                  for {
                    updatedAnswers <- Future.fromTry(request.userAnswers.set(ImportDetailsInfoPage, value))
                    _              <- sessionRepository.set(updatedAnswers)
                  } yield {
                    if (response.count > 0) {
                      Redirect(controllers.warning.routes.ImportDetailsWarningController.onPageLoad(mode))
                    } else {
                      Redirect(navigator.nextPage(ImportDetailsInfoPage, mode, updatedAnswers))
                    }
                  }
                }
            case None =>
              logger.warn("Session is missing applicationId or itemNumber")
              Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
          }
        }
      )
  }

  private def buildRequest(userAnswers: UserAnswers): Option[ReferenceInfoCountRequest] = {
    for {
      applicationId <- userAnswers.get(ClaimApplicationResponseQuery).map(_.applicationId)
      itemNumber    <- userAnswers.get(AddImportResponsePage).map(_.itemNumber)
    } yield ReferenceInfoCountRequest(applicationId, itemNumber)
  }
}
