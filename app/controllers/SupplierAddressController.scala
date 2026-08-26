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
import forms.SupplierAddressFormProvider
import models.requests.DataRequest
import models.{CheckMode, Mode, NormalMode, SupplierAddress, UserAnswers}
import navigation.Navigator
import pages.{PurchaseTypePage, SupplierAddressPage}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.ControllerHelpers.*
import utils.{CheckModeShortCircuit, SaveAndRedirect}
import views.html.SupplierAddressView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

class SupplierAddressController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: SupplierAddressFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: SupplierAddressView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  val form: Form[SupplierAddress] = formProvider()

  private def backLink: Call = routes.SuppliersNameController.onPageLoad(NormalMode)

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    val preparedForm = form.preparedFromAnswers(SupplierAddressPage, request.userAnswers)
    okView(preparedForm, mode)
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(badRequestView(formWithErrors, mode)),
        value =>
          CheckModeShortCircuit.applyNoPersist(
            CheckModeShortCircuit.ShortCircuitNoPersistArgs(
              SupplierAddressPage,
              value,
              mode,
              request.userAnswers,
              controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad(),
              (answersAfterSet: UserAnswers) => {
                val userAnswersTry = Success(answersAfterSet)

                val redirectCall = computeRedirectAfterSave(answersAfterSet, mode)

                SaveAndRedirect.saveTryAndRedirect(userAnswersTry, sessionRepository, redirectCall)
              }
            )
          )
      )
  }

  private def okView(preparedForm: Form[?], mode: Mode)(implicit request: DataRequest[?]) =
    Ok(view(preparedForm, mode, backLink))

  private def badRequestView(formWithErrors: Form[?], mode: Mode)(implicit request: DataRequest[?]) =
    BadRequest(view(formWithErrors, mode, backLink))

  private def computeRedirectAfterSave(answersAfterSet: UserAnswers, mode: Mode)(implicit request: DataRequest[?]) =
    if (mode == CheckMode && request.userAnswers.get(PurchaseTypePage).isDefined) {
      controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()
    } else {
      navigator.nextPage(SupplierAddressPage, mode, answersAfterSet)
    }
}
