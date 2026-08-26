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
import forms.PurchaseTypeFormProvider
import models.requests.{AddPurchaseRequest, DataRequest}
import models.{CheckMode, Mode, PurchaseType, UserAnswers}
import navigation.Navigator
import pages.*
import play.api.Logging
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.*
import queries.ClaimApplicationResponseQuery
import repositories.SessionRepository
import services.EuVatRefundsService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import models.responses.AddPurchaseResponse
import utils.{CheckModeShortCircuit, ConfigPurchaseMapping, CountryCode, MountPrefix}
import utils.ControllerHelpers.*
import views.html.PurchaseTypeView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class PurchaseTypeController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  config: ConfigPurchaseMapping,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: PurchaseTypeFormProvider,
  val controllerComponents: MessagesControllerComponents,
  euVatRefundsService: EuVatRefundsService,
  view: PurchaseTypeView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form: Form[PurchaseType] = formProvider()

  private def backLink(mode: Mode)(implicit request: DataRequest[?]) =
    // We always return to the 'BeforeYouStart' page from here
    routes.BeforeYouStartController.onPageLoad()

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    if (request.userAnswers.get(CountryChangedPage).contains(true)) {
      val clearedTry = for {
        afterRemovedPurchaseType        <- request.userAnswers.remove(PurchaseTypePage)
        afterRemovedPurchaseSubType     <- afterRemovedPurchaseType.remove(PurchaseSubTypePage)
        afterRemovedPurchaseSubTypeLbl  <- afterRemovedPurchaseSubType.remove(PurchaseSubTypeLabelPage)
        afterRemovedPurchaseSubCategory <- afterRemovedPurchaseSubTypeLbl.remove(PurchaseSubCategoryPage)
        afterRemovedPurchaseSubCatLbl   <- afterRemovedPurchaseSubCategory.remove(PurchaseSubCategoryLabelPage)
        afterClearedFlag                <- afterRemovedPurchaseSubCatLbl.remove(CountryChangedPage)
      } yield afterClearedFlag

      Future
        .fromTry(clearedTry)
        .flatMap(updated =>
          sessionRepository
            .set(updated)
            .map(_ => {
              val preparedForm = form.preparedFromAnswers(PurchaseTypePage, updated)
              Ok(view(preparedForm, mode, backLink(mode)))
            })
        )
    } else {
      val preparedForm = form.preparedFromAnswers(PurchaseTypePage, request.userAnswers)
      Future.successful(Ok(view(preparedForm, mode, backLink(mode))))
    }
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, backLink(mode)))),
        value => {
          val previous = request.userAnswers.get(PurchaseTypePage)

          if (mode == CheckMode && previous.contains(value)) {
            handleUnchangedCheckModeSubmission(value, mode)
          } else {
            handleSubmissionWhenChangedOrNormal(value, mode)
          }
        }
      )
  }
  private def redirectWithPrefix(call: Call)(implicit request: RequestHeader): Result = {
    val prefix = MountPrefix.getFromRequest
    if (prefix.isEmpty || call.url.startsWith(prefix)) {
      Redirect(call)
    } else {
      Redirect(Call(call.method, s"$prefix${call.url}"))
    }
  }

  private def arrivedFromDescribe(implicit request: DataRequest[?]): Boolean =
    request.userAnswers.get(DescribeItemsArrivedFromCheckYourAnswersPage).contains(true)

  private def arrivedFromSubTypeOrCategory(implicit request: DataRequest[?]): Boolean = {
    val isPurchaseSubTypeArrivedFromCheckYourAnswers = request.userAnswers
      .get(PurchaseSubTypeArrivedFromCheckYourAnswersPage)
      .contains(true)
    val isPurchaseSubCategoryArrivedFromCheckYourAnswers = request.userAnswers
      .get(PurchaseSubCategoryArrivedFromCheckYourAnswersPage)
      .contains(true)
    isPurchaseSubTypeArrivedFromCheckYourAnswers || isPurchaseSubCategoryArrivedFromCheckYourAnswers
  }

  private def describePresent(implicit request: DataRequest[?]): Boolean = request.userAnswers.get(DescribeItemsOnInvoicePage).exists(_.trim.nonEmpty)

  private def isNoneSubTypeSelection(subType: String): Boolean =
    subType == ConfigPurchaseMapping.NoneValue || subType.split("\\.").lastOption.contains("99")

  private def shouldReturnToDescribeForOtherNone(value: PurchaseType)(implicit request: DataRequest[?]): Boolean =
    value == models.Other && request.userAnswers.get(PurchaseSubTypePage).exists(isNoneSubTypeSelection)

  private def hasMeaningfulSubcodes(value: PurchaseType)(implicit request: DataRequest[?]): Boolean =
    CountryCode
      .findCountryCode(request.userAnswers)
      .flatMap { c =>
        try Some(config.subcodesFor(c, value.toString).exists { case (code, _) => !code.split("\\.").lastOption.contains("99") })
        catch { case _: Throwable => None }
      }
      .getOrElse(true)

  private def handleUnchangedCheckModeSubmission(value: PurchaseType, mode: Mode)(implicit request: DataRequest[?]): Future[Result] = {
    if (arrivedFromDescribe && !arrivedFromSubTypeOrCategory && shouldReturnToDescribeForOtherNone(value)) {
      if (describePresent || hasMeaningfulSubcodes(value)) {
        val removedTry = request.userAnswers.remove(DescribeItemsArrivedFromCheckYourAnswersPage)
        Future.fromTry(removedTry).flatMap { ua =>
          sessionRepository.set(ua).map { _ =>
            redirectWithPrefix(controllers.routes.DescribeItemsOnInvoiceController.onPageLoad(CheckMode))
          }
        }
      } else {
        shortCircuitOrFailForPurchaseType(value, mode)
      }
    } else {
      shortCircuitOrFailForPurchaseType(value, mode)
    }
  }

  private def shortCircuitOrFailForPurchaseType(value: PurchaseType, mode: Mode)(implicit request: DataRequest[?]): Future[Result] =
    if (mode == CheckMode && request.userAnswers.isAnswerUnchanged(PurchaseTypePage, value)) {
      Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
    } else {
      Future.failed(new IllegalStateException("Expected short-circuit result for unchanged CheckMode submission"))
    }

  private def handleSubmissionWhenChangedOrNormal(value: PurchaseType, mode: Mode)(implicit request: DataRequest[?]): Future[Result] = {
    if (mode == CheckMode && request.userAnswers.isAnswerUnchanged(PurchaseTypePage, value)) {
      Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
    } else {
      val userAnswersTry = request.userAnswers.get(PurchaseTypePage) match {
        case Some(prev) if prev != value => buildUpdatedTryForPurchaseTypeChange(value)
        case _                           => request.userAnswers.set(PurchaseTypePage, value)
      }

      persistAndHandleSaved(userAnswersTry, value, mode)
    }
  }

  private def buildUpdatedTryForPurchaseTypeChange(value: PurchaseType)(implicit request: DataRequest[?]): Try[UserAnswers] =
    for {
      afterRemovedSubType        <- request.userAnswers.remove(PurchaseSubTypePage)
      afterRemovedSubTypeLabel   <- afterRemovedSubType.remove(PurchaseSubTypeLabelPage)
      afterRemovedSubCategory    <- afterRemovedSubTypeLabel.remove(PurchaseSubCategoryPage)
      afterRemovedSubCategoryLbl <- afterRemovedSubCategory.remove(PurchaseSubCategoryLabelPage)
      afterRemovedDescribe       <- afterRemovedSubCategoryLbl.remove(DescribeItemsOnInvoicePage)
      afterSetPurchaseType       <- afterRemovedDescribe.set(PurchaseTypePage, value)
    } yield afterSetPurchaseType

  private def persistAndHandleSaved(userAnswersTry: Try[UserAnswers], value: PurchaseType, mode: Mode)(implicit
    request: DataRequest[?]
  ): Future[Result] =
    Future.fromTry(userAnswersTry).flatMap { persistedAnswers =>
      sessionRepository.set(persistedAnswers).flatMap { _ =>
        if (persistedAnswers.get(AddPurchaseResponsePage).isEmpty && persistedAnswers.get(queries.ClaimApplicationResponseQuery).isDefined) {
          addPurchaseAndPersist(persistedAnswers, value, mode)
        } else if (mode == CheckMode) {
          handleCheckModePostPersist(persistedAnswers, value)
        } else {
          handleNormalModeRedirect(persistedAnswers, mode)
        }
      }
    }
  private def hasSubcodesFor(answers: UserAnswers, value: PurchaseType): Boolean =
    CountryCode
      .findCountryCode(answers)
      .flatMap { c =>
        try Some(config.subcodesFor(c, value.toString).nonEmpty)
        catch { case _: Throwable => None }
      }
      .getOrElse(true)

  private def removeFlagThenRedirectToSubType(flagPage: QuestionPage[Boolean], answers: UserAnswers, value: PurchaseType)(implicit
    req: RequestHeader
  ): Future[Result] = {
    val removeTry = answers.remove(flagPage)
    Future.fromTry(removeTry).flatMap { ua =>
      sessionRepository.set(ua).map { _ =>
        redirectWithPrefix(controllers.purchase.routes.PurchaseSubTypeController.onPageLoad(PurchaseType.urlSlugForPurchaseType(value), CheckMode))
      }
    }
  }

  private def removeFlagAndRedirect(flagPage: QuestionPage[Boolean], answers: UserAnswers, call: => Call)(implicit
    req: RequestHeader
  ): Future[Result] = {
    val removeTry = answers.remove(flagPage)
    Future.fromTry(removeTry).flatMap { ua =>
      sessionRepository.set(ua).map { _ =>
        redirectWithPrefix(call)
      }
    }
  }

  private def clearDescribeArrivalFlagIfPresent(answers: UserAnswers): Future[UserAnswers] =
    if (answers.get(DescribeItemsArrivedFromCheckYourAnswersPage).contains(true)) {
      Future
        .fromTry(answers.remove(DescribeItemsArrivedFromCheckYourAnswersPage))
        .flatMap { ua =>
          sessionRepository.set(ua).map(_ => ua)
        }
    } else {
      Future.successful(answers)
    }

  private def shouldReturnToDescribeForAnswers(answers: UserAnswers, value: PurchaseType): Boolean =
    value == models.Other && answers.get(PurchaseSubTypePage).exists(isNoneSubTypeSelection)

  private def redirectChangePath(value: PurchaseType)(implicit request: RequestHeader): Future[Result] = {
    val slug = PurchaseType.urlSlugForPurchaseType(value)
    val prefix = MountPrefix.getFromRequest
    val changePath = s"${if (prefix.isEmpty) "" else prefix}/change-$slug"
    Future.successful(Redirect(Call("GET", changePath)))
  }

  private def handleCheckModePostPersist(updatedAnswers: UserAnswers, value: PurchaseType)(implicit request: DataRequest[?]): Future[Result] = {
    processCheckModePostPersist(updatedAnswers, value)(request)
  }

  private def processCheckModePostPersist(updatedAnswers: UserAnswers, value: PurchaseType)(implicit request: DataRequest[?]): Future[Result] = {
    if (!hasSubcodesFor(updatedAnswers, value)) {
      handleNoSubcodesCase(updatedAnswers)
    } else if (updatedAnswers.get(DescribeItemsArrivedFromCheckYourAnswersPage).contains(true)) {
      handleDescribeArrivedCase(updatedAnswers, value)
    } else if (updatedAnswers.get(PurchaseSubTypeArrivedFromCheckYourAnswersPage).contains(true)) {
      removeFlagAndRedirect(
        PurchaseSubTypeArrivedFromCheckYourAnswersPage,
        updatedAnswers,
        controllers.purchase.routes.PurchaseSubTypeController.onPageLoad(PurchaseType.urlSlugForPurchaseType(value), CheckMode)
      )(request)
    } else if (updatedAnswers.get(PurchaseSubCategoryArrivedFromCheckYourAnswersPage).contains(true)) {
      removeFlagAndRedirect(
        PurchaseSubCategoryArrivedFromCheckYourAnswersPage,
        updatedAnswers,
        controllers.purchase.routes.PurchaseSubTypeController.onPageLoad(PurchaseType.urlSlugForPurchaseType(value), CheckMode)
      )(request)
    } else {
      redirectChangePath(value)
    }
  }

  private def handleNoSubcodesCase(updatedAnswers: UserAnswers)(implicit request: DataRequest[?]): Future[Result] = {
    val cya = controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()

    removeFirstFlagThen(
      updatedAnswers,
      Seq(
        (DescribeItemsArrivedFromCheckYourAnswersPage, (_: UserAnswers) => cya),
        (PurchaseSubTypeArrivedFromCheckYourAnswersPage, (_: UserAnswers) => cya),
        (PurchaseSubCategoryArrivedFromCheckYourAnswersPage, (_: UserAnswers) => cya)
      )
    ).flatMap {
      case Some(result) => Future.successful(result)
      case None         => Future.successful(Redirect(cya))
    }
  }

  private def handleDescribeArrivedCase(updatedAnswers: UserAnswers, value: PurchaseType)(implicit request: DataRequest[?]): Future[Result] = {
    if (shouldReturnToDescribeForAnswers(updatedAnswers, value)) {
      removeFlagAndRedirect(DescribeItemsArrivedFromCheckYourAnswersPage,
                            updatedAnswers,
                            controllers.routes.DescribeItemsOnInvoiceController.onPageLoad(CheckMode)
                           )(request)
    } else {
      clearDescribeArrivalFlagIfPresent(updatedAnswers).flatMap { clearedAnswers =>
        removeFirstFlagThen(
          clearedAnswers,
          Seq(
            (PurchaseSubTypeArrivedFromCheckYourAnswersPage,
             (_: UserAnswers) => controllers.purchase.routes.PurchaseSubTypeController.onPageLoad(PurchaseType.urlSlugForPurchaseType(value), CheckMode)
            ),
            (PurchaseSubCategoryArrivedFromCheckYourAnswersPage,
             (_: UserAnswers) => controllers.purchase.routes.PurchaseSubTypeController.onPageLoad(PurchaseType.urlSlugForPurchaseType(value), CheckMode)
            )
          )
        ).flatMap {
          case Some(result) => Future.successful(result)
          case None         => redirectChangePath(value)
        }
      }
    }
  }

  private def removeFirstFlagThen(
    answers: UserAnswers,
    pairs: Seq[(QuestionPage[Boolean], UserAnswers => Call)]
  )(implicit request: DataRequest[?]): Future[Option[Result]] = {
    pairs.collectFirst { case (page, callF) if answers.get(page).contains(true) => (page, callF) } match {
      case Some((page, callF)) =>
        val call = callF(answers)
        removeFlagAndRedirect(page, answers, call)(request).map(Some(_))
      case None => Future.successful(None)
    }
  }

  private def handleNormalModeRedirect(updatedAnswers: UserAnswers, mode: Mode)(implicit request: DataRequest[?]): Future[Result] = {
    val call = navigator.nextPage(PurchaseTypePage, mode, updatedAnswers)
    val prefix = MountPrefix.getFromRequest
    if (prefix.isEmpty || call.url.startsWith(prefix)) Future.successful(Redirect(call))
    else Future.successful(Redirect(Call(call.method, s"$prefix${call.url}")))
  }

  private def addPurchaseAndPersist(
    answers: UserAnswers,
    purchaseType: PurchaseType,
    mode: Mode
  )(implicit request: DataRequest[?]): Future[Result] = {

    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromRequestAndSession(request, request.session)
    answers
      .get(ClaimApplicationResponseQuery)
      .fold {
        logger.warn("Missing applicationId for addPurchase")
        Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad()))
      } { claimResponse =>

        val purchaseRequest = AddPurchaseRequest(
          applicationId            = claimResponse.applicationId,
          goodsDescriptionCategory = PurchaseType.codes(purchaseType),
          updateSequenceNumber     = claimResponse.updateSeqNumber
        )

        def persistAddPurchaseResponseAndRedirect(response: AddPurchaseResponse): Future[Result] =
          for {
            updatedAnswers <- Future.fromTry(answers.set(AddPurchaseResponsePage, response))
            _              <- sessionRepository.set(updatedAnswers)
          } yield redirectWithPrefix(navigator.nextPage(PurchaseTypePage, mode, updatedAnswers))(request)

        euVatRefundsService
          .addPurchase(purchaseRequest)
          .flatMap(persistAddPurchaseResponseAndRedirect)
          .recover { case ex =>
            logger.error("Error while adding the purchase", ex)
            Redirect(routes.JourneyRecoveryController.onPageLoad())
          }
      }
  }
}
