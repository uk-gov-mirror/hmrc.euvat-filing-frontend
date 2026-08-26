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

import controllers.actions.*
import forms.PurchaseSubTypeFormProvider
import models.requests.DataRequest
import models.{CheckMode, Mode, PurchaseSubCategoryType, PurchaseType, UserAnswers}
import navigation.Navigator
import pages.*
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.Logging
import play.api.mvc.*
import play.api.data.Form
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.{ConfigPurchaseMapping, ControllerHelpers, CountryCode, MountPrefix}
import views.html.PurchaseSubTypeView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import models.{Mode, Other, PurchaseSubCategoryType, PurchaseType, UserAnswers}

class PurchaseSubTypeController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: PurchaseSubTypeFormProvider,
  config: ConfigPurchaseMapping,
  val controllerComponents: MessagesControllerComponents,
  view: PurchaseSubTypeView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with play.api.Logging:

  private def resolveParentAndCountry(purchaseTypeSlug: String, userAnswers: UserAnswers): Option[(String, String)] =
    // Attempt to determine `parentKey` from the provided slug first
    val parentKey =
      PurchaseType.valueFromUrlSlug
        .get(purchaseTypeSlug)
        .orElse(
          userAnswers
            .get(PurchaseTypePage)
            .map(_.toString)
        )
    // Attempt to determine the refunding country from `UserAnswers`
    val country = CountryCode.findCountryCode(userAnswers)

    (parentKey, country) match {
      case (Some(parentKey), Some(country)) => Some((parentKey, country))
      case _                                => None
    }

  private def prepareViewData(parentKey: String, country: String, purchaseTypeSlug: String, userAnswers: UserAnswers, mode: Mode)(implicit
    request: RequestHeader
  ) = {
    val options = config.subcodesFor(country, parentKey)

    val rawItems = config.buildRadioItems(options, messagesApi.preferred(request))

    // For the `other` parent we exclude the sentinel 'None' option from the list
    val items = if (parentKey == "other") rawItems.filterNot(_.value.contains(ConfigPurchaseMapping.NoneValue)) else rawItems

    val parentHeading = parentHeadingFor(parentKey)

    val msgs = messagesApi.preferred(request)

    // Pick a validation error key scoped to the parent when available
    val requiredKeyCandidates = Seq(s"purchase.sub.$parentKey.error.required")
    val requiredKey = requiredKeyCandidates.find(k => msgs.isDefinedAt(k)).getOrElse("error.required")

    val preparedForm = userAnswers.get(PurchaseSubTypePage).fold(formProvider(requiredKey))(formProvider(requiredKey).fill)

    val resolvedSlug = resolvedSlugFor(parentKey, purchaseTypeSlug)
    val formAction = formActionFor(resolvedSlug, mode)

    (options, items, parentHeading, preparedForm, resolvedSlug, formAction)
  }

  private def ensurePurchaseTypeWhenMissing(currentAnswers: UserAnswers,
                                            parentKey: String,
                                            updatedAnswers: UserAnswers
                                           ): scala.util.Try[UserAnswers] =
    currentAnswers.get(PurchaseTypePage) match {
      case Some(_) => scala.util.Success(updatedAnswers)
      case None =>
        PurchaseType.values.find(_.toString == parentKey) match {
          case Some(pt) => updatedAnswers.set(PurchaseTypePage, pt)
          case None     => scala.util.Success(updatedAnswers)
        }
    }

  private def isNoneOfTheseSelection(selection: String): Boolean =
    selection == ConfigPurchaseMapping.NoneValue || selection.split("\\.").lastOption.contains("99")

  private def isTransitionAwayFromNoneForOther(parentKey: String, previousSelection: String, newSelection: String): Boolean =
    parentKey == models.Other.toString && isNoneOfTheseSelection(previousSelection) && !isNoneOfTheseSelection(newSelection)

  private def persistChangedSelection(currentAnswers: UserAnswers, parentKey: String, value: String, label: String): scala.util.Try[UserAnswers] =
    for {
      removedSubCategory      <- currentAnswers.remove(PurchaseSubCategoryPage)
      removedSubCategoryLabel <- removedSubCategory.remove(PurchaseSubCategoryLabelPage)
      clearedDescribe <-
        currentAnswers.get(PurchaseSubTypePage) match {
          case Some(previousSelection) if isTransitionAwayFromNoneForOther(parentKey, previousSelection, value) =>
            for {
              afterRemovedDescribe <- removedSubCategoryLabel.remove(DescribeItemsOnInvoicePage)
              afterRemovedFlag     <- afterRemovedDescribe.remove(DescribeItemsArrivedFromCheckYourAnswersPage)
            } yield afterRemovedFlag
          case _ => scala.util.Success(removedSubCategoryLabel)
        }
      setSubType      <- clearedDescribe.set(PurchaseSubTypePage, value)
      setSubTypeLabel <- setSubType.set(PurchaseSubTypeLabelPage, label)
      finalAnswers    <- ensurePurchaseTypeWhenMissing(currentAnswers, parentKey, setSubTypeLabel)
    } yield finalAnswers

  private def persistUnchangedOrNewSelection(currentAnswers: UserAnswers,
                                             parentKey: String,
                                             value: String,
                                             label: String
                                            ): scala.util.Try[UserAnswers] =
    for {
      setSubType      <- currentAnswers.set(PurchaseSubTypePage, value)
      setSubTypeLabel <- setSubType.set(PurchaseSubTypeLabelPage, label)
      finalAnswers    <- ensurePurchaseTypeWhenMissing(currentAnswers, parentKey, setSubTypeLabel)
    } yield finalAnswers

  private def persistSelection(currentAnswers: UserAnswers, parentKey: String, value: String, label: String): scala.util.Try[UserAnswers] =
    currentAnswers.get(PurchaseSubTypePage) match {
      case Some(previousSelection) if previousSelection != value =>
        persistChangedSelection(currentAnswers, parentKey, value, label)

      case _ =>
        persistUnchangedOrNewSelection(currentAnswers, parentKey, value, label)
    }

  private def parentHeadingFor(parentKey: String)(implicit request: RequestHeader): String =
    // Map known parent keys to their localized headings; fallback to key
    parentKey match {
      case "fuel"         => messagesApi.preferred(request)("purchase.sub.fuel.heading")
      case "transport"    => messagesApi.preferred(request)("purchase.sub.transport.heading")
      case "foodAndDrink" => messagesApi.preferred(request)("purchase.sub.foodAndDrink.heading")
      case "luxuries"     => messagesApi.preferred(request)("purchase.sub.luxuries.heading")
      case "other"        => messagesApi.preferred(request)("purchase.sub.other.heading")
      case _              => parentKey
    }

  private def resolvedSlugFor(parentKey: String, fallback: String): String =
    // Derive a URL slug for routing from the PurchaseType enum or fallback
    PurchaseType.values.find(_.toString == parentKey).map(PurchaseType.urlSlugForPurchaseType).getOrElse(fallback)

  private def formActionFor(uri: String, mode: Mode)(implicit request: RequestHeader) = {
    // Compute POST action URL slug respecting mount prefix and CheckMode change- prefix
    val isChangeMode = if (mode == models.CheckMode) "change-" else ""
    Call("POST", s"${MountPrefix.getFromRequest}/$isChangeMode$uri")
  }

  private def backUrlFor(mode: Mode) = controllers.routes.PurchaseTypeController.onPageLoad(mode).url

  private def handleCountryChanged(purchaseTypeSlug: String, userAnswers: UserAnswers)(implicit request: RequestHeader) = {
    val clearedAnswers = for {
      afterRemovedSubType      <- userAnswers.remove(PurchaseSubTypePage)
      afterRemovedSubTypeLabel <- afterRemovedSubType.remove(PurchaseSubTypeLabelPage)
      afterClearedFlag         <- afterRemovedSubTypeLabel.remove(CountryChangedPage)
    } yield afterClearedFlag

    Future
      .fromTry(clearedAnswers)
      .flatMap(updated =>
        sessionRepository.set(updated).map { _ =>
          val prefix = MountPrefix.getFromRequest
          val path = if (prefix.isEmpty) s"/$purchaseTypeSlug" else s"$prefix/$purchaseTypeSlug"
          Redirect(Call("GET", path))
        }
      )
  }

  private def renderSubTypeView(preparedForm: Form[?], items: Seq[RadioItem], heading: String, formAction: Call, mode: Mode)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val backUrl = backUrlFor(mode)
    Future.successful(Ok(view(preparedForm, items, heading, heading, formAction, backUrl)))
  }

  private def markArrivalAndRenderSubType(preparedForm: Form[?],
                                          items: Seq[RadioItem],
                                          heading: String,
                                          formAction: Call,
                                          mode: Mode,
                                          userAnswers: UserAnswers
                                         )(implicit request: DataRequest[AnyContent]): Future[Result] =
    ControllerHelpers.markArrivalAndRender(
      PurchaseSubTypeArrivedFromCheckYourAnswersPage,
      mode,
      userAnswers,
      sessionRepository
    )(_ => renderSubTypeView(preparedForm, items, heading, formAction, mode))

  private def redirectWhenNoOptions(mode: Mode): Future[Result] =
    Future.successful(
      ControllerHelpers.redirectToInvoiceTypeOrCYA(mode)
    )

  private def handleSingleOtherOption(options: Seq[(String, String)],
                                      preparedForm: Form[?],
                                      items: Seq[RadioItem],
                                      parentHeading: String,
                                      formAction: Call,
                                      mode: Mode,
                                      userAnswers: UserAnswers
                                     )(implicit request: DataRequest[AnyContent]): Future[Result] = {
    val singleCode = options.head._1
    val lastSeg = singleCode.split("\\.").lastOption.getOrElse(singleCode)
    if (lastSeg == "99") {
      val labelKey = options.head._2
      val label = if (labelKey != null && labelKey.nonEmpty) messagesApi.preferred(request)(labelKey) else singleCode
      val savedTry = persistSelection(userAnswers, "other", singleCode, label)

      for {
        updatedAnswers <- Future.fromTry(savedTry)
        _              <- sessionRepository.set(updatedAnswers)
      } yield Redirect(controllers.routes.DescribeItemsOnInvoiceController.onPageLoad(mode))
    } else {
      markArrivalAndRenderSubType(preparedForm, items, parentHeading, formAction, mode, userAnswers)
    }
  }

  private def handlePageLoadForResolvedParent(purchaseTypeSlug: String, parentKey: String, country: String, mode: Mode, userAnswers: UserAnswers)(
    implicit request: DataRequest[AnyContent]
  ): Future[Result] = {
    val (options, items, parentHeading, preparedForm, _, formAction) =
      prepareViewData(parentKey, country, purchaseTypeSlug, userAnswers, mode)(request)

    if (options.isEmpty) {
      redirectWhenNoOptions(mode)
    } else if (parentKey == "other" && options.size == 1) {
      handleSingleOtherOption(options, preparedForm, items, parentHeading, formAction, mode, userAnswers)
    } else {
      markArrivalAndRenderSubType(preparedForm, items, parentHeading, formAction, mode, userAnswers)
    }
  }

  def onPageLoad(purchaseTypeSlug: String, mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>
      if (request.userAnswers.get(CountryChangedPage).contains(true)) {
        handleCountryChanged(purchaseTypeSlug, request.userAnswers)
      } else {
        resolveParentAndCountry(purchaseTypeSlug, request.userAnswers) match {
          case Some((parentKey, country)) =>
            handlePageLoadForResolvedParent(
              purchaseTypeSlug,
              parentKey,
              country,
              mode,
              request.userAnswers
            )

          case None => Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
      }
  }

  private def badSubmitRequest(formWithErrors: Form[?], items: Seq[RadioItem], parentHeading: String, resolvedSlug: String, mode: Mode)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val formAction = formActionFor(resolvedSlug, mode)
    val backUrl = backUrlFor(mode)
    Future.successful(BadRequest(view(formWithErrors, items, parentHeading, parentHeading, formAction, backUrl)))
  }

  private def persistNoneSelection(mode: Mode, userAnswers: UserAnswers)(implicit request: DataRequest[AnyContent]): Future[Result] = {
    val noneLabel = ConfigPurchaseMapping.NoneValue
    val savedTry = for {
      a1 <- userAnswers.set(PurchaseSubTypePage, ConfigPurchaseMapping.NoneValue)
      a2 <- a1.set(PurchaseSubTypeLabelPage, noneLabel)
      a3 <- a2.remove(PurchaseSubCategoryPage)
      a4 <- a3.remove(PurchaseSubCategoryLabelPage)
    } yield a4

    for {
      updatedAnswers <- Future.fromTry(savedTry)
      _              <- sessionRepository.set(updatedAnswers)
      result         <- redirectWhenNoOptions(mode)
    } yield result
  }

  private def routeToSubCategory(parentKey: String, value: String, mode: Mode)(implicit request: RequestHeader): Option[Call] = {
    val routeParentCodeCandidate = value
    val candidates = Seq(routeParentCodeCandidate).distinct

    candidates.iterator
      .map { c =>
        try {
          val slug = PurchaseSubCategoryType.pathFor(parentKey, c)
          val prefix = MountPrefix.getFromRequest
          val path = ControllerHelpers.pathForSlug(slug, mode, prefix)
          Some(Call("GET", path))
        } catch {
          case _: Throwable => None
        }
      }
      .collectFirst { case Some(call) => call }
  }

  private def noChildrenRedirect(value: String, resolvedSlug: String, mode: Mode): Result = {
    val lastSeg = value.split("\\.").lastOption.getOrElse(value)
    val isOtherPurchaseType =
      PurchaseType.values.find(pt => PurchaseType.urlSlugForPurchaseType(pt) == resolvedSlug).contains(models.Other)

    if (isOtherPurchaseType && lastSeg == "99") {
      Redirect(controllers.routes.DescribeItemsOnInvoiceController.onPageLoad(mode))
    } else {
      ControllerHelpers.redirectToInvoiceTypeOrCYA(mode)
    }
  }

  private def persistNormalSelection(parentKey: String, country: String, value: String, resolvedSlug: String, mode: Mode, userAnswers: UserAnswers)(
    implicit request: DataRequest[AnyContent]
  ): Future[Result] = {
    val labelKeyOpt = config.subcodesFor(country, parentKey).find(_._1 == value).map(_._2)
    val label = labelKeyOpt.map(k => messagesApi.preferred(request)(k)).getOrElse(value)

    val savedTry = persistSelection(userAnswers, parentKey, value, label)

    for {
      updatedAnswers <- Future.fromTry(savedTry)
      _              <- sessionRepository.set(updatedAnswers)
      result <- {
        val children = config.subcategoriesFor(country, parentKey, value)

        if (children.nonEmpty) {
          val maybeCall = routeToSubCategory(parentKey, value, mode)

          Future.successful(maybeCall.fold(Redirect(controllers.routes.InvoiceTypeController.onPageLoad(mode)): Result)(Redirect))

        } else {
          Future.successful(noChildrenRedirect(value, resolvedSlug, mode))
        }
      }
    } yield result
  }

  private def handleSubmitValue(value: String, parentKey: String, country: String, resolvedSlug: String, mode: Mode, userAnswers: UserAnswers)(
    implicit request: DataRequest[AnyContent]
  ): Future[Result] =
    if (mode == CheckMode && userAnswers.isAnswerUnchanged(PurchaseSubTypePage, value)) {
      Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
    } else {
      if (value == ConfigPurchaseMapping.NoneValue) {
        persistNoneSelection(mode, userAnswers)
      } else {
        persistNormalSelection(parentKey, country, value, resolvedSlug, mode, userAnswers)
      }
    }

  private def submitForResolvedParent(purchaseTypeSlug: String, parentKey: String, country: String, mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val (options, items, parentHeading, preparedForm, resolvedSlug, _) =
      prepareViewData(parentKey, country, purchaseTypeSlug, userAnswers, mode)(request)

    if (options.isEmpty) {
      redirectWhenNoOptions(mode)
    } else {
      preparedForm
        .bindFromRequest()
        .fold(
          formWithErrors => badSubmitRequest(formWithErrors, items, parentHeading, resolvedSlug, mode),
          value => handleSubmitValue(value, parentKey, country, resolvedSlug, mode, userAnswers)
        )
    }
  }

  def onSubmit(purchaseTypeSlug: String, mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      // Resolve context (parentKey + country) from slug/session
      resolveParentAndCountry(purchaseTypeSlug, request.userAnswers) match {
        case Some((parentKey, country)) =>
          submitForResolvedParent(
            purchaseTypeSlug,
            parentKey,
            country,
            mode,
            request.userAnswers
          )

        case None => Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
      }
    }
