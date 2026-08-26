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
import controllers.routes
import forms.PurchaseSubTypeFormProvider
import models.requests.DataRequest
import models.{CheckMode, Mode, NormalMode, PurchaseSubCategoryType, PurchaseType, UserAnswers}
import navigation.Navigator
import pages.*
import play.api.data.Form
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.Logging
import play.api.mvc.*
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.{CheckModeShortCircuit, ConfigPurchaseMapping, ControllerHelpers, CountryCode, MountPrefix}
import views.html.PurchaseSubTypeView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PurchaseSubCategoryController @Inject() (
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
    with Logging {

  private case class SubCategoryViewData(
    resolvedParentCode: String,
    options: Seq[(String, String)],
    items: Seq[RadioItem],
    pageTitle: String,
    heading: String,
    preparedForm: Form[String],
    formAction: Call,
    backUrl: String,
    parentBase: String,
    childToPersist: String,
    parentLabelKeyOpt: Option[String]
  )

  val form: Form[String] = formProvider()

  private def stripLeadingNumeric(key: String): String = {
    // split the dotted key into parts
    val parts = key.split("\\.")
    // if the key looks like purchase.sub.X.Y.Z where an extra numeric
    // prefix was inserted, drop that segment for lookup
    if (parts.length >= 5 && parts.head == "purchase" && parts(1) == "sub") {
      (parts.take(3) ++ parts.drop(4)).mkString(".")
    } else {
      key
    }
  }

  private def titleForLabelKey(labelKey: String, msgs: Messages): Option[String] = {
    // build candidate message keys: the raw label key and a stripped variant
    val original = s"$labelKey.title"
    val stripped = s"${stripLeadingNumeric(labelKey)}.title"
    // return the first defined message for those candidate keys
    Seq(original, stripped).collectFirst { case k if msgs.isDefinedAt(k) => msgs(k) }
  }

  private def parentDerivedTitle(parentKey: String, resolvedParentCode: String, msgs: Messages): Option[String] = {
    // attempt several keys to derive a parent title based on different
    // granularity of the resolved code (full, drop leading segment, last segment)
    val asIs = s"purchase.sub.$parentKey.$resolvedParentCode.title"
    val dropLeading = {
      // if resolved code contains multiple segments, drop the first and try
      val parts = resolvedParentCode.split("\\.")
      if (parts.length > 1) s"purchase.sub.$parentKey.${parts.drop(1).mkString(".")}.title" else asIs
    }
    // lastSeg is just the final segment, used by some localized keys
    val lastSeg = resolvedParentCode.split("\\.").lastOption.map(s => s"purchase.sub.$parentKey.$s.title").getOrElse(asIs)
    // return the first key that exists in messages
    Seq(asIs, dropLeading, lastSeg).collectFirst { case k if msgs.isDefinedAt(k) => msgs(k) }
  }

  private def tryReverseParent(parentKey: String, candidate: String, mode: Mode)(implicit request: RequestHeader): Option[Call] = {
    try {
      val slug = PurchaseSubCategoryType.pathFor(parentKey, candidate)
      val prefix = utils.MountPrefix.getFromRequest
      val url = ControllerHelpers.pathForSlug(slug, mode, prefix)
      Some(Call("POST", url))
    } catch { case _: Throwable => None /* return None when slug computation fails */ }
  }

  private def computeFormAction(parentKey: String, candidates: Seq[String], userAnswers: UserAnswers, mode: Mode)(implicit
    request: RequestHeader
  ): Call = {
    // compute mount prefix and session slug candidate
    val prefix = utils.MountPrefix.getFromRequest
    val maybeSessionSlug = userAnswers.get(PurchaseTypePage).map(models.PurchaseType.urlSlugForPurchaseType)
    // try reversing using candidate codes first; if none succeed fall back
    // to a slug derived from the session PurchaseType or to root
    candidates.iterator
      .flatMap(c => tryReverseParent(parentKey, c, mode))
      .find(_ => true)
      .getOrElse(
        maybeSessionSlug
          .map { slug =>
            val url = ControllerHelpers.pathForSlug(slug, mode, prefix)
            Call("POST", url)
          }
          .getOrElse(Call("POST", if (prefix.isEmpty) s"/" else s"$prefix/"))
      )
  }

  private def backUrlFor(userAnswers: UserAnswers, mode: Mode)(implicit request: RequestHeader): String = {
    // compute the back URL that returns to the parent purchase type
    // when in CheckMode the back target should include the change-<prefix>
    val prefix = MountPrefix.getFromRequest
    userAnswers.get(PurchaseTypePage).map(pt => PurchaseType.urlSlugForPurchaseType(pt)) match {
      case Some(slug) =>
        val url = ControllerHelpers.pathForSlug(slug, mode, prefix)
        Call("GET", url).url
      case None =>
        // fallback to the top-level PurchaseType page in NormalMode
        controllers.routes.PurchaseTypeController.onPageLoad(NormalMode).url
    }
  }

  private def selectTitle(parentKey: String, resolvedParentCode: String, options: Seq[(String, String)], msgs: Messages): String = {
    val lastSeg = resolvedParentCode.split("\\.").lastOption.getOrElse(resolvedParentCode)
    val headSeg = resolvedParentCode.split("\\.").headOption.getOrElse(resolvedParentCode)

    val specificTitleKeys = Seq(
      s"purchase.sub.$parentKey.$lastSeg.title",
      s"purchase.sub.$parentKey.$resolvedParentCode.title",
      s"purchase.sub.$parentKey.$headSeg.title"
    )

    val childTitleOpt = specificTitleKeys
      .collectFirst { case k if msgs.isDefinedAt(k) => msgs(k) }
      .orElse(options.to(LazyList).flatMap { case (_, labelKey) => titleForLabelKey(labelKey, msgs) }.headOption)

    val parentHeading = msgs(s"purchase.sub.$parentKey.heading")
    childTitleOpt.orElse(parentDerivedTitle(parentKey, resolvedParentCode, msgs)).getOrElse(parentHeading)
  }

  private def requiredKeyFor(parentKey: String, resolvedParentCode: String, msgs: Messages): String = {
    val lastSeg = resolvedParentCode.split("\\.").lastOption.getOrElse(resolvedParentCode)
    val candidateKeys = Seq(
      s"purchase.sub.$parentKey.$lastSeg.error.required",
      s"purchase.sub.$parentKey.error.required"
    )
    candidateKeys.find(k => msgs.isDefinedAt(k)).getOrElse("error.required")
  }

  private def formActionCandidates(resolvedParentCode: String): Seq[String] = {
    val head = resolvedParentCode.split("\\.").headOption.getOrElse(resolvedParentCode)
    val last = resolvedParentCode.split("\\.").lastOption.getOrElse(resolvedParentCode)
    Seq(resolvedParentCode, last, head).distinct
  }

  private def childToPersistFor(resolvedParentCode: String, options: Seq[(String, String)]): String =
    if (resolvedParentCode.contains(".")) resolvedParentCode else options.headOption.map(_._1).getOrElse(resolvedParentCode)

  private def findByLastSegment(parentKey: String, seg: String, country: String): Option[String] =
    config.subcodesFor(country, parentKey).map(_._1).find(code => code.split("\\.").lastOption.contains(seg))

  private def computeResolvedParentAndOptions(parentKey: String,
                                              effectiveParentCode: String,
                                              parentCode: String,
                                              country: String
                                             ): (String, Seq[(String, String)]) = {
    // attempt to get options for the provided effective parent code
    val initialOptions = config.subcategoriesFor(country, parentKey, effectiveParentCode)
    if (initialOptions.nonEmpty) {
      (effectiveParentCode, initialOptions)
    } else {
      // if no options, try dropping the first segment of the effective code
      val alt = effectiveParentCode.split("\\.").drop(1).mkString(".")
      val altOptions = if (alt.nonEmpty) config.subcategoriesFor(country, parentKey, alt) else Seq.empty
      if (altOptions.nonEmpty) {
        (alt, altOptions)
      } else {
        // as a final fallback try to locate an option by matching the last segment
        findByLastSegment(parentKey, parentCode, country)
          .map(found => (found, config.subcategoriesFor(country, parentKey, found)))
          .getOrElse((parentCode, initialOptions))
      }
    }
  }

  private def prepareSubCategoryViewData(parentKey: String,
                                         parentCode: String,
                                         effectiveParentCode: String,
                                         country: String,
                                         userAnswers: UserAnswers,
                                         mode: Mode
                                        )(implicit request: RequestHeader): SubCategoryViewData = {
    val msgs = messagesApi.preferred(request)

    val (resolvedParentCode, options) = computeResolvedParentAndOptions(parentKey, effectiveParentCode, parentCode, country)
    val items = config.buildRadioItems(options, msgs)
    val heading = selectTitle(parentKey, resolvedParentCode, options, msgs)
    val pageTitle = heading

    val parentLabelKeyOpt = config.subcodesFor(country, parentKey).find(_._1 == resolvedParentCode).map(_._2)
    val requiredKey = requiredKeyFor(parentKey, resolvedParentCode, msgs)
    val preparedForm = userAnswers.get(PurchaseSubCategoryPage).fold(formProvider(requiredKey))(formProvider(requiredKey).fill)
    val candidates = formActionCandidates(resolvedParentCode)
    val formAction = computeFormAction(parentKey, candidates, userAnswers, mode)(request)
    val backUrl = backUrlFor(userAnswers, mode)
    val parentBase = resolvedParentCode.split("\\.").headOption.getOrElse(resolvedParentCode)
    val childToPersist = childToPersistFor(resolvedParentCode, options)

    SubCategoryViewData(
      resolvedParentCode,
      options,
      items,
      pageTitle,
      heading,
      preparedForm,
      formAction,
      backUrl,
      parentBase,
      childToPersist,
      parentLabelKeyOpt
    )
  }

  private def effectiveParentCodeFor(country: String, parentKey: String, userAnswers: UserAnswers): String =
    userAnswers.get(PurchaseSubTypePage).getOrElse {
      // fallback to the first configured subcode when session missing
      config.subcodesFor(country, parentKey).headOption.map(_._1).getOrElse("")
    }

  private def resolveParentAndCountry(userAnswers: UserAnswers): Option[(String, String)] = {
    val maybeParent = userAnswers.get(PurchaseTypePage).map(_.toString)
    val maybeCountry = CountryCode.findCountryCode(userAnswers)
    ControllerHelpers.bothDefined(maybeParent, maybeCountry)
  }

  private def renderSubCategoryView(data: SubCategoryViewData)(implicit request: DataRequest[AnyContent]): Future[Result] =
    Future.successful(Ok(view(data.preparedForm, data.items, data.pageTitle, data.heading, data.formAction, data.backUrl)))

  private def markArrivalAndRenderSubCategory(data: SubCategoryViewData, mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] =
    ControllerHelpers.markArrivalAndRender(
      PurchaseSubCategoryArrivedFromCheckYourAnswersPage,
      mode,
      userAnswers,
      sessionRepository
    )(_ => renderSubCategoryView(data))

  private def persistDefaultParentAndRender(data: SubCategoryViewData, mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val labelForParent = data.parentLabelKeyOpt.flatMap(k => Some(messagesApi.preferred(request)(k))).getOrElse(data.childToPersist)
    val saved = for {
      afterSetParent      <- userAnswers.set(PurchaseSubTypePage, data.childToPersist)
      afterSetParentLabel <- afterSetParent.set(PurchaseSubTypeLabelPage, labelForParent)
    } yield afterSetParentLabel

    for {
      updatedAnswers <- Future.fromTry(saved)
      _              <- sessionRepository.set(updatedAnswers)
      result         <-
        // When persisting a default parent in CheckMode, also mark that the user arrived from the sub-category
        // change flow so PurchaseTypeController can route back here when appropriate. Use the already-persisted `ua`
        // to avoid overwriting the saved default parent.
        ControllerHelpers.markArrivalAndRender(
          PurchaseSubCategoryArrivedFromCheckYourAnswersPage,
          mode,
          updatedAnswers,
          sessionRepository
        )(_ => renderSubCategoryView(data))
    } yield result
  }

  private def handleCountryChangedOnPageLoad(request: DataRequest[AnyContent]): Future[Result] = {
    val clearedAnswers = for {
      afterRemovedSubCategory      <- request.userAnswers.remove(PurchaseSubCategoryPage)
      afterRemovedSubCategoryLabel <- afterRemovedSubCategory.remove(PurchaseSubCategoryLabelPage)
      afterClearedFlag             <- afterRemovedSubCategoryLabel.remove(CountryChangedPage)
    } yield afterClearedFlag

    Future.fromTry(clearedAnswers).flatMap { updated =>
      sessionRepository.set(updated).map(_ => Redirect(Call("GET", request.path)))
    }
  }

  private def handleResolvedSubCategoryPageLoad(parentKey: String, country: String, mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val effectiveParentCode = effectiveParentCodeFor(country, parentKey, userAnswers)
    val data = prepareSubCategoryViewData(parentKey, effectiveParentCode, effectiveParentCode, country, userAnswers, mode)(request)

    renderOrPersistForResolvedSubCategory(data, mode, userAnswers)
  }

  private def renderOrPersistForResolvedSubCategory(data: SubCategoryViewData, mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] =
    if (data.options.isEmpty) {
      Future.successful(redirectAfterSubmit(mode))
    } else {
      userAnswers.get(PurchaseSubTypePage) match {
        case Some(existing) if existing.split("\\.").headOption.contains(data.parentBase) =>
          markArrivalAndRenderSubCategory(data, mode, userAnswers)
        case _ =>
          persistDefaultParentAndRender(data, mode, userAnswers)
      }
    }

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      // If the country has changed, clear dependent subcategory values
      if (request.userAnswers.get(CountryChangedPage).contains(true)) {
        handleCountryChangedOnPageLoad(request)

      } else {
        resolveParentAndCountry(request.userAnswers) match {
          case Some((parentKey, country)) =>
            handleResolvedSubCategoryPageLoad(parentKey, country, mode, request.userAnswers)
          case None =>
            // missing parent or country -> recover the journey
            Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
      }
    }

  private def redirectAfterSubmit(mode: Mode): Result =
    ControllerHelpers.redirectToInvoiceTypeOrCYA(mode)

  private def persistNoneSubCategorySelection(mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val noneLabel = ConfigPurchaseMapping.NoneValue
    val savedTry = for {
      a1 <- userAnswers.set(PurchaseSubCategoryPage, ConfigPurchaseMapping.NoneValue)
      a2 <- a1.set(PurchaseSubCategoryLabelPage, noneLabel)
    } yield a2

    for {
      updatedAnswers <- Future.fromTry(savedTry)
      _              <- sessionRepository.set(updatedAnswers)
    } yield redirectAfterSubmit(mode)
  }

  private def persistSelectedSubCategory(value: String, options: Seq[(String, String)], mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val labelKeyOpt = options.find(_._1 == value).map(_._2)
    val label = labelKeyOpt.map(k => messagesApi.preferred(request)(k)).getOrElse(value)

    val savedTry = for {
      afterSet      <- userAnswers.set(PurchaseSubCategoryPage, value)
      afterSetLabel <- afterSet.set(PurchaseSubCategoryLabelPage, label)
    } yield afterSetLabel

    for {
      updatedAnswers <- Future.fromTry(savedTry)
      _              <- sessionRepository.set(updatedAnswers)
    } yield redirectAfterSubmit(mode)
  }

  private def handleSubmitValue(value: String, options: Seq[(String, String)], mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] =
    if (mode == CheckMode && userAnswers.isAnswerUnchanged(PurchaseSubCategoryPage, value)) {
      Future.successful(Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad()))
    } else {
      if (value == ConfigPurchaseMapping.NoneValue) {
        persistNoneSubCategorySelection(mode, userAnswers)
      } else {
        persistSelectedSubCategory(value, options, mode, userAnswers)
      }
    }

  private def handleSubmitWithResolvedContext(parentKey: String, country: String, mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val effectiveParentCode = effectiveParentCodeFor(country, parentKey, userAnswers)
    val data = prepareSubCategoryViewData(parentKey, effectiveParentCode, effectiveParentCode, country, userAnswers, mode)(request)

    if (data.options.isEmpty) {
      Future.successful(redirectAfterSubmit(mode))
    } else {
      data.preparedForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(BadRequest(view(formWithErrors, data.items, data.pageTitle, data.heading, data.formAction, data.backUrl))),
          value => handleSubmitValue(value, data.options, mode, userAnswers)
        )
    }
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    resolveParentAndCountry(request.userAnswers) match {
      case Some((parentKey, country)) =>
        handleSubmitWithResolvedContext(parentKey, country, mode, request.userAnswers)
      case None =>
        // missing context -> recover the journey
        Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
    }
  }

}
