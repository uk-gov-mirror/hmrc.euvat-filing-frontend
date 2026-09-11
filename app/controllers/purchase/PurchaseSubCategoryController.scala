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
import forms.purchase.PurchaseSubTypeFormProvider
import models.requests.DataRequest
import models.{CheckMode, Mode, NormalMode, PurchaseOrImportType, PurchaseOrImportSubCategoryType, UserAnswers}
import navigation.Navigator
import pages.*
import play.api.Logging
import play.api.data.Form
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.*
import repositories.SessionRepository
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.{ConfigPurchaseOrImportMapping, ControllerHelpers, CountryCode, MountPrefix}
import views.html.PurchaseOrImportSubTypeView

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
  config: ConfigPurchaseOrImportMapping,
  val controllerComponents: MessagesControllerComponents,
  view: PurchaseOrImportSubTypeView
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
    val parts = key.split("\\.")
    if (parts.length >= 5 && parts.head == "purchase" && parts(1) == "sub") {
      (parts.take(3) ++ parts.drop(4)).mkString(".")
    } else {
      key
    }
  }

  private def titleForLabelKey(labelKey: String, msgs: Messages): Option[String] = {
    val original = s"$labelKey.title"
    val stripped = s"${stripLeadingNumeric(labelKey)}.title"
    Seq(original, stripped).collectFirst { case k if msgs.isDefinedAt(k) => msgs(k) }
  }

  private def parentDerivedTitle(parentKey: String, resolvedParentCode: String, msgs: Messages): Option[String] = {
    val asIs = s"purchase.sub.$parentKey.$resolvedParentCode.title"
    val dropLeading = {
      val parts = resolvedParentCode.split("\\.")
      if (parts.length > 1) s"purchase.sub.$parentKey.${parts.drop(1).mkString(".")}.title" else asIs
    }
    val lastSeg = resolvedParentCode.split("\\.").lastOption.map(s => s"purchase.sub.$parentKey.$s.title").getOrElse(asIs)
    Seq(asIs, dropLeading, lastSeg).collectFirst { case k if msgs.isDefinedAt(k) => msgs(k) }
  }

  private def tryReverseParent(parentKey: String, candidate: String, mode: Mode)(implicit request: RequestHeader): Option[Call] = {
    try {
      val slug = PurchaseOrImportSubCategoryType.pathFor(parentKey, candidate)
      val prefix = utils.MountPrefix.getFromRequest
      val url = ControllerHelpers.pathForSlug(slug, mode, prefix)
      Some(Call("POST", url))
    } catch { case _: Throwable => None }
  }

  private def computeFormAction(parentKey: String, candidates: Seq[String], userAnswers: UserAnswers, mode: Mode)(implicit
    request: RequestHeader
  ): Call = {
    val prefix = utils.MountPrefix.getFromRequest
    val maybeSessionSlug = userAnswers.get(PurchaseTypePage).map(models.PurchaseOrImportType.urlSlugForPurchaseType)
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
    val prefix = MountPrefix.getFromRequest
    userAnswers.get(PurchaseTypePage).map(pt => PurchaseOrImportType.urlSlugForPurchaseType(pt)) match {
      case Some(slug) =>
        if (mode == CheckMode) {
          routes.CheckYourPurchaseDetailsController.onPageLoad().url
        } else {
          val url = ControllerHelpers.pathForSlug(slug, mode, prefix)
          Call("GET", url).url
        }
      case None => routes.PurchaseTypeController.onPageLoad(NormalMode).url
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
    val initialOptions = config.subcategoriesFor(country, parentKey, effectiveParentCode)
    if (initialOptions.nonEmpty) {
      (effectiveParentCode, initialOptions)
    } else {
      val alt = effectiveParentCode.split("\\.").drop(1).mkString(".")
      val altOptions = if (alt.nonEmpty) config.subcategoriesFor(country, parentKey, alt) else Seq.empty
      if (altOptions.nonEmpty) {
        (alt, altOptions)
      } else {
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
      config.subcodesFor(country, parentKey).headOption.map(_._1).getOrElse("")
    }

  private def resolveParentAndCountry(userAnswers: UserAnswers): Option[(String, String)] = {
    val maybeParent = userAnswers.get(PurchaseTypePage).map(_.toString)
    val maybeCountry = CountryCode.findCountryCode(userAnswers)
    ControllerHelpers.bothDefined(maybeParent, maybeCountry)
  }

  private def renderSubCategoryView(data: SubCategoryViewData)(implicit request: DataRequest[AnyContent]): Future[Result] =
    Future.successful(Ok(view(data.preparedForm, data.items, data.pageTitle, data.heading, "purchase.caption", data.formAction, data.backUrl)))

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
      result <-
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
      if (request.userAnswers.get(CountryChangedPage).contains(true)) {
        handleCountryChangedOnPageLoad(request)
      } else {
        resolveParentAndCountry(request.userAnswers) match {
          case Some((parentKey, country)) =>
            handleResolvedSubCategoryPageLoad(parentKey, country, mode, request.userAnswers)
          case None =>
            Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
      }
    }

  private def redirectAfterSubmit(mode: Mode): Result = ControllerHelpers.redirectToInvoiceTypeOrCYA(mode)

  private def persistNoneSubCategorySelection(mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val noneLabel = ConfigPurchaseOrImportMapping.NoneValue
    val savedTry = for {
      a1 <- userAnswers.set(PurchaseSubCategoryPage, ConfigPurchaseOrImportMapping.NoneValue)
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
      Future.successful(Redirect(routes.CheckYourPurchaseDetailsController.onPageLoad()))
    } else {
      if (value == ConfigPurchaseOrImportMapping.NoneValue) {
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
            Future.successful(
              BadRequest(view(formWithErrors, data.items, data.pageTitle, data.heading, "purchase.caption", data.formAction, data.backUrl))
            ),
          value => handleSubmitValue(value, data.options, mode, userAnswers)
        )
    }
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    resolveParentAndCountry(request.userAnswers) match {
      case Some((parentKey, country)) =>
        handleSubmitWithResolvedContext(parentKey, country, mode, request.userAnswers)
      case None =>
        Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
    }
  }

}
