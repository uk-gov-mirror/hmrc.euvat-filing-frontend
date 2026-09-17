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
import forms.PurchaseOrImportSubTypeFormProvider
import models.requests.DataRequest
import models.{CheckMode, Mode, NormalMode, PurchaseOrImportSubCategoryType, PurchaseOrImportType, UserAnswers}
import navigation.Navigator
import pages.*
import play.api.Logging
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.*
import repositories.SessionRepository
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.PurchaseOrImportHelpers.*
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
  formProvider: PurchaseOrImportSubTypeFormProvider,
  config: ConfigPurchaseOrImportMapping,
  val controllerComponents: MessagesControllerComponents,
  view: PurchaseOrImportSubTypeView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private case class SubCategoryViewData(
    options: Seq[(String, String)],
    items: Seq[RadioItem],
    title: String,
    form: Form[String],
    formAction: Call,
    backUrl: String,
    parentBase: String,
    childToPersist: String,
    parentLabelKeyOpt: Option[String]
  )

  private def tryReverseParent(parentKey: String, candidate: String, mode: Mode)(implicit request: RequestHeader): Option[Call] =
    try {
      val slug = PurchaseOrImportSubCategoryType.pathFor(parentKey, candidate)
      val url = ControllerHelpers.pathForSlug(slug, mode, MountPrefix.getFromRequest)
      Some(Call("POST", url))
    } catch { case _: Throwable => None }

  private def computeFormAction(parentKey: String, candidates: Seq[String], userAnswers: UserAnswers, mode: Mode)(implicit
    request: RequestHeader
  ): Call = {
    val prefix = MountPrefix.getFromRequest
    val maybeSessionSlug = userAnswers.get(PurchaseTypePage).map(PurchaseOrImportType.urlSlugForPurchaseType)
    candidates.iterator
      .flatMap(c => tryReverseParent(parentKey, c, mode))
      .find(_ => true)
      .getOrElse(
        maybeSessionSlug
          .map(slug => Call("POST", ControllerHelpers.pathForSlug(slug, mode, prefix)))
          .getOrElse(Call("POST", if (prefix.isEmpty) "/" else s"$prefix/"))
      )
  }

  private def backUrlFor(userAnswers: UserAnswers, mode: Mode)(implicit request: RequestHeader): String =
    userAnswers.get(PurchaseTypePage).map(PurchaseOrImportType.urlSlugForPurchaseType) match {
      case Some(_) if mode == CheckMode => routes.CheckYourPurchaseDetailsController.onPageLoad().url
      case Some(slug)                   => ControllerHelpers.pathForSlug(slug, mode, MountPrefix.getFromRequest)
      case None                         => routes.PurchaseTypeController.onPageLoad(NormalMode).url
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

  private def computeResolvedParentAndOptions(parentKey: String, parentCode: String, country: String): (String, Seq[(String, String)]) = {
    val initialOptions = config.subcategoriesFor(country, parentKey, parentCode)
    if (initialOptions.nonEmpty) {
      (parentCode, initialOptions)
    } else {
      val alt = parentCode.split("\\.").drop(1).mkString(".")
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

  private def effectiveParentCodeFor(country: String, parentKey: String, userAnswers: UserAnswers): String =
    userAnswers.get(PurchaseSubTypePage).getOrElse {
      config.subcodesFor(country, parentKey).headOption.map(_._1).getOrElse("")
    }

  private def prepareSubCategoryViewData(parentKey: String, country: String, userAnswers: UserAnswers, mode: Mode)(implicit
    request: RequestHeader
  ): SubCategoryViewData = {
    val msgs = messagesApi.preferred(request)
    val effectiveParentCode = effectiveParentCodeFor(country, parentKey, userAnswers)
    val (resolvedParentCode, options) = computeResolvedParentAndOptions(parentKey, effectiveParentCode, country)
    val requiredKey = requiredErrorKey(parentKey, Some(resolvedParentCode))(msgs)

    SubCategoryViewData(
      options           = options,
      items             = radioItems(config, options)(msgs),
      title             = subCategoryTitle(parentKey, resolvedParentCode, options)(msgs),
      form              = preparedForm(formProvider, requiredKey, userAnswers.get(PurchaseSubCategoryPage)),
      formAction        = computeFormAction(parentKey, formActionCandidates(resolvedParentCode), userAnswers, mode),
      backUrl           = backUrlFor(userAnswers, mode),
      parentBase        = resolvedParentCode.split("\\.").headOption.getOrElse(resolvedParentCode),
      childToPersist    = childToPersistFor(resolvedParentCode, options),
      parentLabelKeyOpt = config.subcodesFor(country, parentKey).find(_._1 == resolvedParentCode).map(_._2)
    )
  }

  private def withParentAndCountry(block: (String, String) => Future[Result])(implicit request: DataRequest[AnyContent]): Future[Result] = {
    val maybeParent = request.userAnswers.get(PurchaseTypePage).map(_.toString)
    val maybeCountry = CountryCode.findCountryCode(request.userAnswers)
    ControllerHelpers.bothDefined(maybeParent, maybeCountry) match {
      case Some((parentKey, country)) => block(parentKey, country)
      case None                       => Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
    }
  }

  private def renderView(data: SubCategoryViewData, form: Form[String])(implicit request: DataRequest[AnyContent]) =
    view(form, data.items, data.title, data.title, "purchase.caption", data.formAction, data.backUrl)

  private def markArrivalAndRender(data: SubCategoryViewData, mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] =
    ControllerHelpers.markArrivalAndRender(
      PurchaseSubCategoryArrivedFromCheckYourAnswersPage,
      mode,
      userAnswers,
      sessionRepository
    )(_ => Future.successful(Ok(renderView(data, data.form))))

  private def persistDefaultParentAndRender(data: SubCategoryViewData, mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] = {
    val label = data.parentLabelKeyOpt.map(key => messagesApi.preferred(request)(key)).getOrElse(data.childToPersist)
    for {
      updatedAnswers <- Future.fromTry(setSelection(userAnswers, PurchaseSubTypePage, PurchaseSubTypeLabelPage, data.childToPersist, label))
      _              <- sessionRepository.set(updatedAnswers)
      result         <- markArrivalAndRender(data, mode, updatedAnswers)
    } yield result
  }

  private def renderOrPersistParent(data: SubCategoryViewData, mode: Mode, userAnswers: UserAnswers)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] =
    if (data.options.isEmpty) {
      Future.successful(ControllerHelpers.redirectToInvoiceTypeOrCYA(mode))
    } else {
      userAnswers.get(PurchaseSubTypePage) match {
        case Some(existing) if existing.split("\\.").headOption.contains(data.parentBase) => markArrivalAndRender(data, mode, userAnswers)
        case _                                                                            => persistDefaultParentAndRender(data, mode, userAnswers)
      }
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

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    if (request.userAnswers.get(CountryChangedPage).contains(true)) {
      handleCountryChangedOnPageLoad(request)
    } else {
      withParentAndCountry { (parentKey, country) =>
        val data = prepareSubCategoryViewData(parentKey, country, request.userAnswers, mode)
        renderOrPersistParent(data, mode, request.userAnswers)
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
      val label = labelFor(value, options)
      for {
        updatedAnswers <- Future.fromTry(setSelection(userAnswers, PurchaseSubCategoryPage, PurchaseSubCategoryLabelPage, value, label))
        _              <- sessionRepository.set(updatedAnswers)
      } yield ControllerHelpers.redirectToInvoiceTypeOrCYA(mode)
      if (value == ConfigPurchaseOrImportMapping.NoneValue) {
        persistNoneSubCategorySelection(mode, userAnswers)
      } else {
        persistSelectedSubCategory(value, options, mode, userAnswers)
      }
    }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    withParentAndCountry { (parentKey, country) =>
      val data = prepareSubCategoryViewData(parentKey, country, request.userAnswers, mode)

      if (data.options.isEmpty) {
        Future.successful(ControllerHelpers.redirectToInvoiceTypeOrCYA(mode))
      } else {
        data.form
          .bindFromRequest()
          .fold(
            formWithErrors => Future.successful(BadRequest(renderView(data, formWithErrors))),
            value => handleSubmitValue(value, data.options, mode, request.userAnswers)
          )
      }
    }
  }
}
