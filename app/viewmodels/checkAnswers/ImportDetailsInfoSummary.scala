package viewmodels.checkAnswers

import controllers.routes
import models.{CheckMode, UserAnswers}
import pages.ImportDetailsInfoPage
import play.api.i18n.Messages
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import viewmodels.govuk.summarylist._
import viewmodels.implicits._

object ImportDetailsInfoSummary  {

  def row(answers: UserAnswers)(implicit messages: Messages): Option[SummaryListRow] =
    answers.get(ImportDetailsInfoPage).map {
      answer =>

        SummaryListRowViewModel(
          key     = "importDetailsInfo.checkYourAnswersLabel",
          value   = ValueViewModel(HtmlFormat.escape(answer).toString),
          actions = Seq(
            ActionItemViewModel("site.change", routes.ImportDetailsInfoController.onPageLoad(CheckMode).url)
              .withVisuallyHiddenText(messages("importDetailsInfo.change.hidden"))
          )
        )
    }
}
