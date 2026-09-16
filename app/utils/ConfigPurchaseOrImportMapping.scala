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

package utils

import javax.inject.Inject
import play.api.Configuration
import play.api.Environment
import scala.io.Source
import scala.jdk.CollectionConverters.*
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import scala.util.control.NonFatal

// Import commonly used types from the Typesafe config library so the code
// below can use short names instead of fully-qualified class names.
import com.typesafe.config.{ConfigObject, ConfigValueType}

case class PurchaseNode(parent: String, code: String, label: String, children: Seq[PurchaseNode] = Seq.empty)

/** `ConfigPurchaseOrImportMapping` loads a declarative purchase mapping from `application.conf` (under `purchase.mapping`) and exposes helpers used
  * by controllers and views to build radio items and lookup subcodes/subcategories.
  *
  * The mapping supports mixed arrays (plain strings and nested objects) and contains logic to normalise label keys that include numeric ordering
  * segments. The class is intentionally defensive: most parsing errors are swallowed and an empty mapping is returned so the application can fall
  * back to sensible defaults.
  */

object ConfigPurchaseOrImportMapping {
  val NoneValue: String = "__none__"
  val NoneOfTheseSubCode: String = "10.99"
}

class ConfigPurchaseOrImportMapping @Inject() (config: Configuration = Configuration.empty, env: Environment = Environment.simple()) {

  val prefix = "sub."

  private def normalizeLabel(label: String, code: String): String = {
    if !label.startsWith(prefix) || code.isEmpty then label
    else
      val parts = label.substring(prefix.length).split("\\.")
      val codeHead = code.split("\\.").headOption.getOrElse("")
      if parts.length >= 2 && parts(1) == codeHead then prefix + (parts.head +: parts.drop(2)).mkString(".")
      else label
  }

  private val mapping: Map[String, Seq[PurchaseNode]] =
    try {
      val rootConfig = config.underlying.getConfig("purchase-or-import-mapping")

      def parseEntry(entry: Any): PurchaseNode = entry match {
        case s: String =>
          val parts = s.split("\\|", 3)
          PurchaseNode(parts(0), parts(1), parts(2), Seq.empty)

        case c: Configuration =>
          val parent = c.getOptional[String]("parent").getOrElse("")
          val code = c.getOptional[String]("code").getOrElse("")
          val label = c.getOptional[String]("label").getOrElse("")

          val children: Seq[PurchaseNode] =
            try {
              val underlying = c.underlying
              val list = underlying.getList("subcodes")
              list.asScala.toSeq.map { v =>
                v.valueType() match {
                  case ConfigValueType.STRING =>
                    parseEntry(v.unwrapped().asInstanceOf[String])
                  case ConfigValueType.OBJECT =>
                    val obj = v.asInstanceOf[ConfigObject].toConfig
                    parseEntry(Configuration(obj))
                  case _ =>
                    PurchaseNode("", "", "", Seq.empty)
                }
              }
            } catch {
              case _: Throwable => Seq.empty
            }

          PurchaseNode(parent, code, label, children)

        case _ => PurchaseNode("", "", "", Seq.empty)
      }

      val countries: Seq[String] = rootConfig.root().keySet().asScala.toSeq
      val playRoot = Configuration(rootConfig)

      countries.map { key =>
        val seqAny: Seq[Any] =
          try {
            val list = rootConfig.getList(key)
            list.asScala.toSeq.map { v =>
              v.valueType() match {
                case ConfigValueType.STRING => v.unwrapped().asInstanceOf[String]
                case ConfigValueType.OBJECT =>
                  val obj = v.asInstanceOf[ConfigObject].toConfig
                  Configuration(obj)
                case _ => v.unwrapped()
              }
            }
          } catch {
            case _: Throwable =>
              try {
                playRoot.get[Seq[String]](key).map(_.asInstanceOf[Any])
              } catch {
                case _: Throwable =>
                  try {
                    playRoot.get[Seq[Configuration]](key).map(_.asInstanceOf[Seq[Any]])
                  } catch {
                    case _: Throwable => Seq.empty
                  }
              }
          }

        val flatNodes = seqAny.map(parseEntry)

        def normalizeLabelKey(label: String): String = {
          if (!label.startsWith(prefix)) return label
          val rest = label.substring(prefix.length)
          val parts = rest.split("\\.")
          if (parts.length >= 2 && parts(1).matches("\\d+")) {
            val newRest = parts.head +: parts.drop(2)
            prefix + newRest.mkString(".")
          } else {
            label
          }
        }

        val groupedByParent: Map[String, Seq[PurchaseNode]] = flatNodes.groupBy(_.parent)

        val nodes = groupedByParent.toSeq.flatMap { case (parentKey, nodesForParent) =>
          val baseKeys: Seq[String] = nodesForParent.map { n =>
            val parts = n.code.split("\\.")
            parts.take(2).mkString(".")
          }.distinct

          baseKeys.map { base =>
            val explicitLabelOpt = nodesForParent.find(_.code == base).map(_.label)
            val derivedLabel = explicitLabelOpt.orElse {
              nodesForParent.find(n => n.code.startsWith(base + ".")).flatMap { child =>
                val l = child.label
                if (l.startsWith("sub.")) {
                  val parts = l.split("\\.")
                  if (parts.length > 3) Some(parts.dropRight(1).mkString(".")) else None
                } else None
              }
            }
            val label = derivedLabel.getOrElse(base)
            val explicitNodeOpt = nodesForParent.find(_.code == base)
            val explicitChildren: Seq[PurchaseNode] = explicitNodeOpt.toSeq.flatMap(_.children)

            val siblingChildren: Seq[PurchaseNode] = nodesForParent
              .filter(n => n.code != base && n.code.startsWith(base + "."))
              .map(n => PurchaseNode(parentKey, n.code, n.label, Seq.empty))

            val children = (explicitChildren ++ siblingChildren).groupBy(_.code).map(_._2.head).toSeq.sortBy(_.code)

            PurchaseNode(parentKey, base, label, children)
          }
        }

        key -> nodes
      }.toMap
    } catch {
      case _: Throwable => Map.empty
    }

  def subcodesFor(country: String, parentKey: String): Seq[(String, String)] =
    nodesForCountry(country).toSeq.flatMap(_.filter(_.parent == parentKey).map(n => (n.code, n.label)))

  def subcodesFor(parentKey: String): Seq[(String, String)] =
    mapping.values.toSeq.flatten.filter(_.parent == parentKey).map(n => (n.code, n.label))

  def selectableSubcodes(country: String, parentKey: String): Option[Seq[(String, String)]] =
    Some(subcodesFor(country, parentKey)).filter { options =>
      options.nonEmpty && options.map(_._1) != Seq(ConfigPurchaseOrImportMapping.NoneOfTheseSubCode)
    }

  def subcategoriesFor(country: String, parentKey: String, subcode: String): Seq[(String, String)] =
    nodesForCountry(country).toSeq.flatMap(_.filter(n => n.parent == parentKey && n.code == subcode).flatMap(_.children).map(c => (c.code, c.label)))

  private def nodesForCountry(country: String): Option[Seq[PurchaseNode]] = {
    val key = country.trim
    val commaParts = key.split(",").map(_.trim).filter(_.nonEmpty)
    val spaceParts = key.split(" ").map(_.trim).filter(_.nonEmpty)

    val candidates = Seq(
      key,
      key.toUpperCase,
      key.trim,
      key.trim.toUpperCase
    ) ++
      commaParts.reverse ++ // prefer part after comma (often the code)
      commaParts.reverse.map(_.toUpperCase) ++
      spaceParts.reverse ++ // prefer last space-delimited token
      spaceParts.reverse.map(_.toUpperCase) ++
      (if (key.length >= 2) Seq(key.takeRight(2).toUpperCase) else Seq.empty)

    candidates.iterator.flatMap(c => mapping.get(c)).toSeq.headOption
  }

  def buildRadioItems(options: Seq[(String, String)], msgs: play.api.i18n.Messages): Seq[RadioItem] =
    options.zipWithIndex.map { case ((code, labelKey), idx) =>
      def stripLeadingNumeric(key: String): String = {
        val parts = key.split("\\.")
        if parts.length >= 4 && parts(0) == "purchase" && parts(1) == "sub"
        then (parts.take(3) ++ parts.drop(4)).mkString(".")
        else key
      }

      def normalizeLabelKey(label: String): String = {
        if (!label.startsWith(prefix)) return label
        val rest = label.substring(prefix.length)
        val parts = rest.split("\\.")
        if (parts.length >= 3 && parts(1).matches("\\d+")) {
          val newRest = parts.head +: parts.drop(2)
          prefix + newRest.mkString(".")
        } else label
      }

      val candidates = Seq(
        labelKey,
        normalizeLabel(labelKey, code),
        normalizeLabelKey(labelKey),
        stripLeadingNumeric(labelKey)
      ).distinct

      val lang = Option(msgs.lang.code).getOrElse("en")

      def loadPurchaseMessages(langCode: String): Map[String, String] = {
        val fileName: String = s"messages.purchaseOrImport.$langCode"

        try {
          env
            .resourceAsStream(fileName)
            .fold(Map.empty) { stream =>
              val src = Source.fromInputStream(stream, "UTF-8")
              try {
                src
                  .getLines()
                  .toSeq
                  .map(_.trim)
                  .filterNot(line => line.isEmpty && line.head != '#')
                  .flatMap { line =>
                    val idx = line.indexOf('=')
                    Some(line.substring(0, idx).trim -> line.substring(idx + 1).trim)
                  }
                  .toMap
              } finally src.close()
            }
        } catch {
          case NonFatal(_) => Map.empty
        }
      }

      val purchaseMap = loadPurchaseMessages(lang)

      val label = candidates
        .collectFirst {
          case k if msgs.isDefinedAt(k)     => msgs(k)
          case k if purchaseMap.contains(k) => purchaseMap(k)
        }
        .getOrElse(labelKey)
      RadioItem(
        content = Text(label),
        value   = Some(code),
        id      = Some(s"value_$idx")
      )
    } :+ RadioItem(
      content = Text("None"),
      value   = Some(ConfigPurchaseOrImportMapping.NoneValue),
      id      = Some(s"value_${options.size}")
    )
}
