package com.github.mdr.mash.evaluator

import com.github.mdr.mash.utils.Utils
import org.apache.commons.text.similarity.LevenshteinDistance

object Suggestor {

  def suggestionSuffix(possibleNames: Seq[String], requested: String): String =
    getSuggestion(possibleNames, requested).map(suggestion ⇒ s". Did you mean '$suggestion'?") getOrElse ""

  private def getSuggestion(possibleNames: Seq[String], requested: String): Option[String] =
    Utils.minBy(possibleNames, (possibleName: String) ⇒
      LevenshteinDistance.getDefaultInstance.apply(requested.toLowerCase, possibleName.toLowerCase))

}
