package com.github.doohochang.csv

import scala.annotation.tailrec

class CSV(fieldDelimiter: Char, recordDelimiter: Char, trimSpaces: Boolean) {
  import CSV._

  /**
    * Reads list of records from the given string.
    * Records are separated by record delimiter, and a record is a list of fields.
    * Fields are strings of each record separated by field delimiter.
    */
  def read(string: String): Either[Failure, List[List[String]]] = {
    @tailrec
    def readRecords(string: String, acc: List[List[String]]): Either[Failure, List[List[String]]] = {
      if (string.isEmpty)
        Right(acc.reverse)
      else
        splitFirstRecord(string) match {
          case Right((record, remaining)) =>
            readRecords(remaining, record :: acc)
          case Left(failure) =>
            Left(failure)
        }
    }

    readRecords(string, Nil)
  }

  private def splitFirstRecord(string: String): Either[Failure, (List[String], String)] = {
    @tailrec
    def split(string: String, acc: List[String]): Either[Failure, (List[String], String)] = {
      if (string.isEmpty)
        Right(acc.reverse, "")
      else if (string.charAt(0) == recordDelimiter)
        Right(acc.reverse, string.drop(1))
      else
        splitFirstField(string) match {
          case Right((field, remaining)) =>
            split(remaining, field :: acc)
          case Left(failure) =>
            Left(failure)
        }
    }

    split(string, Nil)
  }

  /**
    * Splits first field from the given string.
    * @return (string of the first field, remaining string)
    */
  private def splitFirstField(string: String): Either[Failure, (String, String)] = {
    val trimmedString = trimIfNeeded(string)

    if (trimmedString.isEmpty)
      Right(("", ""))
    else if (trimmedString.charAt(0) == quote)
      splitFirstQuotedField(trimmedString)
    else
      Right(splitFirstNonQuotedField(trimmedString))
  }

  private def splitFirstQuotedField(string: String): Either[Failure, (String, String)] = {
    val length = string.length
    var index = 1

    def isFinishingQuote(index: Int): Boolean =
      string.charAt(index) == quote && (index == length - 1 || string.charAt(index + 1) != quote)

    def isNotFinished(index: Int): Boolean =
      index < length && !isFinishingQuote(index)

    def isLiteralQuote(index: Int): Boolean =
      index < length - 1 && string.charAt(index) == quote && string.charAt(index + 1) == quote

    while (isNotFinished(index)) {
      if (isLiteralQuote(index))
        index = index + 2
      else
        index = index + 1
    }

    if (index >= length) // If there's no finishing quote
      Left(NoFinishingQuote(string))
    else {
      val rawFieldString = string.substring(1, index)
      val fieldString = rawFieldString.replace(s"$quote$quote",s"$quote")

      val remaining = trimIfNeeded(string.drop(index + 1))

      if(remaining.isEmpty)
        Right((fieldString, ""))
      else if (remaining.charAt(0) == fieldDelimiter)
        Right((fieldString, remaining.drop(1)))
      else if (remaining.charAt(0) == recordDelimiter)
        Right((fieldString, remaining))
      else
        Left(WrongQuotedForm(string))
    }
  }

  private def splitFirstNonQuotedField(string: String): (String, String) = {
    val splitIndex = string.indexWhere(isDelimiter)

    if (splitIndex < 0)
      (trimIfNeeded(string), "")
    else
      (
        trimIfNeeded(string.take(splitIndex)),
        trimIfNeeded(
          if (string.charAt(splitIndex) == fieldDelimiter)
            string.drop(splitIndex + 1)
          else
            string.drop(splitIndex)
        )
      )
  }

  private def trimIfNeeded(string: String): String =
    if (trimSpaces) trim(string)
    else string

  private def trim(string: String): String = {
    var len = string.length
    var st = 0

    while (st < len && string.charAt(st).isWhitespace && !isDelimiter(string.charAt(st))) {
      st = st + 1
    }

    while (len > st && string.charAt(len - 1).isWhitespace && !isDelimiter(string.charAt(len - 1))) {
      len = len - 1
    }

    if(st >= len) ""
    else string.substring(st, len)
  }

  private def isDelimiter(char: Char): Boolean =
    char == fieldDelimiter || char == recordDelimiter
}

object CSV {
  def apply(
    fieldDelimiter: Char = ',',
    recordDelimiter: Char = '\n',
    trimSpaces: Boolean
  ): CSV =
    new CSV(fieldDelimiter, recordDelimiter, trimSpaces)

  val quote: Char = '\"'

  sealed trait Failure

  case class NoFinishingQuote(string: String) extends Failure
  case class WrongQuotedForm(string: String) extends Failure
}
