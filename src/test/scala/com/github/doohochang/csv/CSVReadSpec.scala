package com.github.doohochang.csv

import org.scalatest._

import CSV._

class CSVReadSpec extends FlatSpec with Matchers {
  "CSV" should "read well-formed strings correctly where trimSpaces mode is true" in {
    val csv = CSV(trimSpaces = true)

    csv.read("1997,Ford,E350") should be (Right(
      List(List("1997", "Ford", "E350"))
    ))

    csv.read("19 97,F  ord,E350") should be (Right(
      List(List("19 97", "F  ord", "E350"))
    ))

    csv.read("  1997 , For d,E 350   ") should be (Right(
      List(List("1997", "For d", "E 350"))
    ))

    csv.read("\"1997\",\"Ford\",\"E350\"") should be (Right(
      List(List("1997", "Ford", "E350"))
    ))

    csv.read("  \"1997\" ,  \"Ford\",  \"E350\"   ") should be (Right(
      List(List("1997", "Ford", "E350"))
    ))

    csv.read("1997,Ford,E350,\"Super, luxurious truck\"") should be (Right(
      List(List("1997", "Ford", "E350", "Super, luxurious truck"))
    ))

    csv.read("1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"") should be (Right(
      List(List("1997", "Ford", "E350", "Super, \"luxurious\" truck"))
    ))

    csv.read("1997,Ford,E350,\"Go get one now\nthey are going fast\"") should be (Right(
      List(List("1997", "Ford", "E350", "Go get one now\nthey are going fast"))
    ))

    csv.read("19 97,F  ord\nE350, \" is expensive,,\"\"! \"  ") should be (Right(
      List(List("19 97", "F  ord"), List("E350", " is expensive,,\"! "))
    ))

    csv.read("19 97,  \"F  ord\"  \n   E350 \n  \" is expensive,,\"\"! \"  ") should be (Right(
      List(List("19 97", "F  ord"), List("E350"), List(" is expensive,,\"! "))
    ))
  }

  "CSV" should "report malformed strings correctly where trimSpaces mode is true" in {
    val csv = CSV(trimSpaces = true)

    csv.read("\"1997,Ford,E350") should matchPattern {
      case Left(NoFinishingQuote(_)) =>
    }

    csv.read("  \"1997\" ,  \"Ford\"\",  \"E350\"   ") should matchPattern {
      case Left(_) =>
    }

    csv.read("19 97,F  ord\nE350, \" is expensive,,\"\"! \" asdf  ") should matchPattern {
      case Left(WrongQuotedForm(_)) =>
    }
  }

  "CSV" should "read well-formed strings correctly where trimSpaces mode is false" in {
    val csv = CSV(trimSpaces = false)

    csv.read("1997,Ford,E350") should be (Right(
      List(List("1997", "Ford", "E350"))
    ))

    csv.read("19 97,F  ord,E350") should be (Right(
      List(List("19 97", "F  ord", "E350"))
    ))

    csv.read("  1997 , For d,E 350   ") should be (Right(
      List(List("  1997 ", " For d", "E 350   "))
    ))

    csv.read("\"1997\",\"Ford\",\"E350\"") should be (Right(
      List(List("1997", "Ford", "E350"))
    ))

    csv.read("\" 1997\",\"Fo rd, \",\"  E350\"") should be (Right(
      List(List(" 1997", "Fo rd, ", "  E350"))
    ))

    csv.read("1997 , Ford,E350,\"Super, luxurious truck\"") should be (Right(
      List(List("1997 ", " Ford", "E350", "Super, luxurious truck"))
    ))

    csv.read("1997 , Ford, E350,\"Super, \"\"luxurious\"\" truck\"") should be (Right(
      List(List("1997 ", " Ford", " E350", "Super, \"luxurious\" truck"))
    ))

    csv.read("1997,Ford,E350,\"Go get one now\nthey are going fast\"") should be (Right(
      List(List("1997", "Ford", "E350", "Go get one now\nthey are going fast"))
    ))

    csv.read("19 97  , F  ord\n E350  ,\" is expensive,,\"\"! \"") should be (Right(
      List(List("19 97  ", " F  ord"), List(" E350  ", " is expensive,,\"! "))
    ))

    csv.read("19 97 ,\"F  ord\"\n   E350 \n\" is expensive,,\"\"! \"") should be (Right(
      List(List("19 97 ", "F  ord"), List("   E350 "), List(" is expensive,,\"! "))
    ))
  }

  "CSV" should "report malformed strings correctly where trimSpaces mode is false" in {
    val csv = CSV(trimSpaces = false)

    csv.read("\"1997,Ford,E350") should matchPattern {
      case Left(NoFinishingQuote(_)) =>
    }

    csv.read("1997,\"Ford\" ,E350") should matchPattern {
      case Left(WrongQuotedForm(_)) =>
    }

    csv.read("1997 , Ford, E350,\"Super, \"\"luxurious\"\" truck\" ") should matchPattern {
      case Left(WrongQuotedForm(_)) =>
    }

    csv.read("1997 , Ford, E350,\"Super, \"\"luxurious\"\" truck\"a") should matchPattern {
      case Left(WrongQuotedForm(_)) =>
    }
  }
}
