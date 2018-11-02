package com.github.doohochang.csv

trait Read[T] {
  def read(source: T, fieldDelimiter: Char, recordDelimiter: Char, trimSpaces: Boolean): List[List[String]]
}
