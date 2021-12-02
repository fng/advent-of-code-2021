package com.github.fng.adventofcode2021

import com.github.fng.adventofcode2021.day01.Day01.getClass

import scala.io.BufferedSource

object ResourceUtils {

  def getLinesFromResource(resource: String): List[String] = {
    val source: BufferedSource =
      io.Source.fromURL(getClass.getClassLoader.getResource(resource))
    try {
      source.getLines().toList
    } finally {
      source.close()
    }
  }

}
