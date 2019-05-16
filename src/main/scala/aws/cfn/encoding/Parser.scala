package aws.cfn.encoding

import java.io.File

import argonaut.{Json, Parse}

import scala.io.Source

object Parser {
  def jsonFromFilePath(fp : String): Option[Json] = jsonFromFile( new File(fp) )

  def jsonFromFile(f : File): Option[Json] = Parse.parseOption( Source.fromFile(f).mkString ) match {
    case None =>  None
    case x => x
  }

}
