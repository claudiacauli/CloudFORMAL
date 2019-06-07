package aws.cfn.shared

import java.io.{File, FileNotFoundException}
import argonaut.{Json, Parse}
import scala.io.Source

object ParseUtils {

  def jsonFromFilePath(fp : String): Option[Json] = jsonFromFile( new File(fp) )

  def jsonFromFile(f : File): Option[Json] = {
     try {
       Parse.parseOption( Source.fromFile(f).mkString ) match {
         case None =>  None
         case x => x
       }
     } catch {
       case _: FileNotFoundException => None
     }

  }

}
