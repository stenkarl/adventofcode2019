package util

import scala.io.Source

object File {

  def fromFile(name:String): List[String] = {
    def text = Source.fromFile(name)

    def list = text.getLines().toList
    text.close()

    list
  }


}
