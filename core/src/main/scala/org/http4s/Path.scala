/*
 * Derived from Twitter Finagle.
 *
 * Original source:
 * https://github.com/twitter/finagle/blob/6e2462acc32ac753bf4e9d8e672f9f361be6b2da/finagle-http/src/main/scala/com/twitter/finagle/http/path/Path.scala
 */

package org.http4s

import org.http4s.util._

import scala.collection.immutable.BitSet
import scalaz.ValidationNel
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

/** Represents the path segment of a URI */
abstract class Path extends Renderable {
  def /(child: String) = new /(this, child)
  def toList: List[String]
  def parent: Path
  def lastOption: Option[String]
  def startsWith(other: Path): Boolean
  def asUrlEncodedString: String

  def render(writer: Writer): writer.type =
    writer.append(asUrlEncodedString)
}

object Path {
  def fromEncoded(str: String): Path =
    if (str == "" || str == "/")
      Root
    else if (!str.startsWith("/"))
      Path("/" + str)
    else {
      val slash = str.lastIndexOf('/')
      val prefix = Path.fromEncoded(str.substring(0, slash))
      prefix / UrlCodingUtils.urlDecode(str.substring(slash + 1))
    }

  def apply(first: String, rest: String*): Path =
    rest.foldLeft(Root / first)(_ / _)

  def apply(list: List[String]): Path = list.foldLeft(Root: Path)(_ / _)

  def unapplySeq(path: Path): Option[List[String]] = Some(path.toList)

  def unapplySeq(request: Request): Option[List[String]] = Some(Path.fromEncoded(request.pathInfo).toList)

  val pathUnreserved = UrlFormCodec.urlUnreserved ++ BitSet(":@!$&'()*+,;=".toList.map(_.toInt): _*)
}

case class /(parent: Path, child: String) extends Path {
  lazy val toList: List[String] = parent.toList ++ List(child)
  def lastOption: Option[String] = Some(child)
  def asUrlEncodedString = parent.toString + "/" + UrlCodingUtils.urlEncode(child, toSkip = Path.pathUnreserved)
  override def toString = asUrlEncodedString
  def startsWith(other: Path) = {
    val components = other.toList
    (toList take components.length) == components
  }
}



/**
 * Root extractor:
 * {{{
 *   Path("/") match {
 *     case Root => ...
 *   }
 * }}}
 */
case object Root extends Path {
  def toList: List[String] = Nil
  def parent = this
  def lastOption: Option[String] = None
  def asUrlEncodedString: String = "/"
  override def toString = asUrlEncodedString
  def startsWith(other: Path) = other == Root
}

/**
 * Path separator extractor:
 * {{{
 *   Path("/1/2/3/test.json") match {
 *     case "1" /: "2" /: _ =>  ...
 * }}}
 */
object /: {
  def unapply(path: Path): Option[(String, Path)] = {
    path.toList match {
      case Nil => None
      case head :: tail => Some((head, Path(tail)))
    }
  }
}
