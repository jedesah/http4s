package org.http4s
package dsl

import scalaz._

object UriExtractors {
  object :? {
    def unapply(req: Request): Option[(Request, Map[String, Seq[String]])] = {
      Some((req, req.multiParams))
    }
  }

  /** File extension extractor */
  object ~ {
    /**
      * File extension extractor for Path:
      *   Path("example.json") match {
      *     case Root / "example" ~ "json" => ...
      */
    def unapply(path: Path): Option[(Path, String)] = {
      path match {
        case Root => None
        case parent / last =>
          unapply(last) map {
            case (base, ext) => (parent / base, ext)
          }
      }
    }

    /**
      * File extension matcher for String:
      * {{{
      *   "example.json" match {
      *      case => "example" ~ "json" => ...
      * }}}
      */
    def unapply(fileName: String): Option[(String, String)] = {
      fileName.lastIndexOf('.') match {
        case -1 => Some((fileName, ""))
        case index => Some((fileName.substring(0, index), fileName.substring(index + 1)))
      }
    }
  }

  object -> {
    /**
      * HttpMethod extractor:
      * {{{
      *   (request.method, Path(request.path)) match {
      *     case Method.GET -> Root / "test.json" => ...
      * }}}
      */
    def unapply(req: Request): Option[(Method, Path)] = {
      Some((req.method, Path(req.pathInfo)))
    }
  }

  class MethodConcat(val methods: Set[Method]) {
    /**
      * HttpMethod 'or' extractor:
      * {{{
      *  val request: Request = ???
      *  request match {
      *    case (Method.GET | Method.POST) -> Root / "123" => ???
      *  }
      * }}}
      */
    def unapply(method: Method): Option[Method] =
      if (methods(method)) Some(method) else None
  }

  // Base class for Integer and Long path variable extractors.
  protected class NumericPathVar[A <: AnyVal](cast: String => A) {
    def unapply(str: String): Option[A] = {
      if (!str.isEmpty && str.forall(Character.isDigit))
        try {
          Some(cast(str))
        } catch {
          case _: NumberFormatException =>
            None
        }
      else
        None
    }
  }

  /**
    * Integer extractor of a path variable:
    * {{{
    *   Path("/user/123") match {
    *      case Root / "user" / IntVar(userId) => ...
    * }}}
    */
  object IntVar extends NumericPathVar(_.toInt)

  /**
    * Long extractor of a path variable:
    * {{{
    *   Path("/user/123") match {
    *      case Root / "user" / LongVar(userId) => ...
    * }}}
    */
  object LongVar extends NumericPathVar(_.toLong)

  /**
    * Multiple param extractor:
    * {{{
    *   object A extends QueryParamDecoderMatcher[String]("a")
    *   object B extends QueryParamDecoderMatcher[Int]("b")
    *   val service: HttpService = {
    *     case GET -> Root / "user" :? A(a) +& B(b) => ...
    * }}}
    */
  object +& {
    def unapply(params: Map[String, Seq[String]]) = Some((params, params))
  }


  /**
    * param extractor using [[QueryParamDecoder]]:
    * {{{
    *   case class Foo(i: Int)
    *   implicit val fooDecoder: QueryParamDecoder[Foo] = ...
    *
    *   object FooMatcher extends QueryParamDecoderMatcher[Foo]("foo")
    *   val service: HttpService = {
    *     case GET -> Root / "closest" :? FooMatcher(2) => ...
    * }}}
    */
  abstract class QueryParamDecoderMatcher[T: QueryParamDecoder](name: String) {
    def unapplySeq(params: Map[String, Seq[String]]): Option[Seq[T]] =
      params.get(name).flatMap(values =>
        values.toList.traverseU(s =>
          QueryParamDecoder[T].decode(QueryParameterValue(s)).toOption
        )
      )

    def unapply(params: Map[String, Seq[String]]): Option[T] =
      params.get(name).flatMap(_.headOption).flatMap(s =>
        QueryParamDecoder[T].decode(QueryParameterValue(s)).toOption
      )
  }

  /**
    * param extractor using [[QueryParamDecoder]]:
    *
    * {{{
    *   case class Foo(i: Int)
    *   implicit val fooDecoder: QueryParamDecoder[Foo] = ...
    *   implicit val fooParam: QueryParam[Foo] = ...
    *
    *   object FooMatcher extends QueryParamDecoderMatcher[Foo]
    *   val service: HttpService = {
    *     case GET -> Root / "closest" :? FooMatcher(2) => ...
    * }}}
    */
  abstract class QueryParamMatcher[T: QueryParamDecoder: QueryParam]
    extends QueryParamDecoderMatcher[T](QueryParam[T].key.value)


  abstract class OptionalQueryParamDecoderMatcher[T: QueryParamDecoder](name: String) {
    def unapply(params: Map[String, Seq[String]]): Option[Option[T]] =
      params.get(name).flatMap(_.headOption).traverseU(s =>
        QueryParamDecoder[T].decode(QueryParameterValue(s))
      ).toOption
  }

  abstract class OptionalQueryParamMatcher[T: QueryParamDecoder: QueryParam]
    extends OptionalQueryParamDecoderMatcher[T](QueryParam[T].key.value)


  /**
    *  param extractor using [[org.http4s.QueryParamDecoder]]. Note that this will return a
    *  [[ParseFailure]] if the parameter cannot be decoded.
    *
    * {{{
    *  case class Foo(i: Int)
    *  implicit val fooDecoder: QueryParamDecoder[Foo] = ...
    *
    *  object FooMatcher extends ValidatingQueryParamDecoderMatcher[Foo]("foo")
    *  val service: HttpService = {
    *  case GET -> Root / "closest" :? FooMatcher(fooValue) => {
    *    fooValue.fold(
    *      nelE => BadRequest(nelE.toList.map(_.sanitized).mkString("\n")),
    *      foo  => { ... }
    *    )
    *  }
    * }}}
    */
  abstract class ValidatingQueryParamDecoderMatcher[T: QueryParamDecoder](name: String) {
    def unapply(params: Map[String, Seq[String]]): Option[ValidationNel[ParseFailure, T]] =
      params.get(name).flatMap(_.headOption).map {
        s => QueryParamDecoder[T].decode(QueryParameterValue(s))
      }
  }

  /**
    *  param extractor using [[org.http4s.QueryParamDecoder]]. Note that this will _always_ match, but will return
    *  an Option possibly containing the result of the conversion to T
    *
    * {{{
    *  case class Foo(i: Int)
    *  implicit val fooDecoder: QueryParamDecoder[Foo] = ...
    *
    *  case class Bar(i: Int)
    *  implicit val barDecoder: QueryParamDecoder[Bar] = ...
    *
    *  object FooMatcher extends ValidatingQueryParamDecoderMatcher[Foo]("foo")
    *  object BarMatcher extends OptionalValidatingQueryParamDecoderMatcher[Bar]("bar")
    *
    *  val service: HttpService = {
    *  case GET -> Root / "closest" :? FooMatcher(fooValue) +& BarMatcher(barValue) => {
    *    ^(fooValue, barValue getOrElse 42.right) { (foo, bar) =>
    *      ...
    *    }.fold(
    *      nelE => BadRequest(nelE.toList.map(_.sanitized).mkString("\n")),
    *      baz  => { ... }
    *    )
    *  }
    * }}}
    */
  abstract class OptionalValidatingQueryParamDecoderMatcher[T: QueryParamDecoder](name: String) {
    def unapply(params: Map[String, Seq[String]]): Option[Option[ValidationNel[ParseFailure, T]]] =
      Some {
        params.get(name).flatMap(_.headOption).fold[Option[ValidationNel[ParseFailure, T]]](None) {
          s => Some(QueryParamDecoder[T].decode(QueryParameterValue(s)))
        }
      }
  }
}
