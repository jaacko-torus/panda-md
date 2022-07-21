package jaackotorus.panda_md

object Json {
  sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String)         extends AnyVal with Val
  case class Obj(value: (java.lang.String, Val)*) extends AnyVal with Val
  case class Arr(value: Val*)                     extends AnyVal with Val
  case class Num(value: Double)                   extends AnyVal with Val
  case object False extends Val {
    def value = false
  }
  case object True extends Val {
    def value = true
  }
  case object Null extends Val {
    def value = null
  }
}

object Panda {
  sealed trait Val {
    val value: String
  }

  sealed case class Str(value: String)         extends Val
  case class Italic(value: String)             extends Val
  case class Bold(value: String)               extends Val
  case class Code(value: String, lang: String) extends Val
  case class Par(value: String)                extends Val
  case class H1(value: String)                 extends Val
  case class H2(value: String)                 extends Val
  case class H3(value: String)                 extends Val
  case class H4(value: String)                 extends Val
  case class H5(value: String)                 extends Val
  case class H6(value: String)                 extends Val
  case class ListItem(value: String)           extends Val
  case class List(value: String)               extends Val
}

object Parser {
  import fastparse._, NoWhitespace._
  def stringChars(c: Char) = c != '\"' && c != '\\'

  def space[_: P]      = P(CharsWhileIn(" \r\n", 0))
  def digits[_: P]     = P(CharsWhileIn("0-9"))
  def exponent[_: P]   = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[_: P] = P("." ~ digits)
  def integral[_: P]   = P("0" | CharIn("1-9") ~ digits.?)

  def number[_: P] =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x => Json.Num(x.toDouble))

  def `null`[_: P]  = P("null").map(_ => Json.Null)
  def `false`[_: P] = P("false").map(_ => Json.False)
  def `true`[_: P]  = P("true").map(_ => Json.True)

  def hexDigit[_: P]      = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[_: P]        = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def strChars[_: P] = P(CharsWhile(stringChars))
  def string[_: P] =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Json.Str)

  def array[_: P] =
    P("[" ~/ jsonExpr.rep(sep = ","./) ~ space ~ "]").map(Json.Arr(_: _*))

  def pair[_: P] = P(string.map(_.value) ~/ ":" ~/ jsonExpr)

  def obj[_: P] =
    P("{" ~/ pair.rep(sep = ","./) ~ space ~ "}").map(Json.Obj(_: _*))

  def jsonExpr[_: P]: P[Json.Val] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space,
  )
}
