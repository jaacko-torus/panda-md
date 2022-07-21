package jaackotorus.panda_md

object JsonAST {
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

object PandaAST {
  sealed trait Inline {
    val value: String
  }

  case class Str(value: String)     extends Inline
  case class Italic(value: String)  extends Inline
  case class Bold(value: String)    extends Inline
  case class Crossed(value: String) extends Inline
  case class Code(value: String)    extends Inline

  sealed trait Block extends Inline

  case class CodeBlock(value: String, lang: String) extends Block
  case class H1(value: String)                      extends Block
  case class H2(value: String)                      extends Block
  case class H3(value: String)                      extends Block
  case class H4(value: String)                      extends Block
  case class H5(value: String)                      extends Block
  case class H6(value: String)                      extends Block

  case class List(value: String)     extends Block
  case class ListItem(value: String) extends Block

  case class Br(value: String = "") extends Block
}

object PandaParser {
  import fastparse._
  import fastparse.NoWhitespace._

  def space[_: P] = P(CharsWhileIn(" ", 1))

  def hexDigit[_: P]      = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[_: P]        = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def string[_: P] = P(escape | space | AnyChar)

  def str[_: P]     = P(string.rep.!).map(PandaAST.Str)
  def italic[_: P]  = P("/" ~ string ~ "/").!.map(PandaAST.Italic)
  def bold[_: P]    = P("*" ~ string ~ "*").!.map(PandaAST.Bold)
  def crossed[_: P] = P("~" ~ string ~ "~").!.map(PandaAST.Crossed)
  def code[_: P]    = P("`" ~ string ~ "`").!.map(PandaAST.Code)

  def h1[_: P] = P("#".rep(1) ~ space ~ string.!).map(PandaAST.H1)
  def h2[_: P] = P("#".rep(2) ~ space ~ string.!).map(PandaAST.H2)
  def h3[_: P] = P("#".rep(3) ~ space ~ string.!).map(PandaAST.H3)
  def h4[_: P] = P("#".rep(4) ~ space ~ string.!).map(PandaAST.H4)
  def h5[_: P] = P("#".rep(5) ~ space ~ string.!).map(PandaAST.H5)
  def h6[_: P] = P("#".rep(6) ~ space ~ string.!).map(PandaAST.H6)
}

object JsonParser {
  import fastparse._, NoWhitespace._
  def stringChars(c: Char) = c != '\"' && c != '\\'

  def space[_: P]      = P(CharsWhileIn(" \r\n", 0))
  def digits[_: P]     = P(CharsWhileIn("0-9"))
  def exponent[_: P]   = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[_: P] = P("." ~ digits)
  def integral[_: P]   = P("0" | CharIn("1-9") ~ digits.?)

  def number[_: P] =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x => JsonAST.Num(x.toDouble))

  def `null`[_: P]  = P("null").map(_ => JsonAST.Null)
  def `false`[_: P] = P("false").map(_ => JsonAST.False)
  def `true`[_: P]  = P("true").map(_ => JsonAST.True)

  def hexDigit[_: P]      = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[_: P]        = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def strChars[_: P] = P(CharsWhile(stringChars))
  def string[_: P] =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(JsonAST.Str)

  def array[_: P] =
    P("[" ~/ jsonExpr.rep(sep = ","./) ~ space ~ "]").map(JsonAST.Arr(_: _*))

  def pair[_: P] = P(string.map(_.value) ~/ ":" ~/ jsonExpr)

  def obj[_: P] =
    P("{" ~/ pair.rep(sep = ","./) ~ space ~ "}").map(JsonAST.Obj(_: _*))

  def jsonExpr[_: P]: P[JsonAST.Val] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space,
  )
}
