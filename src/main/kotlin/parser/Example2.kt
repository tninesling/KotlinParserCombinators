package parser

sealed class Expr
data class App(val e1: Expr, val e2: Expr): Expr()
data class Lam(val s: String, val e: Expr): Expr()
data class Let(val s: String, val e1: Expr, val e2: Expr): Expr()
data class Var(val s: String): Expr()

// type Parser<Expr>
fun expr(s: String): List<Pair<Expr, String>> =
  (atom chainl1 parserResult({e1: Expr, e2: Expr -> App(e1, e2)}))(s)

val variable: Parser<String> = identifier(listOf("let", "in"))

val parseParen: Parser<Expr> = middle(symbol("("), ::expr, symbol(")"))

val parseVar: Parser<Expr> = variable bind {x -> parserResult(Var(x))}

val parseLam: Parser<Expr> = symbol("\\") bind {_ ->
  variable bind {x ->
    symbol("->") bind {_ ->
      ::expr bind {e ->
        parserResult(Lam(x, e))
      }
    }
  }
}

val parseLocal: Parser<Expr> = symbol("let") bind {_ ->
  variable bind {x ->
    symbol("=") bind {_ ->
      ::expr bind {e1 ->
        symbol("in") bind {_ ->
          ::expr bind {e2 ->
            parserResult(Let(x, e1, e2))
          }
        }
      }
    }
  }
}

val atom: Parser<Expr> = parseLam detOr parseLocal detOr parseVar detOr parseParen
