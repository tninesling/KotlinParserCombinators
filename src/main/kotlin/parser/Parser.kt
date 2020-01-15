package parser

import kotlin.math.pow

/* Parser based on monadic parsing description in http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf.
 * Some of the implementations and/or optimizations are not necessarily efficient if they rely
 * on laziness.
 */

// TODO: Replace List<T> with Sequence<T>

typealias Parser<A> = (String) -> List<Pair<A, String>>

fun <A> parserResult(v: A): Parser<A> = { inp -> listOf(Pair(v, inp)) }

fun <A> zero(@Suppress("UNUSED_PARAMETER") inp: String): List<Pair<A, String>> = emptyList()

// The paper uses a list comprehension and a Haskell-style concat, but it's really just flatMap.
// This may need to be curried, depending on usage
infix fun <A,B> Parser<A>.bind(f: (A) -> Parser<B>): Parser<B> =
  { inp -> this(inp).flatMap({ (v, inp2) -> f(v)(inp2) }) }

// type Parser<Char>
fun item(inp: String): List<Pair<Char, String>> =
  when (inp) {
    ""   -> emptyList()
    else -> listOf(Pair(inp.get(0), inp.substring(1)))
  }

// Returns a parser which parses a Char if it satisfied predicate p
// Note: the :: before `item` makes it pass the reference to the function
fun sat(p: (Char) -> Boolean): Parser<Char> =
  ::item bind {x: Char ->
      if (p(x)) {
        parserResult(x)
      } else {
        {s -> zero(s)}
      }
    }

fun char(x: Char): Parser<Char> = sat({y -> x == y})

val digit: Parser<Char> = sat({x -> '0' <= x && x <= '9'})

val lower: Parser<Char> = sat({x -> 'a' <= x && x <= 'z'})

val upper: Parser<Char> = sat({x -> 'A' <= x && x <= 'Z'})

val letter: Parser<Char> = lower or upper

val alphanum: Parser<Char> = letter or digit

// type Parser<String>
fun word(s: String): List<Pair<String, String>> {
  val neWord =
    letter bind {x ->
      ::word bind {xs ->
        parserResult(x.plus(xs))
      }
    }
  
  return (neWord or parserResult(""))(s)
}

val ident: Parser<String> =
  lower bind {x ->
    many(alphanum) bind {xs ->
      parserResult(listOf(x).plus(xs).joinToString())
    }
  }

val nat: Parser<Int> =
  digit bind {x ->
    parserResult(x.toInt())
  } chainl1
    parserResult({m, n -> 10*m + n})

val int: Parser<Int> =
  char('-') bind {_ ->
    nat bind {n ->
      parserResult(-1 * n)
    }
  } or (nat)

// COMBINATORS /////////////////

// Parses an A with this parser or q
infix fun <A> Parser<A>.or(q: Parser<A>): Parser<A> =
  {inp -> this(inp).plus(q(inp))}

// A deterministic implementation of or
infix fun <A> Parser<A>.detOr(q: Parser<A>): Parser<A> =
  first(this or q)

fun <A> force(p: Parser<A>): Parser<A> = {inp ->
  val x = p(inp)
  val xHead = x.first()
  val xTail = x.drop(1)
  listOf(Pair(xHead.first, xHead.second)).plus(xTail)
}

fun <A> many(p: Parser<A>): Parser<List<A>> =
  force(
    p bind {x: A ->
      many(p) bind {xs: List<A> ->
        parserResult(listOf(x).plus(xs))
      }
    } or parserResult(emptyList())
  )

// Parses at least one A with p
fun <A> many1(p: Parser<A>): Parser<List<A>> =
  p bind {x ->
    many(p) bind {xs ->
      parserResult(listOf(x).plus(xs))
    }
  }

fun <A> first(p: Parser<A>): Parser<A> = {inp ->
  val xs = p(inp)
  if (xs.isEmpty())
    emptyList()
  else
    listOf(xs.first())
}

// Parses many As with p, separated by Bs parsed by sep
// This requires at least one A be parsed
fun <A,B> sepby1(p: Parser<A>, sep: Parser<B>): Parser<List<A>> {
  val sepThenNext =
    many(
      sep bind {_ ->
        p bind {y ->
          parserResult(y)
        }
      }
    )

  return p bind {x ->
    sepThenNext bind {xs ->
      parserResult(listOf(x).plus(xs))
    }
  }
}

// This is `bracket` in the paper
// Parses 3 items, then returns the middle one
fun <A,B,C> middle(left: Parser<A>, middle: Parser<B>, right: Parser<C>): Parser<B> =
  left bind {_ ->
    middle bind {x ->
      right bind {_ ->
        parserResult(x)
      }
    }
  }

// Parses a List of As parsed by p, separated by Bs parsed by sep
// Parsing neither is also acceptable (note the addition of [[]] to
// the List of parsed parserResults)
fun <A,B> sepby(p: Parser<A>, sep: Parser<B>): Parser<List<A>> =
  sepby1(p, sep) or parserResult(emptyList())

typealias BinaryOp<A> = (A,A) -> A

infix fun <A> Parser<A>.chainl1(op: Parser<BinaryOp<A>>): Parser<A> {
  fun rest(x: A): Parser<A> =
    op bind {f ->
      this bind {y ->
        rest(f(x, y))
      }
    }

  return this bind ::rest
}

infix fun <A> Parser<A>.chainr1(op: Parser<BinaryOp<A>>): Parser<A> =
  this bind {x ->
    op bind {f ->
      this chainr1 op bind {y ->
        parserResult(f(x, y))
      }
    }
  }

fun <A,B> ops(xs: List<Pair<Parser<A>, B>>): Parser<B> =
  xs.map({(p: Parser<A>, op: B) ->
    p bind {_ ->
      parserResult(op)
    }
  }).reduce({p: Parser<B>, q: Parser<B> -> p or q})

val isSpace = {x: Char ->
  x == ' ' ||
  x == '\n' ||
  x == '\t'
}

val spaces: Parser<Unit> =
  many1(sat(isSpace)) bind {_ ->
    parserResult(Unit)
  }

fun string(s: String): Parser<String> =
  if (s.isEmpty())
    parserResult("")
  else
    char(s.first()) bind {_ ->
      string(s.substring(1)) bind {_ ->
        parserResult(s)
      }
    }

val comment: Parser<Unit> =
  string("--") bind {_ ->
    many(sat({x -> x != '\n'})) bind {_ ->
      parserResult(Unit)
    }
  }

val junk: Parser<Unit> =
  many(spaces detOr comment) bind {_ ->
    parserResult(Unit)
  }

fun <A> parse(p: Parser<A>): Parser<A> =
  junk bind {_ ->
    p bind {v ->
      parserResult(v)
    }
  }

fun <A> token(p: Parser<A>): Parser<A> =
  p bind {v ->
    junk bind {_ ->
      parserResult(v)
    }
  }

val natural: Parser<Int> = token(nat)

val integer: Parser<Int> = token(int)

val symbol: (String) -> Parser<String> = {xs ->
  token(string(xs))
}

val identifier: (List<String>) -> Parser<String> = {ks ->
  token(
    ident bind {x ->
      if (ks.contains(x))
        {_ -> zero(x)}
      else
        parserResult(x)
    }
  )
}