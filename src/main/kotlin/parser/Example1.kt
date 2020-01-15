package parser

import kotlin.math.pow

val add = {x: Int, y: Int -> x + y}

val sub = {x: Int, y: Int -> x - y}

val exp = {x: Int, y: Int -> x.toDouble().pow(y).toInt()}

val addop = ops(listOf(Pair(char('+'), add), Pair(char('-'), sub)))

val expop = ops(listOf(Pair(char('^'), exp)))

val term = ::factor chainr1 expop

// type Parser<Int>
val expression = term chainl1 addop

fun factor(inp: String): List<Pair<Int, String>> =
  nat(inp).plus(middle(char('('), expression, char(')'))(inp))