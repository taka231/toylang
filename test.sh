#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  echo "$input" | stack run | lli
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 32 32
assert 10 "// comment
10"
assert 10 "/* comment
*/ 10"
assert 2 "1+1"
assert 38 "15+23"
assert 40 "13 + 32 - 5"
assert 24 "1+3/1+5*4"
assert 16 "(3+5)*2"
assert 30 "a=10; b=20; a+b;"
assert 3 "if 1 < 2 then 3 else 4"
assert 4 "if 1 > 2 then 3 else 4"
assert 3 "if 2 <= 2 then 3 else 4"
assert 4 "if 1 >= 2 then 3 else 4"
assert 3 "if 1 == 1 then 3 else 4"
assert 30 "add x y = x + y; add 10 20;"
assert 120 "fact x = if x <= 1 then 1 else x * fact (x - 1); fact 5"
assert 55 "fib n = if n <= 2 then 1 else fib (n - 1) + fib (n - 2); fib 10"
assert 40 "(+*) @(infix L 6); (+*) a b = (a + b) * b; 3 +* 5;"
assert 3 'sub @(infix L 6); sub a b = a - b; 5 `sub` 2'

echo OK