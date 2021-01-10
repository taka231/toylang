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

echo OK