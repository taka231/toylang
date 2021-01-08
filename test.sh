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

echo OK