INPUTA = """
c := 1
d := 2
a := b + c
e := 3 + d * 4 - a
print(e)
"""

INPUTB = """
def f(x) { ret x + 2 }
z := 3
a := z + f(4)
print(a)
"""
INPUTB2 = """
def f(x) { ret x + 2 }
z := 3
a := z + f(f(4))
print( a )
"""

INPUTB3 = """
def f(x) {
  y := 10
  ret y + x + 2
}
z := 3
a := z + f(f(4))
print( a )
"""

INPUTB4 = """
def f(x) {
  z := @l { 2 }
  y := 10
  ret y + x + ^ z + 2
}
z := 3
a := z + f(f(4))
print( a )
"""

INPUTB5 = """
z := @l { 3 }
a := ^ z + 2
print( a )
"""


INPUTC = """x := @l{ 3 + 10 + 20 }
y := ^ x + 4
print(y)
"""

INPUTD = """x := @l{ return int('10') + 3 }
y := ^ x + 4
print(y)
"""

INPUTE = """x := expr ^ @l{ import ast; return ast.parse('3') }
y := step(x) + 7
print(y)
"""

INPUTF = """x := binop expr ^ @l{ import ast; return ast.parse('3+8') }
y := step(x) + step(x) + step(x)
print(y)
"""

INPUTG = """x := binop binop expr ^ @l{ import ast; return ast.parse('3+8-1') }
y := step(x)
w := step(y) + 15
print(x)
print(y)
print(w)
"""
answerG="""BinOp( BinOp( 3(ast.Constant) + 8(ast.Constant) ) - 1(ast.Constant) )
BinOp( 11(int) - 1(ast.Constant) )
25
"""

INPUTG2 = """x := binop binop expr ^ @l{ import ast; return ast.parse('3+8-1') }
y := step(step(x)) + 15
print(x)
print(y)
"""
answerG2="""BinOp( BinOp( 3(ast.Constant) + 8(ast.Constant) ) - 1(ast.Constant) )
25
"""
answerH="""12
13
"""
INPUTH = """
def f(a, b) {
  ret a * b
}
def g(c) {
  ret c + 1
}
x := f(3, 4)
y := g(x)
print(x)
print(y)
"""

INPUTH2 = """
def f(a, b) {
  ret a * b
}
def g(c) {
  ret c + 1
}
print(g(f(3, 4)))
"""

INPUTE2 = """x := ^ @l{ import ast; return ast.parse('3') }
print(x)
"""

INPUTE3 = """x := expr ^ @l{ import ast; return ast.parse('3') }
print(x)
"""

INPUTE4 = """x := @l{ import ast; return ast.parse('3') }
print(x)
"""

INPUTE5 = """x := @l{ return 'cat' }
print(x)
"""

INPUTE6 = """x := ^ @l{ return 'cat' }
print(x)
"""

INPUTE7 = """x := ^ @l{ return 'cat' }
y := ^ @l{ return 'dog' }
z := x + y
print(z)
"""

INPUTE8 = """x := ^ @l{ return 'cat' }
y := ^ @l{ return 'dog' }
print(x + y + x)
"""

INPUTI = """x := ^ c_char_p &cat @c{ const char *cat() { return "cat"; } }?
y := ^ @l{ return 'dog' }
print( x + y)
"""

INPUTI2 = """x := ^ c_int &bat @c{ int bat() { return 3; } }?
y := ^ @l{ return 1 }
print( x + y)
"""

INPUTI4 = """x := ^ c_int &bat @c{
  int bat() {
    int i;
    for (i = 0; i < 3; ++i){ }
    return i;
  }
}?
y := ^ @l{
x = 5
y = 6
return abs(x - y) }
print( x + y)
"""

INPUTI3 = """x :=  ^ @b{ pwd ; ls | head }
y := ^ @m{ ver }
print( x + y)
"""

INPUTI5 = """
y := ^ @l{
def fn(x):
  return x * 2
}
print(y)
"""

INPUTI6 = """
y := ^ @c{
int fn(int a) {
  return a * 2;
}
}?
print(y)
"""

INPUTI7 = """
y := ^ &fn @c{ int fn(){ return 2 * 2;} }?
x := ^ &fn @l{ def fn(): return 2 * 2 }
print(x+y)
"""

INPUTI8 = """
y := ^ c_int &fn @c{ int fn(){ return 2 * 2;} }?
x := ^ &fn @l{ def fn(): return 2 * 2 }
print(x+y)
"""

INPUTI9 = """
x := ^ c_char_p &fn @c{ const char *fn(){ return "cat";} }?
y := ^ &fn @l{ def fn(): return 'dog' }
print(x+y)
"""

INPUTJ = """x := ^ @b{ echo "hello"}
print(x )
"""

INPUTK = """print(^@l{ return 3 })"""

INPUTK2 = """x := @l { return 3 }
print( ^ x )
"""

INPUTK3 = """x := @l { return 3 }
y := @l { return 4 }
print( ^ x + ^ y )
"""

INPUTK4 = """
x := @l { return 3 }
y := @l { return 4 }!
def hello(x) { ret 1+x }
print( ^ x + ^ y + hello(3) )
"""

INPUTK5 = """
x := @l { return 3 }
y := @l { return 4 }!
def hello(y,x,z) { ret x+z+y }
print(hello(3, ^x, ^y))
"""

INPUTK6 = """
x := @l { return 3 }
y := @c { return 5;}!
def hello(y,x,z) { ret x+z+y }
print(^y)
"""

INPUTL = """
x := [ 1, 2, 3 ]
print(x)
"""

INPUTL2 = """
x := [ 1, 2, 3 ]
y := [ 4, 5, 6 ]
print(x+y)
"""

INPUTL3 = """
x := 3 * (2 + 4) * 5
print(x)
"""

INPUTL4 = """
x := 3 + 2 * 4 + 5
print(x)
"""

INPUTM = """
x := 1
if x {
  x = 2
}
print(x)
"""

INPUTM2 = """
x := 1
if x {
  x = 2
  y := 3
}
print(y)
"""

INPUTM3 = """
x := if 1 {
  y := 3
}
print(x)
"""
INPUTM4 = """
x := if 1 {
  y := [ 3, 2, 1 ]
}
print(x)
"""

INPUTM5 = """
x : if 0 {
  y : [ 3, 2, 1 ]
} else {
  z : 4
}
print(x)
"""

INPUTM6 = """
x : if 1 {
  [ 3, 2 ]
} else {
  4
}
print(x)
"""

INPUTM7 = """
x:1
print(if x {
  [ 3, 2 ]
} else {
  4
})
"""

INPUTM8 = """
x:0
print(if x { [ 3, 2 ] } else { 4 })
"""

INPUTN = """
x:-3
print(x+5)
"""

INPUTN2 = """
x:-3
print(-x+-5)
"""

INPUTN3 = """
x:-3
print(-x--5)
"""

INPUTN4 = """
x:-3
print(abs(x)+abs(-5))
"""

INPUTO = """
x:@l { return 'cat' }
print(^x*2)
"""

INPUTO2 = """
y:@c { return 0;}!
x:@l { return 'cat' }
print(2*^x + 1*^y)
"""

INPUTO3 = """
y:2
x:1
print(2*-x + 1*-y)
"""

INPUTO4 = """
y:2
x:1
print(2*-x + 1*abs(-y))
"""

INPUTP = """
y:[ 1, 2, 3 ]
x:0
for e in y {
  x = e + x
}
print(x)
"""

INPUTP2 = """
y:1
x:0
x = y + x
print(x)
"""

INPUTP3 = """
y:[ 1, 2, 3 ]
print(y[1])
"""

INPUTQ = """
def sum(l) {
  s : 0
  for e in l {
    s = s + e
  }
  ret s
}
y: [ 1, 2, 3 ]
print(sum(y))
"""

INPUTQ2 = """
def sum(l) {
  ret 5
  s : 0
  for e in l {
    s = s + e
  }
  ret s
}
y: [ 1, 2, 3 ]
print(sum(y))
"""

INPUTQ3 = """
def double(x) {
  if x {
    r : double(x-1)
    ret r+2
  } else {
    ret x
  }
}
y: 3
print(double(y))
"""

INPUTQ4 = """
def fact(x) {
  if x {
    r : fact(x-1)
    ret x*r
  }
  ret 1
}
y: 3
print(fact(y))
print(fact(4))
print(fact(5))
"""

INPUTQ5 = """
def addsome(x) {
  if x {
    ret x + 10
  }
    ret x + 1
}
print(addsome(5))
"""

INPUTQ6 = """
def addsome(x,y) {
  if x {
    if y {
      ret x + 10
    } else {
      ret x + 20
    }
  }
    ret x + 1
}
print(addsome(5, 1))
print(addsome(5, 0))
print(addsome(0, 0))
"""

INPUTR = """
x := 1+1 == 2
print(x)
"""

INPUTR2 = """
x := (3+2) == (1+4)
print(x)
"""

INPUTR3 = """
x := 3+2 == 1+4
print(x)
"""

INPUTS = """
def fib(x) {
  if x == 0 { ret 0 }
  if x == 1 { ret 1 }
  ret fib(x-1) + fib(x-2)
}
print(fib(3))
print(fib(4))
print(fib(5))
print(fib(6))
print(fib(7))
"""

INPUTS = """
def fib(x) {
  if x <= 1 { ret x }
  ret fib(x-1) + fib(x-2)
}
print(fib(3))
print(fib(4))
print(fib(5))
print(fib(6))
print(fib(7))
"""

INPUTT = """
x : 3 < 4 == 2 >= 1
y : 4 < 3 == 5 > 6 == 1 > 3
print(x)
print(y)
"""

INPUTT2 = """
x : 3 < 2+5 == 1+10 > 5
print(x)
"""

INPUTT3 = """
def ack(x, y) {
  if x == 0 {
    ret y+1
  }
  if y == 0 {
    ret ack(x-1, 1)
  }
  a : ack(x, y-1)
  b : ack(x-1, a)
  ret b
}
print(ack(1,1))
print(ack(2,2))
print(ack(3,0))
print(ack(3,1))
print(ack(3,2))
print(ack(3,3))
"""

INPUTT4 = """
def nonack(x, y) {
  if x == 0 { ret y }
  if y == 0 { ret nonack(x-1, 2) }
  ret nonack(x-1, nonack(x, y-1))
}
print(nonack(4,4))
"""

INPUTU = """
x : "cat\\n"
y : "dog\\n"
z : x.strip() + y
print(z)
"""

INPUTU2 = """
x : 3
print(x.bit_count())
"""

INPUTU3 = """
x : "print('hi')"
print(eval(x))
"""

INPUTU4 = """
x : "print('hi')"
x.print()
"""

INPUTU5 = """
x : "print('hi')"
print(x.rindex("int"))
"""

INPUTU6 = """
x : binop binop expr ^ @l { import ast; ast.parse('3+2-1') }
print(x.step().step())
"""

INPUTU7 = """
type ty {
  x
  y
  z
}
myty <: ty = { 3, 4, 5, }
print(myty.z.bit_count())
"""

INPUTU8 = """
type Ty {
  x
  y
}
type Uy {
  a
  b
}
myty <: Ty = { 2, 2, }
myuy <: Uy = { 1, 2, }
myuy2 <: Uy = { 2, 2, }
myty2 <: Ty = { 2, 2, }
print(myty == myuy)
print(myty === myuy2)
print(myty === myty2)
"""

INPUTU9 = """
type Ty { x }
type Uy { a }
t <: Ty = { 42, }
print(type(t))
"""

INPUTU10 = """
type Ty { x }
t <: Ty = { 42, }
def func(tt) {
  ret tt
}
print(func(t).x)
"""

INPUTV = """
def func() {
  ret 3; 2
}
print(func())
"""

INPUTV2 = """
def func() {
  ret 3; 2
}
a : func()
print(a)
"""

INPUTV3 = """
def func() {
  x : 13 + 7
  ret 3; x; 1
}
a; b; c : func()
print(a)
print(b)
print(c)
"""

INPUTV4 = """
def func() {
  x : 13 + 7
  ret x
}
q : func()
print(q)
"""


INPUTW = """
x : 0
y : 0
while x == 0 {
  print(x)
  print(y)
  if y == 1 {
    x = x + 1
  }
  y = y + 1
}
print(x)
print(y)
"""

INPUTX = """
type Yy {
  x
  y
}
type Ty {
  x
  y
}
X <: Ty = { 42, 24, }
Y <: Yy = { 42, 24, }
print(X)
print(ty(X))
print(Ty)
print(Y)
print(ty(Y))
print(Yy)
print(ty(X)==Ty)
print(Yy==ty(Y))
"""