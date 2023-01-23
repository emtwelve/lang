from dataclasses import dataclass
#from distutils.ccompiler import new_compiler
from typing import Optional, Union
from collections import defaultdict
from io import StringIO

import ast, inspect
import itertools, functools
import types, ctypes
import sys, os, atexit, traceback
import tempfile
import copy
import argparse
import inspect
import re
import faulthandler
faulthandler.enable()

os.system('')

def write_normal():
  sys.stdout.write("\033[0;0m")

def write_red():
  sys.stdout.write("\033[1;31m")

def write_green():
  sys.stdout.write("\033[1;32m")

def write_blue():
  sys.stdout.write("\033[1;34m")

def cr_print(*args, **kwargs):
  write_red()
  print(*args, **kwargs)
  write_normal()

def cg_print(*args, **kwargs):
  write_green()
  print(*args, **kwargs)
  write_normal()

def cb_print(*args, **kwargs):
  write_blue()
  print(*args, **kwargs)
  write_normal()

def panic():
  assert False

#io
logging = False

#errors
@dataclass
class Error(Exception):
  message:list
  def __init__(s, *args, **kwargs):
    super().__init__()
    s.message = args
    print(*args, **kwargs)

# tokens / lexemes
@dataclass
class ID():
  val:str
  line:int = 0
  def __call__(s): return s.val
  def __hash__(s): return hash(s.val)
  def __str__(s):
    if type(s.val) == list:
      print("HERE0")
      return 'IDLISTstr'
    if type(s.val) == str:
      #print("ID strvaltype", type(s.val))
      return "'" + str(repr(s.val))
    else:
      return "'" + 'IDLISTstrr'
  def __repr__(s):
    if type(s.val) == list:
      return 'IDLISTrepr'
    elif type(s.val) == "AGG_TYPE":
      return 'IDLISTreprr'
    elif type(s.val) == str:
      return s.val
    else:
      #print('IDtypeval', type(s.val))
      return 'IDnostr'
  def __eq__(s, o):
    return isinstance(o, ID) and s.val == o.val

@dataclass
class KW():
  x:str

@dataclass
class IF(): pass

@dataclass
class ELSE(): pass

@dataclass
class FOR(): pass

@dataclass
class IN(): pass

@dataclass
class WHILE(): pass

@dataclass
class NUM():
  val:int
  def __call__(s): return s.val
  def __hash__(s): return hash(s.val)
  def __str__(s):
    return "'" + str(s.val)
  def __repr__(s): return s.__str__()

@dataclass
class CLOSURE():
  val:int
  def __call__(s): return s.val
  def __hash__(s): return hash(s.val)
  def __str__(s):
    return "%" + str(s.val)
  def __repr__(s): return s.__str__()


@dataclass
class STR():
  val:str
  def __call__(s): return s.val
  def __hash__(s): return hash(s.val)
  def __str__(s):
    return "STR(" + str(s.val) + ")"

@dataclass
class TYPE():
  name:str
  def __hash__(s): return hash(s.val)
  
@dataclass
class AGG():
  val:list

@dataclass
class TUPLE():
  val:tuple
  def __call__(s): return s.val
  def __hash__(s): return hash(s.val)

@dataclass
class PRINT():
  arg:Union
  def __str__(s):
    return "PRINT(" + str(s.arg) + ")"

@dataclass
class NUM():
  val:int
  def __call__(s): return s.val
  def __hash__(s): return hash(s.val)
  def __str__(s):
    return "'" + str(s.val)
  def __repr__(s): return s.__str__()

@dataclass
class STR():
  val:str
  def __add__(s,o): return STR(s.val + o.val)
  def __call__(s): return s.val
  def __hash__(s): return hash(s.val)
  def __str__(s):
    return "STR(" + str(s.val) + ")"

@dataclass
class TUPLE():
  val:tuple
  def __call__(s): return s.val
  def __hash__(s): return hash(s.val)

@dataclass
class PRINT():
  arg:Union[NUM,ID,'TermNode']
  def __str__(s):
    return "PRINT(" + str(s.arg) + ")"

@dataclass
class FUNC():
  ident:ID
  param:ID
  def __str__(s):
    return "FUNC(" + str(s.ident) + ',' + str(s.param) + ")"

@dataclass
class CALL():
  ident:ID
  arg:list
  line:int
  def __str__(s):
    return "CALL(ident:" + str(s.ident) + ',arg:' + str(s.arg) + ")"

@dataclass
class UFCSCALL():
  ident:ID
  this:'ExprNode'
  arg:'ExprNode'
  line:int

@dataclass
class MEMBER():
  ident:ID
  this:'ExprNode'
  line:int

@dataclass
class ARRAY_ACCESS():
  arr:'ExprNode'
  idx:'ExprNode'

@dataclass
class TUPLE_BUNDLE():
  nesting:'TUPLE_BUNDLE'
  elem:'ExprNode'

@dataclass
class EVAL_BUNDLE():
  nesting:'EVAL_BUNDLE'
  elem:'ExprNode'

@dataclass
class INNER_BUNDLE():
  LHS:'INNER_BUNDLE'
  RHS:'ExprNode'

@dataclass
class IDENTIFIER_BUNDLE():
  nesting:'IDENTIFIER_BUNDLE'
  elem:ID

@dataclass
class RET():
  def __str__(s): return 'RET'

@dataclass
class LANGBLOCK():
  lang:ID
  body:str
  type:str
  args:'ArgBundle'
  def __str__(s):
    return "LB(l:" + str(s.lang) + ',b:' + s.body + ',' + str(s.args) + ')'

@dataclass
class Factor():
  val:Union[CALL,NUM,ID,LANGBLOCK]

# nodes (tokens can be nodes)
@dataclass
class TermNode():
  line:int
  lhs:Factor
  op:str = ''
  rhs:Union[Factor,'TermNode',None] = None
  def __str__(s):
    if s.op:
      return 'TN ' + str(s.lhs) + s.op + str(s.rhs)
    else:
      return 'TN ' + str(s.lhs)

@dataclass
class CarrotNode():
  expr:'ExprNode'
  def __str__(s):
    return 'CN ' + str(s.expr)

@dataclass
class CarrotExpr():
  kw:KW
  expr:Union[CarrotNode,'CarrotExpr']

@dataclass
class PreCarrotExpr():
  kw:KW
  expr:Union[CarrotNode,CarrotExpr,'PreCarrotExpr']

@dataclass
class ID2():
  val:str
  def __repr__(s): return str(s)
  def __str__(s): return '\'' + str(s.val)
  def __hash__(s): return hash(s.val)

@dataclass
class Expr():
  valL:'Expr'
  op:str
  valR:'Expr'
  def __repr__(s): return str(s)
  def __str__(s): return '(' + str(s.valL) + ' ' + str(s.op) + ' ' + str(s.valR) + ')'

@dataclass
class ExprNode():
  lhs:TermNode
  op:str
  rhs:Union[CarrotExpr,CarrotNode,PreCarrotExpr,'ExprNode',TermNode]
  line:int
  def __str__(s):
    return 'EN ' + str(s.lhs) + s.op + str(s.rhs)
  def __repr__(s): return str(s)

@dataclass
class TYPED():
  expr:ExprNode
  typ:TYPE

@dataclass
class CompareNode():
  lhs:ExprNode
  op:str
  rhs:ExprNode
  def __str__(s):
    return 'CmpN(' + str(s.lhs) + s.op + str(s.rhs) + ")"
  def __repr__(s): return str(s)

@dataclass
class NegateNode():
  expr:ExprNode
  def __str__(s):
    return 'NN ' + str(s.expr)

@dataclass
class AggregateNode():
  name:str
  vals:list

@dataclass
class AggregateObject():
  exprs:list
  def __str__(s):
    return "`(" + str(s.exprs) + ')'
  def __repr__(s):
    return str(s)

@dataclass
class AssignNode():
  lhs:ID
  rhs:ExprNode
  def __str__(s):
    return "AN(l: " + str(s.lhs) + ', r: ' + str(s.rhs) + ")"

@dataclass
class AssignMutableNode():
  lhs:ID
  rhs:ExprNode
  def __str__(s):
    return "AM(l: " + str(s.lhs) + ', r: ' + str(s.rhs) + ")"

@dataclass
class AssignTypeNode():
  lhs:ID
  rhs:ExprNode
  typ:type
  def __str__(s):
    return "AT(l: " + str(s.lhs) + ', r: ' + str(s.rhs) + ', t: ' + str(s.typ) + ")"

@dataclass
class FuncNode():
  name:ID
  param:ID
  body:ExprNode
  def __str__(s):
    return "FD(n: " + str(s.name) + ', p: ' + str(s.param) + ', b: ' + str(s.body) + ")"

@dataclass
class ReturnNode():
  expr:ExprNode
  def __str__(s):
    return "Return(expr: " + str(s.expr) + ")"

@dataclass
class ForLoopNode():
  ident:ID
  iterable:ExprNode
  body:list

@dataclass
class WhileLoopNode():
  condition:ExprNode
  body:list

@dataclass
class ListNode():
  l:list

@dataclass
class Grouping():
  expr:ExprNode

def repr_list(l):
  res = ""
  for e in l:
    try:
      if type(e).__name__ == 'function':
        res += 'Function'
      elif type(e) == str:
        res += repr(e)
      else:
        res += repr(e)
    except Exception as ex:
      raise Error(res, e, type(e).__name__)
  return res

@dataclass
class Bundle():
  x:list
  def unbundle(s): return s.x
  def __str__(s):
    if type(s.x).__name__ == 'function':
      return "'" + '<function>'
    elif type(s.x) == list:
      return "'" + str(repr_list(s.x))
    else:
      return "'" + '<unrepresentable>'
      #print(repr(type(s.x)))
  def __repr__(s): return str(s.x)

@dataclass
class FuncBundle():
  x:list
  def __str__(s): return '"' + str(s.x)
  def __repr__(s): return str(s)

@dataclass
class ArgBundle():
  x:list
  def unbundle(s): return s.x
  def __str__(s): return "`" + str(s.x)
  def __repr__(s): return str(s.x)

@dataclass
class IfElseNode():
  c:ExprNode
  t:list
  f:list

"""
 program ::= statements
 statements = (assignment | print)+
 assignment ::= identifier := expression
 expression ::= term {op term}* where op=[+-*/]
 term ::= integral | identifier | expression
 identifier ::= [A-z]+[A-9]*
"""

INPUT1 = [ ID('a'), ':=', ID('b'), '+', ID('c') ]
INPUT2 = [ ID('a'), ID('b'), '+', ID('c') ]
INPUT3 = [ ID('a'), ':=', ID('b'), '+', ID('c'),
           ID('d'), ':=', ID('a'), '-', NUM(5),
           ID('e'), ':=', NUM(3), '+', ID('d') ]
INPUT4 = [ ID('b'), ':=', NUM(8),
           ID('c'), ':=', NUM(1),
           ID('a'), ':=', ID('b'), '+', ID('c'),
           ID('d'), ':=', ID('a'), '-', NUM(5),
           ID('e'), ':=', NUM(3), '+', ID('d'), '-', NUM(2),
           PRINT(ID('e'))
         ]

###########
# Printing

def print_ast(AST):
  for stmt in AST:
    print(stmt)

def print_lexemes(inp):
  lastSaw = False
  for lex in inp:
    if type(lex) in [ ID, NUM, CALL, LANGBLOCK ]:
      if lastSaw: print() #newline
      lastSaw = True
    else: lastSaw = False
    print(lex, end="")
    if lex in [ '{', '}' ]: print()
  print()

###########
# Lexing

# local trace function which returns itself
def my_tracer(frame, event, arg = None):
    # extracts frame code
    code = frame.f_code
  
    # extracts calling function name
    func_name = code.co_name
  
    # extracts the line number
    line_no = frame.f_lineno

    if event == 'exception' and func_name not in [ 'ishashable', '__exit__', 'interleave', '__getitem__', 'dispatch', '_compile' ]:  
       print(f"A {event} encountered in \
      {func_name}() at line number {line_no} ")
  
    return my_tracer
  
#sys.settrace(my_tracer)

sys.setrecursionlimit(10**9 + 10**8)

def add_inner_funcs(s):
  s['double_quote'] = double_quote
  s['single_quote'] = single_quote
  s['reg_compile'] = reg_compile
  s['read_inputfile'] = read_inputfile
  s['read_termfile'] = read_termfile
  s['reg_finditer'] = reg_finditer
  s['reg_numgroups'] = reg_numgroups
  s['reg_lastindex'] = reg_lastindex
  s['reg_match'] = reg_match
  s['strp'] = strp
  s['match_all'] = match_all
  s['reg_search_dquote'] = reg_search_dquote
  s['reg_search_squote'] = reg_search_squote
  s['reg_matchleft'] = reg_matchleft
  s['reg_matchright'] = reg_matchright
  s['has_ddollar'] = has_ddollar
  s['reg_full_match'] = reg_full_match
  s['reg_search'] = reg_search
  s['cast'] = cast
  s['new_set'] = new_set
  s['add_to_set'] = add_to_set
  s['find_in_set'] = find_in_set
  s['my_eval'] = my_eval
  s['kind'] = kind
  s['fval2'] = fval2
  s['fval1'] = fval1
  s['fval0'] = fval0
  s['fval'] = fval
  s['fn_kind'] = fn_kind
  s['pattern'] = pattern
  s['match_obj2'] = match_obj2
  s['match_obj'] = match_obj
  s['rem_quo'] = rem_quo
  s['doesnt_contain_triple'] = doesnt_contain_triple
  s['remove_quadruple'] = remove_quadruple
  s['vval'] = vval
  s['getnext'] = getnext
  s['reset_idx'] = reset_idx
  s['split'] = split
  s['fn_eval'] = fn_eval
  s['cr_print'] = cr_print
  s['cg_print'] = cg_print
  s['cb_print'] = cb_print
  s['panic'] = panic
def new_set():
  return {}
def add_to_set(k, v, s):
  #print(type(s), s, type(k), k, type(v), v)
  s[k] = v
  return s
def find_in_set(s, k):
  print('find_in_set', s, k)
  if k in s: print('found', s[k]); return s[k]
  else: print('not found'); return -100
def kind(x):
  return type(x)
def cast(x):
  return int(x)
def has_ddollar(body):
  rres = re.search(r"\$\$(\w+)", body)
  if rres: return True
  return False
def replace_ddollar(body):
  body = re.sub(r"\$\$(\w+)", r"\1", body)
  return body
def ddollar_ident(body):
  rres = re.search(r"\$\$(\w+)", body)
  return rres.group(1)
def double_quote():
  return '"'
def single_quote():
  return "'"
def reg_search_squote(s):
  m = re.search(r"'(.*)'", s)
  if m:
    return m[1]
  else:
    return s
def reg_search_dquote(s):
  print('reg_search_dquote', s)
  print(repr(s))
  m = re.search(r"\"(.*)\"", s)
  if m:
    return m[1]
  else:
    return s
def reg_search_impl(s):
  #fn_defn = re.search(r"def (?P<name>\w+)\((?P<param>[\w, ]*)\)", fndefn)
  return re.search(r"def (?P<name>\w+)\((?P<param>[\w, ]*)\)", s)
def groupd(m, s):
  #name = fn_defn.groupdict()['name']
  #param = fn_defn.groupdict()['param']
  return m.groupdict()[s]
def reg_search(s, n):
  print('reg_search', s, n)
  m = reg_search_impl(s)
  print('m', m)
  return groupd(m, n)
def reg_search_gen(p, s):
  return re.search(p, s)
def reg_compile(x): return re.compile(x)
def print_list_without_none(x):
  print("[", end=" ")
  for elem in x:
    if elem != None:
      print(elem, end=", ")
  print("]")
@dataclass
class Pattern():
  pattern : re.Pattern
@dataclass
class Match():
  mat : re.Match
def reg_split(cgx, x):
  #print("reg_split call", cgx)
  #print(repr(x))
  if type(cgx) == Pattern:
    #print("Found embedded pattern")
    cgx = cgx.pattern
    if type(cgx) == str:
      #print("Evaluated cgx in reg_split")
      #print(cgx)
      cgx = eval(cgx)
      #print(cgx)
  res = re.split(cgx, x)
  #print("reg_split result")
  #print_list_without_none(res)
  return res
def reg_finditer(cgx, x):
  print("reg_split call", cgx)
  print(repr(x))
  if type(cgx) == Pattern:
    print("Found embedded pattern")
    cgx = cgx.pattern
    if type(cgx) == str:
      print("Evaluated cgx in reg_split")
      print(cgx)
      cgx = eval(cgx)
      print(cgx)
  if x[0:3] == '"""':
    print(x)
    x = x[3:-4]
    print(x)
  res = re.finditer(cgx, x)
  print("reg_split result")
  #print_list_without_none(res)
  return list(res)
def reg_lastindex(m):
  print('reg_lastindex', m, type(m))
  if type(m) == Match:
    #print("Found embedded pattern")
    m = m.mat
  return m.lastindex
def reg_full_match(m):
  if isinstance(m, Match):
    m = m.mat
  #print('reg_full_match', m, m[0])
  return m[0]
def reg_match(m):
  #print('reg_match', m, m[0])
  if isinstance(m, Match):
    m = m.mat
  return m[0]
def reg_matchleft(m):
  if isinstance(m, Match):
    m = m.mat
  return m[1]
def reg_matchright(m):
  if isinstance(m, Match):
    m = m.mat
  return m[2]
def rem_quo(s):
  return s
def reg_numgroups(cgx):
  if type(cgx) == Pattern:
    #print("Found embedded pattern")
    cgx = cgx.pattern
  if type(cgx) == str:
    print("Evaluated cgx in reg_numgroups")
    #print(cgx)
    cgx = re.compile(cgx)
  #print(cgx)
  #print(cgx.groups)
  return cgx.groups
def read_inputfile():
  global inputfile
  with open(inputfile) as f:
    contents = f.read()
    return contents
def read_termfile():
  with open('last.pml') as f:
    contents = f.read()
    return contents
def argv():
  return sys.argv
def ival(expr):
  return eval(expr)
def truncate_val(s):
  if len(str(s)) > 1000:
    return str(s)[:50] + '...'
  else:
    return str(s)
def my_eval(expr, fnvarname, fnval):
  # Add internal function defined to list of locals
  #print('locals:', locals())
  #print('globals', globals())
  if type(fnval) != Agg and fnval != -80 and fnvarname != 'eval':
    print("adding")
    lcls = { fnvarname : fnval }
  else:
    print("not adding")
    lcls = {}
  #assert fnval != -4 # special value in initialization
  try:
    #print("my_eval expr", expr)
    res = eval(expr, globals(), lcls)
    if type(res) == pattern():
      print("Found pattern", expr)
      return Pattern(expr) # weird bug
    elif type(res) == match_obj():
      print('Found a match_obj')
      return Match(expr)
    return res
  except NameError as ex:
    raise ex.with_traceback(sys.exc_info()[2])
def tval(expr, fnvarname, fnval):
  #print("tval", expr, fnvarname, fnval)
  if type(expr) == Match:
    expr = expr.mat
  lcls = { fnvarname : fnval }
  try:
    return eval(expr, {}, lcls)
  except NameError as ex:
    raise ex.with_traceback(sys.exc_info()[2])
def fval(expr, fnvarname):
  #print(expr)
  lcls = { 'reg_lastindex':reg_lastindex, 'reg_match':reg_match,
           'reg_matchleft':reg_matchleft, 'reg_matchright':reg_matchright,
           'reg_search_squote':reg_search_squote, 'reg_search_dquote':reg_search_dquote }
  try:
    return eval(fnvarname, {}, lcls)(expr)
  except NameError as ex:
    raise ex.with_traceback(sys.exc_info()[2])
def fval0(fnvarname):
  #print(expr)
  lcls = { 'reg_lastindex':reg_lastindex, 'reg_match':reg_match,
           'reg_matchleft':reg_matchleft, 'reg_matchright':reg_matchright,
           'reg_search_squote':reg_search_squote, 'reg_search_dquote':reg_search_dquote }
  add_inner_funcs(lcls)
  try:
    return eval(fnvarname, {}, lcls)()
  except NameError as ex:
    raise ex.with_traceback(sys.exc_info()[2])
def fval2(fnvarname, expr, expr2):
  #print(expr)
  lcls = { 'reg_lastindex':reg_lastindex, 'reg_match':reg_match,
           'reg_matchleft':reg_matchleft, 'reg_matchright':reg_matchright,
           'reg_search_squote':reg_search_squote, 'reg_search_dquote':reg_search_dquote }
  add_inner_funcs(lcls)
  try:
    res = eval(fnvarname, {}, lcls)(expr, expr2)
    if type(res) == pattern():
      print("Found pattern", expr)
      return Pattern(expr) # weird bug
    return res
  except NameError as ex:
    raise ex.with_traceback(sys.exc_info()[2])
def fval1(fnvarname, expr):
  #print(expr)
  lcls = { 'reg_lastindex':reg_lastindex, 'reg_match':reg_match,
           'reg_matchleft':reg_matchleft, 'reg_matchright':reg_matchright,
           'reg_search_squote':reg_search_squote, 'reg_search_dquote':reg_search_dquote }
  add_inner_funcs(lcls)
  try:
    print('fval1', fnvarname, type(fnvarname))
    if type(fnvarname).__name__ == 'function':
      res = fnvarname(expr)
    else:
      res = eval(fnvarname, {}, lcls)(expr)
    if type(res) == pattern():
      print("Found pattern", expr)
      return Pattern(expr) # weird bug
    return res
  except NameError as ex:
    raise ex.with_traceback(sys.exc_info()[2])
def fn_eval(fn, arg):
  return fn(arg)
def vval(expr):
  #print("vval", expr)
  if type(expr) == Match:
    expr = expr.mat
  try:
    return eval(expr, {}, {})
  except NameError as ex:
    raise ex.with_traceback(sys.exc_info()[2])
def split(s):
  return s.split(',')
def pattern():
  cegex = re.compile(r".*")
  return type(cegex)
def match_obj():
  cegex = re.compile(r".*")
  m = re.search(cegex, '0')
  return type(m)
def fn_kind():
  return type(fn_kind)
def match_obj2():
  m = Match(None)
  return type(m)
def doesnt_contain_triple(s):
  return '"""' not in s
def remove_quadruple(s):
  if '""""' in s:
    return s[1:-1]
  else:
    return s
idx = 0
def reset_idx():
  global idx
  idx = 0
def getnext():
  global idx
  res = idx
  idx += 1
  return res
def strp(s):
  return s.strip() if s != None else None

def finditer_lines(regex, s):
  matches = list(re.finditer(regex, s))
  if not matches:
    return []
  end = matches[-1].start()
  newline_table = {-1: 0}
  for i, m in enumerate(re.finditer('\\n', s), 1):
      offset = m.start()
      if offset > end:
          break
      newline_table[offset] = i

  for m in matches:
      newline_offset = s.rfind('\n', 0, m.start())
      line_number = newline_table[newline_offset]
      yield (m, line_number)

def lexer(s):
  if logging:
    print("Lexing")

  regex = (
          r"\s*'((?:[\^$@\"\[\]\w\s()<>:;|+\-*?.#=!]|\\)*)'\s*|" # string_single
          r"\s*\"((?:[\^$@,{}\[\]\w\s'()<>:;|+\-*?.#=!]|\\n|\\d|\\|\[\"])*)\"\s*|" # string
           "\s*(?<=[:])\s*(\w+)\s*(?=[=])\s*|"     # type
           "\s*@(\w+)\s*{\s*((?:[\[\]<>=|\.:;\w\d\s'+*\"{}\-()]|\$\$)+)\s*}(!|\?)?\s*|" # langblock
           "\s*\.\s*([\w_]+)\s*\(|"          # ufcs fncall
           "\s*(\w+)\s*\(|"                  # fncall
           "\s*\.\s*([\w_]+)\s*|"            # member access
           "\s*(ret)\s*|"                    # ret
           "\s*(def\s*\w+\([\w, ]*\))\s*|"     # fndefn
           "\s*type\s*(\w+)\s*|"             # aggregate
           "\s*(\^|binop|constant|expr|c_char_p|c_int|&\w+)\s*|" # carrot
           "\s*(\d+)\s*|"                    # numeral
           "\s*(if|else|for|in|while)\s*|" # control
           "\s*(\w+)\s*|"                    # identifier
           "\s*(\[|\]|,|;|\(|\)|<:|<=|>=|<|>|===|==|!=|:=|=|:|\*|\/|\+|-|{|})\s*" # operator or brace
           )
  cegex = re.compile(regex)
  ms = finditer_lines(cegex, s)
  #spl = list(finditer_lines(cegex, s))
  num_features = cegex.groups
  #print('num_features:', num_features)
  tokens = []
  for m, line in ms:
      last_index = m.lastindex
      mat = m[0].strip()
      print(m, line)

      stringsobj = last_index == 1
      stringobj = last_index == 2
      typ = last_index == 3
      langcode = last_index == 4
      langbody = last_index == 5
      langopt = last_index == 6
      ufcs = last_index == 7
      fncall = last_index == 8
      memberaccess = last_index == 9
      ret = last_index == 10
      fndefn = last_index == 11
      aggregate = last_index == 12
      opcar = last_index == 13
      integral = last_index == 14
      controlkw = last_index == 15
      identifier = last_index == 16
      opbrace = last_index == 17

      if stringsobj:
        mat = re.search(r"'(.*)'", mat)
        yield STR(mat[1]), line
      elif stringobj:
        mat = re.search(r"\"(.*)\"", mat)
        yield STR(mat[1]), line
      elif controlkw:
        if mat == 'if':
          yield IF(), line
        elif mat == 'else':
          yield ELSE(), line
        elif mat == 'for':
          yield FOR(), line
        elif mat == 'in':
          yield IN(), line
        elif mat == 'while':
          yield WHILE(), line
      elif opcar:
        if mat in [ 'binop', 'constant', 'expr', 'c_char_p', 'c_int' ]:
          yield KW(mat), line
        elif mat[0] == "&":
          yield KW(mat), line
        else:
          assert mat == '^'
          yield '^', line

      elif langbody or langopt:

        langmat = re.search("\s*@(\w+)\s*{\s*((?:[\[\]<>=|\.:;\w\d\s'+*\"{}\-()]|\$\$)+)\s*}(!|\?)?\s*|", m[0].strip())
        langcode = langmat[1]
        langbody = langmat[2]
        if not langmat[3]:
          langopt = '!'
        else:
          langopt = langmat[3]

        rres = re.search(r"\$\$(\w+)", langbody)
        if rres:
          langbody = re.sub(r"\$\$(\w+)", r"\1", langbody)
          yield LANGBLOCK(ID(langcode), langbody, langopt, ArgBundle([ID(rres.group(1))])), line
        else:
          yield LANGBLOCK(ID(langcode), langbody, langopt, ArgBundle([])), line

      elif ufcs:
        yield '.', line
        yield CALL(mat.lstrip('.').rstrip('('), None, None), line
        yield "(", line
      elif fncall:
        yield CALL(mat.rstrip('('), None, None), line
        yield "(", line
      elif memberaccess:
        yield '.', line
        yield MEMBER(ID(mat.lstrip('.')), None, None), line
      elif ret:
        yield RET(), line

      elif fndefn:
        fn_defn = re.search(r"def (?P<name>\w+)\((?P<param>[\w, ]*)\)", mat)
        name = fn_defn.groupdict()['name']
        param = fn_defn.groupdict()['param']
        yield FUNC(name, param), line

      elif aggregate:
        mat = re.search(r"type (\w*)", mat)
        yield AGG(mat[1]), line

      elif typ:
        yield TYPE(mat), line

      elif identifier:
        yield ID(mat), line

      elif integral:
        yield NUM(int(mat)), line

      elif opbrace:
        assert mat in [ '+', '-', '}', '{', '*', '/', ':=', '(', ')', ',', ';', '[', ']', ':', '<:', '=', '==', '!=', '===', '<', '<=', '>', '>=' ]
        yield str(mat), line #operator / brace

      else:
        raise Error

  if logging:
    print(tokens)


###########
# Parsing

def nextToken():
  global token
  global line
  try:
    try:
      token, line = next(INPUT)
    except StopIteration:
      token = 'EOT'
    return token
  except TypeError:
    global tokenidx
    tokenidx += 1
    if tokenidx >= len(INPUT):
      token = 'EOT'
    else:
      token, line = INPUT[tokenidx]
    return token

def identifier():
  if type(token) != ID:
    raise Error(type(token), token)
  tok = token
  nextToken()
  return tok

def number():
  if type(token) != NUM:
    raise Error
  tok = token
  nextToken()
  return tok

def kw():
  if type(token) != KW:
    raise Error
  tok = token
  nextToken()
  return tok

def string():
  if type(token) != STR:
    raise Error
  tok = token
  nextToken()
  return tok

def left_brace():
  if token != '{':
    raise Error
  nextToken()

def right_brace():
  if token != '}':
    raise Error
  nextToken()

# check f body in symbol table
def call():
  if type(token) != CALL:
    raise Error
  tok = token
  """
  if type(tok.arg) == list:
    print('expr_parse call')
    tok.arg = expr_parse(tok.arg)
  """
  nextToken()
  if token != "(":
    raise Error
  return tok

def langblock():
  if type(token) != LANGBLOCK:
    raise Error
  tok = token
  nextToken()
  return tok

def listt():
  tok = token
  if tok != '[':
    raise Error
  nextToken()
  LHS = expression()
  l = [ LHS ]
  while token == ',':
    nextToken()
    LHS = expression()
    l += [ LHS ]
  if token != ']':
    raise Error
  nextToken()
  return ListNode(l)

def grouping():
  tok = token
  if tok != '(':
    raise Error
  nextToken()
  expr = expression()
  if token != ')':
    raise Error
  nextToken()
  return Grouping(expr)

def typeinit():
  tok = token
  if tok != '{':
    raise Error
  nextToken()

  exprs = []
  while token != '}':
    expr = expression()
    if token != ',':
      raise Error("Forgot a comma")
    nextToken()
    exprs += [ expr ]
  nextToken()
  return AggregateObject(Bundle(exprs))

def ifelse():
  if type(token) != IF:
    raise Error
  nextToken()

  guard = expression()
  if token != '{':
    raise Error(token)
  nextToken()

  truepath = []
  while token != '}':
    stmt = statement()
    truepath += [ stmt ]
  nextToken()

  falsepath = []
  if type(token) == ELSE:
    nextToken()

    if token != '{':
      raise Error
    nextToken()

    while token != '}':
      stmt = statement()
      falsepath += [ stmt ]
    nextToken()

  return IfElseNode(guard, Bundle(truepath), Bundle(falsepath))

def ufcs_call(LHS):
  tok = token
  if type(token) != CALL:
    raise Error
  nextToken()

  if token != '(':
    raise Error
  nextToken()

  RHS = argument_list()

  if token != ')':
    raise Error
  nextToken()

  return UFCSCALL(tok.ident, LHS, RHS, line)

def member_access(LHS):
  tok = token
  if type(token) != MEMBER:
    raise Error
  nextToken()
  return MEMBER(tok.ident, LHS, line)

def factor():
  # lower precedence
  if token in [ '===', '==', '!=', '<', '<=', '>', '>=' ]:
    tok = token
    nextToken()
    return tok

  if type(token) == ID:
    return identifier()
  elif type(token) == NUM:
    return number()
  elif type(token) == CALL:
    unary_tok = token
    nextToken()

    if token != '(':
      raise Error
    nextToken()

    RHS = argument_list()
    if token != ')':
      raise Error
    nextToken()

    LHS = CALL(unary_tok.ident, RHS, line)
    return LHS
  elif type(token) == LANGBLOCK:
    return langblock()
  elif type(token) == STR:
    return string()
  elif type(token) == KW:
    return kw()
  elif token in [ '^', '-', '.' ]: #unary operators
    tok = token
    nextToken()
    return tok
  elif token in [ '[' ]:
    return listt()
  elif token in [ '(' ]:
    return grouping()
  elif token in [ '{' ]:
    return typeinit()
  elif type(token) == IF:
    return ifelse()
  else:
    raise Error("Trailing", token, "of", type(token))

def unary():
  LHS = factor()
  while token in [ '.', '[' ]:
    if token == '[':
      nextToken()

      expr = expression()
      if token != ']':
        raise Error
      nextToken()

      LHS = ARRAY_ACCESS(LHS, expr)

    elif token == '.':
      nextToken()
      if type(token) == CALL: 
        LHS = ufcs_call(LHS)
      elif type(token) == MEMBER:
        LHS = member_access(LHS)
      else:
        raise Error(token, type(token))

    else:
      raise Error(token, type(token))
  return LHS

def term():
  if token == '(':
    pass
  LHS = unary()
  # lower precedence
  if token in [ '===', '==', '!=', '<', '<=', '>', '>=' ]:
    return LHS

  if token in [ '*', '/' ]:
    op = token
    nextToken() # LL(2)
    if token in [ '^', '-' ]: # or type(token) == CALL: #unary token
      unary_tok = token
      nextToken()
      if unary_tok == '^':
        return TermNode(line, LHS, op, CarrotNode(term()))
      elif unary_tok == '-':
        return TermNode(line, LHS, op, NegateNode(term()))
      else:
        raise Error
    else:
      RHS = term()
      return TermNode(line, LHS, op, RHS)
  else:
    return LHS

def binary_terminals_4():
  return [ '*', '/', '%' ]
def binary_terminals_2():
  return [ '+', '-', ]
def binary_terminals():
  return binary_terminals_2() + binary_terminals_4()

def terminals():
  return [ ID, NUM, STR, CALL, RET, LANGBLOCK, 'EOT', KW, FUNC ] \
       + binary_terminals() \
       + [ ',' + ')', '(', '}', '{', ':=', '^' ]

def argument_list():
  arguments = []
  while token != ")":
    arguments += [ expression() ]
    if token not in [ ',', ')' ]:
      raise Error(token)
    if token == ',':
      nextToken()
  return ArgBundle(arguments)

def isPostKW(kw):
  return type(kw) == KW and kw.x in [ 'binop', 'expr', 'constant' ]

def isPreKW(kw):
  return type(kw) == KW and (kw.x in [ 'c_int', 'c_char_p' ] or kw.x[0] == '&')

def proposition():
  LHS = term()

  # lower precedence, just returns
  if token in [ '===', '==', '!=', '<', '<=', '>', '>=' ]:
    return LHS

  # extremely low precedence, terminals
  if token in [ ';', "}", ')', ']', ',', '{', 'EOT' ]:
    return LHS

  # high, unary precedence
  if (LHS == '^' and type(token) == ID or LHS == '-'):
    if LHS == '^':
      tok = token
      nextToken()
      LHSLHS = CarrotNode(tok)
    elif LHS == '-':
      tok = token
      nextToken()
      LHSLHS = NegateNode(tok)

    if token in [ "+", '-', '*', '/' ]: # LL(2) ~ check if a binary operator
      operator_tok = token
      nextToken() # LL(2)
      RHS = expression()
      if operator_tok in [ '+', '-' ]:
        return ExprNode(LHSLHS, operator_tok, RHS, line)
      elif operator_tok in [ '*', '/', '%' ]:
        return TermNode(line, LHSLHS, operator_tok, RHS)
      else:
        raise Error(token, LHS, RHS)
    #elif token in [ ')', ',' ]: # LL(2) ~ check if terminal
    #  return CarrotNode(ident_tok)
    else:
      return LHSLHS

  if type(token) in [ ID, NUM, STR, CALL, RET, FUNC, IF, FOR, IN, WHILE, AGG ]:
    return LHS

  elif isPostKW(token):
    RHS = proposition()
    return CarrotExpr(LHS, RHS)

  elif isPostKW(LHS) and token == "^":
    return CarrotExpr(LHS, proposition())

  elif LHS == "^" and type(token) in [ LANGBLOCK ]:
    assert LHS == "^"
    RHS = proposition()
    return CarrotNode(RHS)

  elif LHS == "^" and isPreKW(token):
    return CarrotNode(proposition())

  elif isPreKW(LHS):
    if type(token) in [ LANGBLOCK, KW ]:
      return PreCarrotExpr(LHS, proposition())
    else:
      raise Error('Expected langblock or prekw following prekw, but found', token, type(token))

  elif token in [ '+', '-' ]:
    op = token
    nextToken()
    RHS = proposition()
    return ExprNode(LHS, op, RHS, line)

  else:
    raise Error(token, type(token))

def comparison():
  LHS = proposition()
  while token in [ '<=', '<', '>', '>=' ]:
    op = token
    nextToken()
    RHS = proposition()
    LHS = CompareNode(LHS, op, RHS)
  return LHS

def expression():
  LHS = comparison()
  while token in [ '==', '!=', '===' ]:
    op = token
    nextToken()
    RHS = comparison()
    LHS = CompareNode(LHS, op, RHS)
  return LHS

def expr_bundle():
  LHS = expression()
  while token in [';']:
    nextToken()
    expr = expression()
    LHS = TUPLE_BUNDLE(LHS, expr)
  return LHS

def assignment(LHS):
  tok = token
  if tok not in [ ':=', '=', ':', '<:' ]:
    raise Error(LHS, tok)
  nextToken()

  if tok == ':=' or tok == ':':
    RHS = expr_bundle()
    return AssignNode(LHS, RHS)
  elif tok == '=':
    RHS = expr_bundle()
    return AssignMutableNode(LHS, RHS)
  elif tok == '<:':
    if type(token) == ID:
      typ = token
      nextToken()

      if token != '=' and token != ':':
        raise Error(token)
      nextToken()
    else:
      typ = None

    RHS = expr_bundle()
    return AssignTypeNode(LHS, RHS, TYPE(typ))
  else:
    raise Error(LHS, tok, token, RHS)

def function_definition():
  tok = token
  nextToken()
  left_brace()
  stmts = []
  while token != "}":
    stmts.append(statement())
  right_brace()
  return FuncNode(tok.ident, tok.param, FuncBundle(stmts))

def print_statement():
  tok = token
  nextToken()
  ret = PRINT(tok.arg)
  assert(ret == tok)
  return tok

def call_statement():
  tok = call()

  if token != '(':
    raise Error
  nextToken()

  RHS = argument_list()
  if token != ')':
    raise Error
  nextToken()

  return CALL(tok.ident, RHS, line)

def return_statement():
  tok = token
  nextToken() #RET itself
  expr = expr_bundle()
  return ReturnNode(expr)

def if_statement():
  return ifelse()

def forloop_statement():

  if type(token) != FOR:
    raise Error
  nextToken()

  loop_var = identifier()

  if type(token) != IN:
    raise Error
  nextToken()

  iterable = expression()

  if token != "{":
    raise Error
  nextToken()

  body = []
  while token != '}':
    stmt = statement()
    body += [ stmt ]
  nextToken()

  return ForLoopNode(loop_var, iterable, Bundle(body))

def whileloop_statement():

  if type(token) != WHILE:
    raise Error
  nextToken()

  condition = expression()

  if token != "{":
    raise Error
  nextToken()

  body = []
  while token != '}':
    stmt = statement()
    body += [ stmt ]
  nextToken()

  return WhileLoopNode(condition, Bundle(body))


def aggregate():
  agg_tok = token
  if type(token) != AGG:
    raise Error
  nextToken()

  if token != '{':
    raise Error
  nextToken()

  fields = []
  while token != '}':
    ident = identifier()
    if token == ':':
      nextToken()
      typ = token
    else:
      typ = None
    fields += [ (ident, typ) ]
  nextToken()
  return AggregateNode(agg_tok.val, Bundle(fields))

def statement():
  if type(token) == FUNC:
    return function_definition()
  elif type(token) == RET:
    return return_statement()
  elif type(token) == CALL:
    return call_statement()
  elif type(token) == IF:
    return if_statement()
  elif type(token) == FOR:
    return forloop_statement()
  elif type(token) == WHILE:
    return whileloop_statement()
  elif type(token) == AGG:
    return aggregate()
  elif type(token) == ID:
    LHS = identifier()
    while token == ';':
      nextToken()

      expr = identifier()
      LHS = IDENTIFIER_BUNDLE(LHS, expr)
      if token in [ ':=', ':', '=', '<:' ]: break

    if token in [ ':=', ':', '=', '<:' ]:
      return assignment(LHS)
    elif token in ['.']:
      nextToken()
      return ufcs_call(LHS)
    return LHS
  else:
    return expression()

def statements():
  stmts = []
  while token != 'EOT':
    x = statement()
    stmts.append(x)
  return stmts

# top-down ll parse
# tokens as a global generator
def program_parse(tokens):
  if logging:
    print("Parsing")
  global INPUT, tokenidx, token
  INPUT=tokens
  tokenidx = -1
  token = nextToken()

  stmts = statements()
  if logging:
    print( stmts )
  return stmts
 

###########
# Evaluation

def eval_pystmt(stmt):
  assert(isinstance(stmt, ast.AST))
  if type(stmt) == ast.Module:
    print(type(stmt), stmt)
  elif type(stmt) == ast.BinOp:
    valL = eval_pystmt(stmt.left)
    valR = eval_pystmt(stmt.right)
    if stmt.op == '+':
      return valL + valR
  elif type(stmt) == ast.Call:
    print ("#### Call:")
    print(type(stmt), stmt)#, dir(stmt))

    # Name [Constant] []
    print(stmt.func, stmt.args, stmt.keywords)

    eval_pystmt(stmt.func)
    eval_pystmt(stmt.args[-1])

  elif type(stmt) == ast.Constant:
    print ("#### Constant:")
    print(type(stmt), stmt, dir(stmt))

    print(stmt.value, stmt.kind, stmt.n, stmt.s)
    return stmt.value
  elif type(stmt) == ast.Name:
    print ("#### Name:")
    print(type(stmt), stmt)#, dir(stmt))
    
    print(stmt.id, stmt.ctx)

    eval_pystmt(stmt.ctx)
  
  elif type(stmt) == ast.Add:
    print ("#### Add:")
    print(type(stmt), stmt, dir(stmt))

  elif type(stmt) == ast.Load:
    print ("#### Load:")
    print(type(stmt), stmt, dir(stmt))

  else:
    print(type(stmt), stmt)
    #raise Error

@dataclass
class FuncLib():
  library:Union[dict, ctypes.CDLL]
  c:bool

@dataclass
class AbstractObject():
  s:Union[str, int]
  l:str
  e:bool
  globs:dict
  locs:dict
  #language:str # python
  def __str__(s):
    return 'AbstractObject s:' + str(s.s) + ' l:' + s.l + ' e:'+str(s.e) + ' locs=' + str(s.locs)
  def __repr__(s):
    return s.__str__()
  def __call__(s):
    global libs
    # if python
    if s.l == 'l' and s.e:
      AST = ast.parse(s.s)
      if type(AST) == ast.Module:
        if type(AST.body[-1]) == ast.Return:
          AST.body[-1] = AST.body[-1].value # remove optional last return of langblock node
        elif type(AST.body[-1]) == ast.If:
          if type(AST.body[-1].body[-1]) == ast.Return:
            AST.body[-1].body[-1] = AST.body[-1].body[-1].value # remove optional last return of langblock node
      stmts = []
      for aststmt in AST.body:
        newstr = ast.unparse(aststmt)
        stmts.append(newstr)
      outer_level = True
      glbs, lcls = s.globs, s.locs
      for stmt in stmts:
        astmt = ast.parse(stmt).body[-1]
        if type(astmt) in [ ast.Import, ast.Assign, ast.If, ast.FunctionDef ]:
          if (type(astmt) == ast.If):
            print(astmt.body)
            print(dir(astmt))
            raise Error
          exec(stmt, glbs, lcls)
        elif type(astmt) == ast.Expr and outer_level:
          return eval(stmt, glbs, lcls)
        elif type(astmt) == ast.Expr:
          eval(stmt, glbs, lcls)
        else:
          raise Error(type(astmt), ast.parse(stmt).body)
      return FuncLib(lcls, False)
    elif s.l == 'l':
      raise Error('unimplemented')
    elif s.l == 'c' and s.e:
      x = tempfile.mkstemp()
      cfile = x[1] + '.c'
      os.rename(x[1], cfile)

      preamble = 'int main() {\n'
      postamble = '\n}'
      f = open(cfile, 'w')
      f.write(preamble + s.s + postamble)
      f.close()

      assert False
      c = new_compiler()
      l = c.compile([cfile])
      _,tail = os.path.split(x[1])
      objname = tail+'obj'
      #c.link_executable(l, objname)
      libs += l
      c.link_shared_lib(l, objname)

      #library = ctypes.CDLL(os.path.join(os.getcwd(), libname))
      #yy = os.system('./'+objname)
      libname = 'lib' + objname + '.so'
      libs += [ libname ]
      library = ctypes.CDLL(os.path.join(os.getcwd(), libname))

      t = True #if expr.type == '!' else False
      #ef = ExtractFunc(AbstractObject(FuncLib(library), s.l, t, {}, {}), "main")
      #return ReturnConversion(ef, ctypes.c_int)
      return FuncLib(library, True)
      raise Error
    elif s.l == 'c':
      x = tempfile.mkstemp()
      cfile = x[1] + '.c'
      os.rename(x[1], cfile)

      with open(cfile, 'w') as f:
        f.write(s.s)
        f.close()

      assert False
      c = new_compiler()
      l = c.compile([cfile])
      libs += l
      _,tail = os.path.split(x[1])
      objname = tail+'obj'
      c.link_shared_lib(l, objname)

      libname = 'lib' + objname + '.so'
      libs += [ libname ]
      library = ctypes.CDLL(os.path.join(os.getcwd(), libname))
      return FuncLib(library, False)
    elif s.l == 'b':
      x = os.system('bash.exe -c ' + s.s)
      return x
    elif s.l == 'm':
      x = os.system('cmd.exe /c ' + s.s)
      return x
    else:
      raise Error("invalid language", s.l)

libs = []
@atexit.register
def remove_objs():
  for lib in libs: os.remove(lib)
  try:
    os.rmdir('tmp')
  except FileNotFoundError:
    pass

@dataclass
class ExtractFunc():
  ao:AbstractObject
  name:str

@dataclass
class ReturnConversion():
  ef:ExtractFunc
  ctyp:str

def rewrite(body, bound_var, new_expr):
  #print('rewrite(', body, bound_var, new_expr, ')')
  if type(body) == ExprNode:
    return ExprNode(rewrite(body.lhs, bound_var, new_expr), body.op,
                    rewrite(body.rhs, bound_var, new_expr), body.line)
  elif type(body) == TermNode:
    if body.op:
      return TermNode(body.line, rewrite(body.lhs, bound_var, new_expr), body.op,
                      rewrite(body.rhs, bound_var, new_expr))
    else:
      return TermNode(body.line, rewrite(body.lhs, bound_var, new_expr))
  elif type(body) == AssignNode:
    return AssignNode(body.lhs, rewrite(body.rhs, bound_var, new_expr))
  elif type(body) == AssignTypeNode:
    return AssignTypeNode(body.lhs, rewrite(body.rhs, bound_var, new_expr), body.typ)
  elif type(body) == AssignMutableNode:
    return AssignMutableNode(body.lhs, rewrite(body.rhs, bound_var, new_expr))
  elif type(body) == ID:
    #print('body', body, 'bound_var', bound_var, 'new_expr', new_expr)
    if body == bound_var:
      return new_expr
    else:
      return body
  elif type(body) == NUM:
    return body
  elif type(body) == CLOSURE:
    return body
  elif type(body) == STR:
    return body
  elif type(body) == int:
    return body
  elif type(body) == str:
    if type(bound_var) == ID and body == bound_var.val:
      return new_expr
    return body
  elif type(body) == FuncNode:
    return FuncNode(body.name, body.param, rewrite(body.body, bound_var, new_expr)) # TODO: add ReturnNode here
  elif type(body) == ReturnNode:
    return ReturnNode(rewrite(body.expr, bound_var, new_expr))
  elif type(body) == NegateNode:
    return NegateNode(rewrite(body.expr, bound_var, new_expr))
  elif type(body) == ForLoopNode:
    return ForLoopNode(body.ident, rewrite(body.iterable, bound_var, new_expr), rewrite(body.body, bound_var, new_expr))
  elif type(body) == WhileLoopNode:
    return WhileLoopNode(rewrite(body.condition, bound_var, new_expr), rewrite(body.body, bound_var, new_expr))
  elif type(body) == IfElseNode:
    return IfElseNode(rewrite(body.c, bound_var, new_expr),
                      rewrite(body.t, bound_var, new_expr),
                      rewrite(body.f, bound_var, new_expr))
  elif type(body) == CompareNode:
    return CompareNode(rewrite(body.lhs, bound_var, new_expr),
                       body.op,
                       rewrite(body.rhs, bound_var, new_expr))
  elif type(body) == ListNode:
    ret_list = []
    for expr in body.l:
      ret_list.append(rewrite(expr, bound_var, new_expr))
    return ListNode(ret_list)
  elif type(body) == Bundle:
    ret_list = []
    for stmt in body.x:
      ret_list.append(rewrite(stmt, bound_var, new_expr))
    return Bundle(ret_list)
  elif type(body) == ArgBundle:
    ret_list = []
    for stmt in body.x:
      ret_list.append(rewrite(stmt, bound_var, new_expr))
    return ArgBundle(ret_list)
  elif type(body) == FuncBundle:
    ret_list = []
    for stmt in body.x:
      ret_list.append(rewrite(stmt, bound_var, new_expr))
    return FuncBundle(ret_list)
  elif type(body) == CALL:
    # FIXME ID -> .val
    return CALL(rewrite(body.ident, bound_var, new_expr), rewrite(body.arg, bound_var, new_expr), body.line) # FIXME
  elif type(body) == UFCSCALL:
    # FIXME ID -> .val
    return UFCSCALL(rewrite(body.ident, bound_var, new_expr), rewrite(body.this, bound_var, new_expr), rewrite(body.arg, bound_var, new_expr)) # FIXME
  elif type(body) == ARRAY_ACCESS:
    return ARRAY_ACCESS(rewrite(body.arr, bound_var, new_expr), rewrite(body.idx, bound_var, new_expr))
  elif type(body) == MEMBER:
    return MEMBER(rewrite(body.ident, bound_var, new_expr), rewrite(body.this, bound_var, new_expr), body.line)
  elif type(body) == AggregateObject:
    ret_list = []
    for expr in body.exprs.unbundle():
      ret_list.append(rewrite(expr, bound_var, new_expr))
    return AggregateObject(Bundle(ret_list))
  elif type(body) in [ LANGBLOCK, CarrotNode, CarrotExpr ]:
    return body
  elif type(body) in [ TUPLE_BUNDLE ]:
    return TUPLE_BUNDLE(rewrite(body.nesting, bound_var, new_expr), rewrite(body.elem, bound_var, new_expr))
  else:
    raise Error(type(body), body, bound_var, new_expr)

def match_all(last_index):
  print('match_all', last_index)

  first = CompareNode(last_index, '==', 1)
  second = CompareNode(last_index, '==', 2)
  third = CompareNode(last_index, '==', 3)
  fourth = CompareNode(last_index, '==', 4)
  fifth = CompareNode(last_index, '==', 5)
  sixth = CompareNode(last_index, '==', 6)
  seventh = CompareNode(last_index, '==', 7)
  eighth = CompareNode(last_index, '==', 8)
  ninth = CompareNode(last_index, '==', 9)
  tenth = CompareNode(last_index, '==', 10)
  eleventh = CompareNode(last_index, '==', 11)
  twelfth = CompareNode(last_index, '==', 12)
  thirteenth = CompareNode(last_index, '==', 13)
  fourteenth = CompareNode(last_index, '==', 14)
  fifteenth = CompareNode(last_index, '==', 15)
  sixteenth = CompareNode(last_index, '==', 16)
  a = CompareNode(last_index, '==', 17)
  b = CompareNode(last_index, '==', 18)
  c = CompareNode(last_index, '==', 19)
  d = CompareNode(last_index, '==', 20)
  e = CompareNode(last_index, '==', 21)
  f = CompareNode(last_index, '==', 22)
  g = CompareNode(last_index, '==', 23)
  h = CompareNode(last_index, '==', 24)
  i = CompareNode(last_index, '==', 25)
  j = CompareNode(last_index, '==', 26)
  return INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(INNER_BUNDLE(first, second), third), fourth), fifth), sixth), seventh), eighth), ninth), tenth), eleventh), twelfth), thirteenth), fourteenth), fifteenth), sixteenth), a), b), c), d), e), f), g), h), i), j)

def evaluate_tuple_bundle(expr, symtab, gsymtab):
  res = evaluate(expr.elem, symtab, gsymtab)
  if type(expr.nesting) == TUPLE_BUNDLE:
    resnesting = evaluate_tuple_bundle(expr.nesting, symtab, gsymtab)
    return EVAL_BUNDLE(resnesting, res)
  else:
    resnesting = evaluate(expr.nesting, symtab, gsymtab)
    return EVAL_BUNDLE(resnesting, res)

@functools.singledispatch
def evaluate(expr, symtab, global_symtab, function=None):
  global types
  #print(type(expr))
  if type(expr) == tuple:
    return list(expr)
  elif type(expr) == list:
    return expr
  elif type(expr) == ast.BinOp:
    return expr
  elif type(expr) == ast.Module:
    return expr
  elif type(expr) == ast.Expr:
    return expr
  elif type(expr) == ast.Constant:
    return expr
  elif type(expr) == ctypes.CDLL:
    return expr
  elif type(expr) == FuncLib:
    return expr
  elif repr(type(expr)) == "<class 'ctypes.CDLL.__init__.<locals>._FuncPtr'>":
    return expr
  elif type(expr) == AssignTypeNode:
    #print('assign type node', expr)
    none = assigntype_eval(symtab, global_symtab, types, expr.typ, expr.lhs, TYPED(evaluate(expr.rhs, symtab, global_symtab), expr.typ))
    return none
  elif type(expr) == AssignNode:
    none = assign_eval(symtab, global_symtab, expr.lhs, expr.rhs)
    return none
  elif type(expr) == AssignMutableNode:
    none = assignmut_eval(symtab, global_symtab, expr.lhs, expr.rhs)
    return none
  elif type(expr) == ReturnNode:
    return expr #evaluate(expr.expr, symtab)
  elif type(expr) == FuncNode:
    none = funcdef_eval(symtab, expr.name, ID(expr.param), expr.body)
    return none
  elif type(expr) == TUPLE_BUNDLE:
    return evaluate_tuple_bundle(expr, symtab, global_symtab)
  elif type(expr) == EVAL_BUNDLE:
    return expr
  elif type(expr) == INNER_BUNDLE:
    return expr
  elif type(expr) == TYPE:
    return expr.name
    return expr # HMM:
  elif type(expr) == Context:
    return expr
  elif type(expr) in [ re.Pattern, Pattern, re.Match, Match ]:
    return expr
  elif type(expr).__name__ in [ 'NoneType' ]:
    return expr
  elif type(expr) in [ dict, set, type ]:
    return expr
  elif type(expr).__name__ in [ 'builtin_function_or_method' ]:
    return expr
  elif type(expr).__name__ in [ 'function' ]:
    return expr
  else:
    #return expr
    raise Error(type(expr), repr(type(expr)), expr)

@evaluate.register
def _(expr: ID2, symtab, gsymtab, function = None):
  return evaluate(ID(expr.val), symtab, gsymtab, function)

@evaluate.register
def _(expr: ID, symtab, global_symtab, function=None):
  if function:
    return evaluate(symtab[function][expr], symtab, global_symtab)
  else:
    # types only declare ID and string in symtab
    istype = (expr in symtab and expr.val in symtab) or\
             (expr in global_symtab and expr.val in global_symtab)

    if expr.val in symtab:
      res = evaluate(symtab[expr.val], symtab, global_symtab)
      if istype:
        return TYPE(res)
      else:
        return res
    elif expr.val in global_symtab:
      res = evaluate(global_symtab[expr.val], symtab, global_symtab)
      if istype:
        return TYPE(res)
      else:
        return res
    else:
      if expr in global_symtab:
        res = evaluate(global_symtab[expr], symtab, global_symtab)
        return res
      raise Error("Undeclared", expr, expr.line)

@evaluate.register
def _(expr: NUM, symtab, global_symtab, function=None): return expr.val

@evaluate.register
def _(expr: CLOSURE, symtab, global_symtab, function=None): return expr.val

@evaluate.register
def _(expr: AggregateObject, symtab, gsymtab, function=None):
  """
  res = []
  for e in expr.exprs.unbundle():
    res.append(evaluate(e, symtab))
  return AggregateObject(Bundle(res))
  """
  return expr

@dataclass
class Agg():

  def __type__(s):
    return "AGG_TYPE"

  def __str__(s):
    res = "Agg<" + s.typename.val + ">("
    for val, typ in s.types.unbundle():
      if val.val != 'tokidx':
        if hasattr(s, val.val):
          g = getattr(s, val.val)
          if type(g).__name__ == "Agg":
            #res += str(val.val) + " " + "AGGTY" + ","
            res += str(val.val) + " " + str(repr(g)) + ","
          else:
            #print('g3', s.typename, type(s.typename), val.val, type(g).__name__)
            if type(g).__name__ == 'function':
              res += str(val.val) + ' FUNCTION,'
            elif type(g) == str:
              #print('gc', g)
              R = repr(g)
              S = str(R)
              res += str(val.val) + " " + S + ","
            elif type(g) == int:
              R = repr(g)
              S = str(R)
              res += str(val.val) + " " + S + ","
            elif type(g) == list:
              R = repr(g)
              S = str(R)
              res += str(val.val) + " " + S + ","
            else:
              res += str(val.val) + ' NONSTR,'
        else:
          res += str(val.val) + " " + "???" + ","
    return res + ")"

  def __repr__(s):
    if type(s) == "AGG_TYPE":
      return "AGGTY"
    else:
      return str(s)

  def __eq__(s, o):
    #print('eq', s, o)
    if type(o) != Agg:
      return False
    if len(s.types.unbundle()) != len(o.types.unbundle()):
      return False

    for idx, (val, typ) in enumerate(s.types.unbundle()):
      val2, typ2 = o.types.unbundle()[idx]
      if getattr(s, val.val) != getattr(o, val2.val):
        return False

    return True

  def __hash__(s):
    return hash(s.typename)

@dataclass
class STRICT():
  expr:ExprNode
  aggType: TYPE
  def __eq__(s, o):
    if len(s.expr.types.unbundle()) != len(o.expr.types.unbundle()):
      return False
    if s.aggType.val != o.aggType.val:
      #print('STRICT', s.aggType, o.aggType)
      return False

    for idx, (val, typ) in enumerate(s.expr.types.unbundle()):
      val2, typ2 = o.expr.types.unbundle()[idx]
      #print('STRICT', val, typ, val2, typ2)
      if getattr(s.expr, val.val) != getattr(o.expr, val2.val) or typ != typ2:
        return False

    return True

@evaluate.register
def _(expr: MEMBER, symtab, global_symtab, function=None):
  #print('eval MEMBER', type(expr.this))
  x = expr.this
  if type(x) == str:
      if x == 'reg_lastindex':
        res = reg_lastindex
      elif x == 'reg_match':
        res = reg_match
      elif x == 'reg_matchleft':
        res = reg_matchleft
      elif x == 'reg_matchright':
        res = reg_matchright
      elif x == 'reg_search_squote':
        res = reg_search_squote
      elif x == 'reg_search_dquote':
        res = reg_search_dquote
      else:
        print('x', x)
        try:
          res = evaluate(x, symtab, global_symtab)
        except Exception as ex:
          raise Error(x, type(x), expr, ex)
  elif type(x) == Agg:
    #print('eval MEMBER typename', x.typename)
    #print('types', x.types)
    #print('agg', x)
    res = evaluate(x, symtab, global_symtab)
  elif type(x) == CLOSURE:
    #print(x.val)
    res = evaluate(x, symtab, global_symtab)
  else:
    try:
      res = evaluate(x, symtab, global_symtab)
    except Exception as ex:
      raise Error(expr, ex)
  agg = eval_context(res, symtab, global_symtab)
  #print(agg, expr)
  try:
    return evaluate(getattr(agg, expr.ident.val), symtab, global_symtab)
  except AttributeError as ex:
     raise Error(ex, agg, expr.ident.val, expr.line)

@evaluate.register
def _(expr: ARRAY_ACCESS, symtab, global_symtab, function=None):
  arrres = evaluate(expr.arr, symtab, global_symtab)
  idxres = evaluate(expr.idx, symtab, global_symtab)
  #print('array_access', idxres)
  return arrres[idxres]

def insp(x):
  if inspect.ismodule(x): return 'module'
  elif inspect.isclass(x): return 'class'
  elif inspect.ismethod(x): return 'method'
  elif inspect.isfunction(x): return 'function'
  elif inspect.isgeneratorfunction(x): return 'generatorfunction'
  elif inspect.isgenerator(x): return 'generator'
  elif inspect.iscoroutinefunction(x): return 'coroutinefunction'
  elif inspect.iscoroutine(x): return 'coroutine'
  elif inspect.isawaitable(x): return 'awaitable'
  elif inspect.isasyncgenfunction(x): return 'asyncgenfunction'
  elif inspect.istraceback(x): return 'traceback'
  elif inspect.isframe(x): return 'frame'
  elif inspect.iscode(x): return 'code'
  elif inspect.isbuiltin(x): return 'builtin'
  elif inspect.isabstract(x): return 'abstract'
  elif inspect.ismethoddescriptor(x): return 'methoddescriptor'
  elif inspect.isdatadescriptor(x): return 'datadescriptor'
  elif inspect.isgetsetdescriptor(x): return 'getsetdescriptor'
  elif inspect.ismemberdescriptor(x): return 'memberdesciptor'
  else: return 'otherinsp'

@evaluate.register
def _(expr: Agg, symtab, global_symtab, function=None):
  #print('eval Agg', expr, expr.typename, expr.types)
  for val, typ in expr.types.unbundle():
    g = getattr(expr, val.val)
    #print('g', g, type(g), expr, val)
    if type(g) == str:
      if g == 'reg_lastindex':
        res = reg_lastindex
      elif g == 'reg_match':
        res = reg_match
      elif g == 'reg_matchleft':
        res = reg_matchleft
      elif g == 'reg_matchright':
        res = reg_matchright
      elif g == 'reg_search_squote':
        res = reg_search_squote
      elif g == 'reg_search_dquote':
        res = reg_search_dquote
      else:
        #print('g', g)
        try:
          res = evaluate(g, symtab, global_symtab)
        except Exception as ex:
          raise Error(g, expr, ex)
    else:
      try:
        #print('g2', type(g), insp(g))
        if type(g).__name__ == 'function':
          res = g
        else:
          res = evaluate(g, symtab, global_symtab)
      except Exception as ex:
        raise Error(g, expr, ex)
    setattr(expr, val.val, res)
  return expr

@evaluate.register
def _(expr: TYPED, symtab, global_symtab, function=None):
  exprs = expr.expr.exprs.unbundle()
  if expr.typ.name in symtab:
    thesetypes = symtab[expr.typ.name]
  elif expr.typ.name in global_symtab:
    thesetypes = global_symtab[expr.typ.name]
  else:
    raise Error("Invalid typename", expr.typ.name)
    #assert expr.typ.name in types
    #thesetypes = types[expr.typ.name]
  aggr = Agg()
  for idx, exp in enumerate(exprs):
    res = evaluate(exp, symtab, global_symtab)
    ty = thesetypes.unbundle()[idx]
    setattr(aggr, ty[0].val, res)
  aggr.typename = expr.typ.name
  aggr.types = thesetypes
  # TODO: set aggr's __str__and/or__repr__ methods
  return aggr

@evaluate.register
def _(expr: Grouping, symtab, global_symtab, function=None):
  res = evaluate(expr.expr, symtab, global_symtab)
  return res

@evaluate.register
def _(expr: ListNode, symtab, global_symtab, function=None):
  ret = [ ]
  for e in expr.l:
    ret.append(evaluate(e, symtab, global_symtab))
  return ret

@evaluate.register
def _(expr: IfElseNode, symtab, global_symtab, function=None):
  cond = evaluate(expr.c, symtab, global_symtab)
  if cond:
    #beforesym = copy.deepcopy(symtab)
    trueres = evaluate(expr.t, symtab, global_symtab)
    if type(trueres) == ReturnNode:
      return trueres # return returns on return
    # remove symbols defined within inner scope
    #symtab = defaultdict(Union[int, defaultdict], { k:v for k, v in symtab.items() if k in beforesym })
    return trueres
  else:
    if expr.f.x != []:
      #copysym = copy.deepcopy(symtab)
      falseres = evaluate(expr.f, symtab, global_symtab)
      if type(falseres) == ReturnNode:
        return falseres # return returns on return
      #symtab = defaultdict(Union[int, defaultdict], { k:v for k, v in symtab.items() if k in copysym })
      return falseres
    else:
      return None

@evaluate.register
def _(expr: ForLoopNode, symtab, global_symtab, function=None):
  loop_var = expr.ident
  iterable = evaluate(expr.iterable, symtab, global_symtab)
  for elem in iterable:
    if type(elem) == re.Match:
      elem = Match(elem)
    symtab[loop_var.val] = elem
    for stmt in expr.body.x:
      res = evaluate(stmt, symtab, global_symtab)
      if type(res) == ReturnNode:
        return res # return returns on return
  return None

@evaluate.register
def _(expr: WhileLoopNode, symtab, global_symtab, function=None):
  condition = evaluate(expr.condition, symtab, global_symtab)
  while condition:
    for stmt in expr.body.x:
      res = evaluate(stmt, symtab, global_symtab)
      if type(res) == ReturnNode:
        return res # return returns on return
    condition = evaluate(expr.condition, symtab, global_symtab)
  return None

@evaluate.register
def _(expr: int, symtab, global_symtab, function=None): return expr

@evaluate.register
def _(expr: STR, symtab, gsymtab, function=None):
  res = expr.val.replace('\\n', '<MARKER\vMARKER>')
  res = res.replace('<MARKER\vMARKER>', '\n')
  return res

@evaluate.register
def _(expr: str, symtab, gsymtab, function=None): return expr

@evaluate.register
def _(expr: TUPLE, symtab, gsymtab, function=None):
  res = []
  for elem in expr.val.split(','):
    res += [ evaluate(symtab[elem], symtab, gsymtab) ]
  return tuple(res)

def evaluate_builtin_step(expr, symtab, gsymtab, function=None):
  if type(expr.arg) == ArgBundle:
    try:
      inner_expr = expr.arg.x[0]
    except IndexError:
      inner_expr = expr.this
  else:
    inner_expr = expr.arg
  # get lhs, rhs from one of symtab as ID or BinOp or directly as BinOp
  if type(inner_expr) == CALL:
    inner_expr = evaluate(inner_expr, symtab, gsymtab)
  elif type(inner_expr) == UFCSCALL:
    inner_expr = evaluate(inner_expr, symtab, gsymtab)

  if type(inner_expr) == ID:
    if inner_expr.val in symtab:
      res = symtab[inner_expr.val]
    else:
      res = gsymtab[inner_expr.val]

    if type(res) == ast.BinOp:
      lhs, op, rhs = res.left, res.op, res.right
    elif type(res) == ast.Constant:
      return res.value
    elif type(res) == int:
      return res
    else:
      lhs, op, rhs = res
  elif type(inner_expr) == ast.BinOp:
    lhs, op, rhs = inner_expr.left, inner_expr.op, inner_expr.right
  elif type(inner_expr) == types.GeneratorType:
    assert False
  else:
    raise Error(type(inner_expr), inner_expr)
  # return lhs + rhs based on appropriate constant representation (either native or reflected)
  if type(lhs) == ast.Constant and type(rhs) == ast.Constant:
    if type(op) == ast.Add:
      return lhs.value + rhs.value
    elif type(op) == ast.Sub:
      return lhs.value - rhs.value
    else:
      raise Error
  elif type(lhs) == int and type(rhs) == ast.Constant:
    if type(op) == ast.Add:
      return lhs + rhs.value
    elif type(op) == ast.Sub:
      return lhs - rhs.value
    else:
      raise Error
  elif type(lhs) == ast.Constant and type(rhs) == int:
    if type(op) == ast.Add:
      return lhs.value + rhs
    elif type(op) == ast.Sub:
      return lhs.value - rhs
    else:
      raise Error
  # recurse on inner node as operation cannot be performed on syntax
  elif type(lhs) == ast.Constant and type(rhs) == ast.BinOp:
    return ast.BinOp(lhs, op, evaluate(CALL('step', rhs, None), symtab, gsymtab))
  elif type(lhs) == ast.BinOp and type(rhs) == ast.Constant:
    return ast.BinOp(evaluate(CALL('step', lhs, None), symtab, gsymtab), op, rhs)
  elif type(lhs) == ast.BinOp and type(rhs) == ast.BinOp:
    return ast.BinOp(evaluate(CALL('step', lhs, None), symtab, gsymtab), op,
                     evaluate(CALL('step', rhs, None), symtab, gsymtab))
  # preserve structures
  elif type(op) in [ ast.Sub, ast.Add ]:
    return ast.BinOp(lhs, op, rhs)
  else:
    raise Error('Missing type:', op, 'lhs, rhs:', lhs, rhs)

def ishashable(x):
  try: hash(x); return True
  except TypeError as ex: return False

def eval_builtin_ty(expr):
  global symtab, global_symtab
  return evaluate_builtin_type(expr, symtab, global_symtab)

def evaluate_builtin_type(expr, symtab, global_symtab):
  arg = expr.arg.unbundle()[0]

  if type(arg) == ID:
    arg = arg.val
  elif type(arg) in [ MEMBER, CLOSURE]: # what?
    arg = evaluate(arg, symtab, global_symtab)
  else:
    raise Error(type(arg), arg)

  # explicitly declared
  if ishashable(arg) and arg in types:
    ty = types[arg]
    if type(ty.name) == ID:
      if ty.name.val in symtab:
        internalized = symtab[ty.name.val]
      else:
        internalized = global_symtab[ty.name.val]
      return TYPE(internalized)
    else:
      raise Error(type(ty.name), ty)

  # implicitly inferred
  if type(arg) == str and arg in symtab:
    arg = symtab[arg]

  if type(arg) == Agg:
    name = arg.typename.val
    if name in symtab:
      internalized = symtab[name]
    else:
      internalized = global_symtab[name]
    return TYPE(internalized)
  elif type(arg).__name__ in [ 'builtin_function_or_method' ]:
    return type(arg)
  elif type(arg) in [ int, list, str, bool, type, re.Pattern, Pattern, re.Match, Match ]:
    return type(arg)
  elif type(arg) in [ TYPE ]:
    return arg # HMM: type(arg)
  elif type(arg).__name__ in [ 'NoneType' ]:
    return type(arg)
  else:
    raise Error("No type belonging to", arg, "of", type(arg), 'on line', expr.line)

@evaluate.register
def _(expr: UFCSCALL, symtab, global_symtab, function=None):
  if expr.ident == 'print':
    res = eval_print(expr.this, symtab, global_symtab)
    return res
  elif expr.ident == 'step':
    return evaluate_builtin_step(expr, symtab, global_symtab, function)
  elif expr.ident == 'ty':
    return evaluate_builtin_type(expr, symtab, global_symtab)

  if type(expr.arg) != ArgBundle:
    raise Error
  args = evaluate(expr.arg, symtab, global_symtab)

  if type(expr.this) == MEMBER:
    exprthis = evaluate(expr.this, symtab, global_symtab)

    lcls = { k: v.val if type(v)==STR else v for k, v in symtab.items() }
    if expr.this.this.val in symtab:
      aggr = symtab[expr.this.this.val]
      key = expr.this.ident.val
      val = getattr(aggr, key)
      lcls[key] = val

    gbls = { k: v.val if type(v)==STR else v for k, v in global_symtab.items() }
    if expr.this.this.val in global_symtab:
      aggr = global_symtab[expr.this.this.val]
      key = expr.this.ident.val
      val = getattr(aggr, key)
      gbls[key] = val

  else:
    exprthis = expr.this
    lcls = { k: v.val if type(v)==STR else v for k, v in symtab.items() }
    gbls = { k: v.val if type(v)==STR else v for k, v in global_symtab.items() }

  if type(exprthis) == ID:
    if exprthis.val in symtab:
      x = symtab[exprthis.val]
    else:
      x = global_symtab[exprthis.val]
    if type(x) == STR:
      if type(args) == STR:
        res = STR(eval(exprthis.val+'.'+expr.ident, gbls, lcls)(args.val))
      elif type(args) == ArgBundle:
        res = STR(eval(exprthis.val+'.'+expr.ident, gbls, lcls)(*args.unbundle()))
      else:
        raise Error(type(args), args)
    else:
      if type(args) in [ int, str, Agg ]:
        res = eval(exprthis.val+'.'+expr.ident, gbls, lcls)(args)
      elif type(args).__name__ == "Agg":
        res = eval(exprthis.val+'.'+expr.ident, gbls, lcls)(args)
      elif type(args) in [ STR ]:
        res = eval(exprthis.val+'.'+expr.ident, gbls, lcls)(args.val)
      elif type(args) == ArgBundle:
        res = eval(exprthis.val+'.'+expr.ident, gbls, lcls)(*args.unbundle())
      elif type(args) == EVAL_BUNDLE:
        assert False
        res = eval(exprthis.val+'.'+expr.ident, gbls, lcls)(*args.unbundle())
      else:
        raise Error(type(args), args, type(args)._name_)
  else:
    if type(expr.this) == MEMBER:
      res = eval(expr.this.ident.val+'.'+expr.ident, gbls, lcls)(*args.unbundle())
    else:
      res = eval(exprthis+'.'+expr.ident, gbls, lcls)(*args.unbundle())
  return res

@evaluate.register
def _(expr: CALL, symtab, global_symtab, function=None):
  if expr.ident == 'step':
    return evaluate_builtin_step(expr, symtab, global_symtab, function)
  elif expr.ident == 'print':
    return eval_print(expr, symtab, global_symtab, {})
  elif expr.ident == 'ty':
    return evaluate_builtin_type(expr, symtab, global_symtab)
  else:
    # Closure
    if type(expr.ident) == CLOSURE:

      param = expr.ident.val[0]
      body = expr.ident.val[1]
      evaluated_arg = evaluate(expr.arg, symtab, global_symtab)
      subst_body = rewrite(body, param, evaluated_arg)
      local_symtab = {}
      result = evaluate(subst_body, local_symtab, global_symtab)
      return result

    # outer defined
    elif expr.ident in global_symtab:
      param, body = global_symtab[expr.ident]
      if type(expr.arg) == ArgBundle and len(expr.arg.x) > 1:
        args = expr.arg.x
        params = [ x.strip() for x in param.val.split(',') ]
        subst_body = body
        for idx, arg in enumerate(args):
          evaluated_arg = evaluate(arg, symtab, global_symtab)
          subst_body = rewrite(subst_body, ID(params[idx]), CLOSURE(evaluated_arg))
        local_symtab = {}
        global_symtab = global_symtab
        result = evaluate(subst_body, local_symtab, global_symtab, expr.ident)
        return result
      else:
        evaluated_arguments = evaluate(expr.arg, symtab, global_symtab)
        substituted_body = rewrite(body, param, evaluated_arguments)

        local_symtab = {}
        global_symtab = global_symtab
        result = evaluate(substituted_body, local_symtab, global_symtab, expr.ident)
        return result
    else:
      if type(expr.arg) == ArgBundle:
        res = []
        for arg in expr.arg.unbundle():
          argres = evaluate(arg, symtab, global_symtab)
          if type(argres) == STR:
            argres = argres.val
          res += [ argres ]
      else:
        res = evaluate(expr.arg, symtab, global_symtab)

      try:
        sys.stdout = mystdout
        sys.stderr = mystderr
      except NameError:
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__
      result = ...
      if type(res) == STR:
        result = eval(expr.ident)(res.val)
      elif type(res) == int:
        result = eval(expr.ident)(res)
      elif type(res) == list:
        #print('eval CALL', expr.ident, res)
        if expr.ident == 'fval':
          result = res[1](res[0])
        else:
          #print('symtabs', symtab, global_symtab)
          lcls = { k: v.val if type(v)==STR else v for k, v in symtab.items() }
          gbls = { k: v.val if type(v)==STR else v for k, v in global_symtab.items() }
          #print (lcls, gbls, expr.ident)
          add_inner_funcs(lcls)
          result = eval(expr.ident, gbls, lcls)(*res)
      elif type(res) == Agg:
        result = eval(expr.ident)(res)
      else:
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__
        raise Error(result, type(result))

      sys.stdout = sys.__stdout__
      sys.stderr = sys.__stderr__
      return result
      """
      evaluated_argument = evaluate(expr.arg, symtab)
      res = eval(expr.ident)(evaluated_argument)
      return res
      """

@evaluate.register
def _(expr:TermNode, symtab, gsymtab, function=None):
  if expr.op == '': return evaluate(expr.lhs, symtab, gsymtab)

  lhs = eval_context(evaluate(expr.lhs, symtab, gsymtab), symtab, gsymtab)
  rhs = eval_context(evaluate(expr.rhs, symtab, gsymtab), symtab, gsymtab)

  if expr.op == '*': return lhs * rhs
  elif expr.op == '/': return lhs / rhs
  else: raise Error(expr.op, expr.line)

@evaluate.register
def _(expr:ExprNode, symtab, global_symtab, function=None):
  lhs = eval_context(evaluate(expr.lhs, symtab, global_symtab), symtab, global_symtab)
  rhs = eval_context(evaluate(expr.rhs, symtab, global_symtab), symtab, global_symtab)

  try:
    if expr.op == '+': return lhs + rhs
    elif expr.op == '-': return lhs - rhs
    else: raise Error(expr.op, expr.line)
  except TypeError as ex:
    raise Error(expr.op, lhs, rhs, expr.line, ex)

@evaluate.register
def _(expr: CompareNode, symtab, gsymtab, function=None):
  if expr.op == '==':
      if type(expr.lhs).__name__ == 'function':
        print('eval CompareNode fn', expr, type(expr.lhs))
      #print('CompareNode', type(expr.lhs))
      lhs = evaluate(expr.lhs, symtab, gsymtab)
      rhs = evaluate(expr.rhs, symtab, gsymtab)
      return lhs == rhs
  elif expr.op == '!=':
      lhs = evaluate(expr.lhs, symtab, gsymtab)
      rhs = evaluate(expr.rhs, symtab, gsymtab)
      return lhs != rhs
  elif expr.op == '===':
      lhs = evaluate(expr.lhs, symtab, gsymtab)
      rhs = evaluate(expr.rhs, symtab, gsymtab)
      return STRICT(lhs, lhs.typename) == STRICT(rhs, rhs.typename)
  elif expr.op == '<=': return evaluate(expr.lhs, symtab, gsymtab) <= evaluate(expr.rhs, symtab, gsymtab)
  elif expr.op == '>=': return evaluate(expr.lhs, symtab, gsymtab) >= evaluate(expr.rhs, symtab, gsymtab)
  elif expr.op == '>': return evaluate(expr.lhs, symtab, gsymtab) > evaluate(expr.rhs, symtab, gsymtab)
  elif expr.op == '<': return evaluate(expr.lhs, symtab, gsymtab) < evaluate(expr.rhs, symtab, gsymtab)
  else: raise Error(expr.op)

@evaluate.register
def _(expr:types.GeneratorType, symtab, function=None):
  lst_ret = []
  for stmt in expr:
    lst_ret.append(evaluate(stmt, symtab))
  return lst_ret[-1] # only last stmt in body is returned

@evaluate.register
def _(expr:Bundle, symtab, gsymtab, function=None):
  if len(expr.x) == 0:
    return Bundle([])
  lst_ret = []
  for stmt in expr.x:
    res = evaluate(stmt, symtab, gsymtab)
    if type(res) == ReturnNode: # Bundle returns ReturnNode
      return res
    lst_ret.append(res)
  return lst_ret[-1]

@evaluate.register
def _(expr:ArgBundle, symtab, global_symtab, function=None):
  if len(expr.x) == 0:
    return ArgBundle([])
  lst_ret = []
  for stmt in expr.x:
    res = evaluate(stmt, symtab, global_symtab)
    lst_ret.append(res)
  return lst_ret[-1]

@dataclass
class Context:
  expr:ExprNode
  lcls:dict
  def __int__(s):
    return s.expr
  def __str__(s): return str(s.expr)
  def __repr__(s): return s.__str__()

@evaluate.register
def _(expr:FuncBundle, symtab, global_symtab, function=None):
  if len(expr.x) == 0:
    raise Error
  lst_ret = []
  for stmt in expr.x:
    res = evaluate(stmt, symtab, global_symtab)
    if type(res) == ReturnNode: # FuncBundle evaluates ReturnNode
      return evaluate(res.expr, symtab, global_symtab)
    lst_ret.append(res)
  return lst_ret[-1]

def eval_context(expr, symtab, gsymtab):
  while type(expr) == Context:
    local_symtab = expr.lcls
    print('eval_context', local_symtab)
    expr = evaluate(expr.expr, local_symtab, gsymtab)
  return expr

@evaluate.register
def _(expr:LANGBLOCK, symtab, gsymtab, function=None):
  if expr.lang.val == 'l':
    t = True if expr.type == '!' else False
    lcls = {}
    for arg in expr.args.unbundle():
      lcls[arg.val] = evaluate(arg, symtab, gsymtab)
    ao =  AbstractObject(expr.body.strip(), expr.lang.val, t, {}, lcls)
    return ao
  elif expr.lang.val == 'c':
    t = True if expr.type == '!' else False
    inner = expr.body
    if t:
      return ReturnConversion(
               ExtractFunc(AbstractObject(inner, expr.lang.val, t, {}, {}), "main"), ctypes.c_int)
    else:
      return AbstractObject(inner, expr.lang.val, t, {}, {})
  elif expr.lang.ident in [ 'm', 'b' ]:
    t = True if expr.type == '!' else False
    inner = expr.body
    return AbstractObject(inner, expr.lang.val, t, {}, {})
  else:
    raise Error("Unrecognized language", expr.lang)

@evaluate.register
def _(expr: AbstractObject, symtab, gsymtab, function=None): return expr

@evaluate.register
def _(expr: ExtractFunc, symtab, gsymtab, function=None):
  res = evaluate(expr.ao, symtab, gsymtab)()
  assert type(res) == FuncLib
  if type(res.library) == ctypes.CDLL:
    fn = getattr(res.library, expr.name)
  elif type(res.library) == dict:
    fn = res.library[expr.name]
  else:
    raise Error(type(res.library), res.library)
  return fn

@evaluate.register
def _(expr: ReturnConversion, symtab, gsymtab, function=None):
  if expr.ctyp == ctypes.c_char_p: #'c_char_p':
      fnhandle = evaluate(expr.ef, symtab, gsymtab)
      fnhandle.restype = ctypes.c_char_p
      return str(fnhandle())
  elif expr.ctyp == ctypes.c_int: #'c_int':
      fnhandle = evaluate(expr.ef, symtab, gsymtab)
      fnhandle.restype = ctypes.c_int
      return int(fnhandle())
  else:
      raise Error(expr.ctype, type(expr.ctype))

@evaluate.register
def _(expr:CarrotNode, symtab, gsymtab, function=None):
  res = evaluate(expr.expr, symtab, gsymtab)
  if type(res) == AbstractObject:
    result = res()
  elif type(res) == ReturnConversion:
    result = evaluate(res, symtab, gsymtab)
  elif type(res) == ExtractFunc:
    result = evaluate(res, symtab, gsymtab)()
  else:
    result = res

  if type(result) == int:
    return result
  elif type(result) == str:
    return result
  elif type(result) == list:
    return result
  elif type(result) == ast.Module:
    dereferencedResult = result.body[-1]
    return dereferencedResult
  elif type(result) == ctypes.CDLL:
    return result
  elif type(result) == FuncLib:
    return result
  else:
    raise Error(type(result), result)

@evaluate.register
def _(expr:CarrotExpr, symtab, gsymtab, function=None):
  if expr.kw.x == 'expr':
      expr_eval = evaluate(expr.expr, symtab, gsymtab)
      if type(expr_eval) == ast.Expr:
          val = expr_eval.value
          return val
      else:
          raise Error('Had expr but expected', type(expr_eval))
  elif expr.kw.x == 'binop':
      binop_eval = evaluate(expr.expr, symtab, gsymtab)
      if type(binop_eval) == ast.BinOp:
          lhs, op, rhs = binop_eval.left, binop_eval.op, binop_eval.right
          return ast.BinOp(lhs, op, rhs)
      else:
          raise Error('Had binop but expected', type(binop_eval), expr.expr, binop_eval)
  else:
      raise Error(expr.kw.x)

@evaluate.register
def _(expr:PreCarrotExpr, symtab, gsymtab, function=None):
  if expr.kw.x == 'c_char_p':
      return ReturnConversion(evaluate(expr.expr, symtab, gsymtab), ctypes.c_char_p)
  elif expr.kw.x == 'c_int':
      return ReturnConversion(evaluate(expr.expr, symtab, gsymtab), ctypes.c_int)
  else:
      if expr.kw.x[0] == "&":
        name = expr.kw.x[1:]
        ao_eval = evaluate(expr.expr, symtab, gsymtab)
        assert type(ao_eval) == AbstractObject
        #fn = getattr(libhandle_eval, name)
        return ExtractFunc(ao_eval, name)
      else:
        raise Error(expr.kw.x)

@evaluate.register
def _(expr: NegateNode, symtab, gsymtab, function=None): return -evaluate(expr.expr, symtab, gsymtab)

@evaluate.register
def _(expr: Expr, symtab, gsymtab, function=None):
  #pdb.set_trace()
  if expr.op == ':':
    resL = expr.valL
    resR = evaluate(expr.valR, symtab, gsymtab)
    return assign_eval(symtab, gsymtab, resL, resR, gbl=True)
  elif expr.op == '+':
    return evaluate(expr.valL, symtab, gsymtab) + evaluate(expr.valR, symtab, gsymtab)
  elif expr.op == '-':
    return evaluate(expr.valL, symtab, gsymtab) - evaluate(expr.valR, symtab, gsymtab)
  elif expr.op == '*':
    return evaluate(expr.valL, symtab, gsymtab) * evaluate(expr.valR, symtab, gsymtab)
  else:
    raise Error(expr.op, expr)

def assign_identifier_bundle(lookup, glookup, lhs, res):
  elemident = lhs.elem
  elemval = res.elem
  assert type(elemident) == ID
  lookup[elemident.val] = evaluate(elemval, lookup, glookup)
  if type(lhs.nesting) in [ IDENTIFIER_BUNDLE, ID ]:
    if type(res.nesting) == EVAL_BUNDLE:
      assign_identifier_bundle(lookup, glookup, lhs.nesting, res.nesting)
    else:
      assert type(lhs.nesting) == ID
      lookup[lhs.nesting.val] = evaluate(res.nesting, lookup, glookup)

def assign_inner_bundle(lookup, glookup, lhs, res):
  elemident = lhs.elem
  elemval = res.RHS
  assert type(elemident) == ID
  lookup[elemident.val] = evaluate(elemval, lookup, glookup)
  if type(lhs.nesting) in [ IDENTIFIER_BUNDLE, ID ]:
    if type(res.LHS) == INNER_BUNDLE:
      assign_inner_bundle(lookup, glookup, lhs.nesting, res.LHS)
    else:
      assert type(lhs.nesting) == ID
      lookup[lhs.nesting.val] = evaluate(res.LHS, lookup, glookup)

def assign_eval(lookup, global_lookup, lhs, rhs, gbl=False):
  if type(lhs) == IDENTIFIER_BUNDLE:
    res = evaluate(rhs, lookup, global_lookup)
    assert type(res) == EVAL_BUNDLE
    assign_identifier_bundle(lookup, global_lookup, lhs, res)
    return res

  if lhs in lookup:
    raise Error(lhs, rhs, lookup[lhs])
  res = evaluate(rhs, lookup, global_lookup)
  if gbl:
    global_lookup[lhs.val] = res
  else:
    lookup[lhs.val] = res
  return res

def assignmut_eval(lookup, global_lookup, lhs, rhs, gbl=False):

  if type(lhs) == IDENTIFIER_BUNDLE:
    res = evaluate(rhs, lookup, global_lookup)
    if type(res) == EVAL_BUNDLE:
      assign_identifier_bundle(lookup, global_lookup, lhs, res)
      return res
    elif type(res) == INNER_BUNDLE:
      assign_inner_bundle(lookup, global_lookup, lhs, res)
      return res
    else:
      raise Error (lhs, type(res), res)

  res = evaluate(rhs, lookup, global_lookup)
  if gbl:
    global_lookup[lhs.val] = res
  else:
    lookup[lhs.val] = res
  return res

def assigntype_eval(lookup, global_lookup, types, typ, lhs, rhs, gbl=False):
  #if lhs in lookup:
  #  raise Error(lhs, rhs, lookup[lhs])
  res = evaluate(rhs, lookup, global_lookup)
  if gbl:
    global_lookup[lhs.val] = res
  else:
    lookup[lhs.val] = res

  types[lhs.val] = typ
  return res

def funcdef_eval(lookup, name, param, body):
  #if name in lookup:
  #  raise Error
  lookup[name] = param, body

internalize = 0
backwards = {}
def aggdef_eval(lookup, glookup, name, vals, gbl=False):
  global internalize
  global backwards
  if name in lookup:
    raise Error
  if gbl:
    glookup[name] = vals
    glookup[name.val] = internalize
  else:
    lookup[name] = vals
    lookup[name.val] = internalize
  backwards[internalize] = name.val
  internalize += 1

def mmprint(arg):
  print(arg)
  return arg

def rprint(*args, **kwargs):
  try:
    sys.stdout = mystdout
    sys.stderr = mystderr
  except NameError:
    sys.stdout = sys.__stdout__ #StringIO()
    sys.stderr = sys.__stderr__ #StringIO()
  if isinstance(args[0], str) and len(args[0]) > 1000:
    print("LONG FIRST ARG:", args[0][:50], '...', sep='')
  else:
    print(*args, **kwargs)
  sys.stdout = sys.__stdout__
  sys.stderr = sys.__stderr__

def print_astBinOp(res, recurse=False):
  rprint('BinOp( ', end='')
  if type(res.left) == ast.BinOp:
    print_astBinOp(res.left, recurse=True)
  else:
    print_Expr(res.left)

  if type(res.op) == ast.Add:
    rprint('+', end=' ')
  elif type(res.op) == ast.Sub:
    rprint('-', end=' ')
  else:
    raise Error

  if type(res.right) == ast.BinOp:
    rprint_astBinOp(res.right, recurse=True)
  else:
    print_Expr(res.right)

  if not recurse:
    rprint(')')
  else:
    rprint(')', end=' ')

def print_Expr(expr):
  if type(expr) == ast.Constant: rprint(expr.value, '(ast.Constant)', sep='', end=' ')
  elif type(expr) == ast.BinOp: print_astBinOp(expr)
  elif type(expr) == ast.Expr: rprint('Expr( ', end=''); print_Expr(expr.value); rprint(')', end=' ')
  else:
      if type(expr) == int:
         rprint(expr, '(int)', sep='', end=' ')
      else:
        raise Error

#import pdb
def program_eval(AST):
  #pdb.set_trace()
  if logging:
    print("Evaluating AST")
  lookup = defaultdict(Union[int,defaultdict])
  global_lookup = {}
  global types
  types = {}
  res = []
  for stmt in AST:
    if type(stmt) is AssignNode:
      assign_eval(lookup, global_lookup, stmt.lhs, evaluate(stmt.rhs, lookup, global_lookup), gbl=True)
    elif type(stmt) is AssignTypeNode:
      # lookup [ lhs ] = TYPED(evaluation)
      # types  [ lhs ] = typ
      assigntype_eval(lookup, global_lookup, types, stmt.typ, stmt.lhs, TYPED(evaluate(stmt.rhs, lookup, global_lookup), stmt.typ), gbl=True)
    elif type(stmt) is AssignMutableNode:
      assignmut_eval(lookup, global_lookup, stmt.lhs, evaluate(stmt.rhs, lookup, global_lookup), gbl=True)
    elif type(stmt) is FuncNode:
      funcdef_eval(global_lookup, stmt.name, ID(stmt.param), stmt.body)
    elif type(stmt) is AggregateNode:
      aggdef_eval(lookup, global_lookup, ID(stmt.name), stmt.vals, gbl=True)
    elif type(stmt) is IfElseNode:
      evaluate(stmt, lookup, global_lookup)
    elif type(stmt) is ForLoopNode:
      evaluate(stmt, lookup, global_lookup)
    elif type(stmt) is WhileLoopNode:
      evaluate(stmt, lookup, global_lookup)
    elif type(stmt) is int:
      res.append(evaluate(stmt, lookup, global_lookup))
    elif type(stmt) is ID2:
      res.append(evaluate(stmt, lookup, global_lookup))
    elif type(stmt) is Expr:
      ret = evaluate(stmt, lookup, global_lookup)
      res.append(ret)
    elif type(stmt) is CALL:
      if stmt.ident == 'print':
        eval_print(stmt, lookup, global_lookup, types)
      else:
        evaluate(stmt, lookup, global_lookup)
    elif type(stmt) is UFCSCALL:
      if stmt.ident == 'print':
        stmt = CALL(stmt.ident, ArgBundle([evaluate(stmt.this, lookup, global_lookup)] + stmt.arg.unbundle()), stmt.line)
        eval_print(stmt, lookup, global_lookup, types)
      else:
        #stmt = CALL(stmt.ident, ArgBundle([evaluate(stmt.this, lookup)] + stmt.arg.unbundle()))
        #print(stmt)
        evaluate(stmt, lookup, global_lookup)
    else:
      raise Error('program_eval stmt:', stmt, type(stmt))

  return res

def print_Aggregate(agg, typ):
  rprint('(', end=" ")
  aggr = Agg()
  types = typ.unbundle()
  aggr.types = types
  for idx, expr in enumerate(agg.exprs.unbundle()):
    ty = types[idx]
    rprint(ty[0].ident, expr, end=" ")
    setattr(aggr, ty[0].ident, expr)
  rprint(')')
  #print(dir(aggr))
def print_Agg(agg):
  rprint('struct(', end=" ")
  for elem in agg.types.unbundle():
    rprint(elem[0], getattr(agg, elem[0].ident), sep=' ', end=' ')
  rprint(')')
  #print(agg.types)
  #print(inspect.getmembers_static(agg))

def print_tuple_bundle(bundle):
  assert type(bundle) == EVAL_BUNDLE
  if type(bundle.nesting) == EVAL_BUNDLE:
    print_tuple_bundle(bundle.nesting)
  else:
    rprint(bundle.nesting, end=' ')
  rprint(bundle.elem)

def print_symbols(symbols_arg):
  rprint("SYMBOLS")

def print_closure(stmt, lookup, glookup):
  rprint("CLOSURE")
  res = eval_context(evaluate(stmt.arg, lookup, glookup), lookup, glookup)
  #rprint("CLOSURE res", res, type(res))


def eval_print(stmt, lookup, global_lookup, types):
  global backwards
  first_arg = stmt.arg if not type(stmt.arg)==Bundle else stmt.arg.unbundle()[0]
  #print('eval_print', first_arg, type(first_arg))
  #if type(first_arg) == CLOSURE:
    #print_closure(stmt, lookup, global_lookup)
    #return
  if type(first_arg) == ID and first_arg.val in [ 'symbols', 'symbols_' ]:
    print_symbols(first_arg)
    return
  elif type(first_arg) == ID and first_arg.val in [ 'contents', 'contents_' ]:
    rprint("CONTENTS")
    return
  #elif type(first_arg) == ID and first_arg.val in [ 'cegex', 'cegex_' ]:
    #return

  #print(stmt.arg)
  res = eval_context(evaluate(stmt.arg, lookup, global_lookup), lookup, global_lookup)
  #print(res)

  if type(res) == ast.BinOp:
    print_astBinOp(res)
  elif type(res) in [ ast.Expr, ast.Constant ]:
    print_Expr(res)
  elif type(res) in [ int, str, tuple, bool, dict, type ]:
    rprint(res)
  elif type(res) in [ list ]:
    rprint(res)
  elif type(res) in [ type ]:
    rprint(res)
  elif type(res).__name__ in [ 'NoneType' ]:
    rprint(res)
  elif type(res) in [ STR ]:
    rprint(res.val)
  elif type(res) == EVAL_BUNDLE:
    print_tuple_bundle(res)
  elif type(res) == INNER_BUNDLE:
    rprint("INNER_BUNDLE")
  elif type(res) == AbstractObject:
    rprint('AO(', res.s, ')')
  elif type(res) == AggregateObject:
    rprint('AggObj(', res.exprs, ')')
  elif type(res) == Agg:
    rprint(res)
  elif type(res) == TYPED:
    print_Aggregate(res.expr, lookup[res.typ.name.ident])
  elif type(res) == TYPE:
    rprint(backwards[res.name])
  elif type(res).__name__ in [ 'builtin_function_or_method' ]:
    rprint(res)
  elif type(res).__name__ in [ 'function' ]:
    rprint(res)
  elif type(res) in [ re.Pattern, Pattern, re.Match, Match ]:
    rprint(res)
  elif type(res) == FuncLib:
    if type(res.library) == dict:
      fn_or_libname = list(res.library.keys())[0]
    elif type(res.library) == ctypes.CDLL:
      fn_or_libname = type(res.library)
    else:
      raise Error(type(res.library), res.library)
    rprint('FuncLib(', fn_or_libname, ')')
  else:
    raise Error(type(res), type(res).__name__, type(res).__qualname__, res)
  return res

#### translation

# translate to IR
def translate_ast_to_ir_expr(expr):
  pass
  
def translate_ast_to_ir(AST):
  ir = []
  for stmt in AST:
    if type(stmt) is AssignNode:
      ir.append(stmt)
    elif type(stmt) is PRINT:
      ir.append(stmt)
    else:
      raise Error

# IR
def translate_to_3addrcode(IR):
  pass
  

####

precedence_relation = [ '⋗', '≐', '⋖' ]

def compile_and_evaluate(string_input):
  inp = string_input

  if logging:
    print(inp)
  tokens = lexer(inp)
  l, r = itertools.tee(tokens)
  print('tokens', list(r))
  AST = program_parse(l)

  print('abstract syntax tree')
  print_ast(AST)
  program_eval(AST)

  threeaddr = translate_to_3addrcode(AST)

def compile(inp):
  compile_and_evaluate(inp)
  return
  try:
    compile_and_evaluate(inp)
  except Error:
    print('Exception caught:')
    traceback.print_exc()
    print('#####')

if __name__ == "__main__":
  from inputs import *
def tests():
  global logging
  oldlog, logging = logging, False

  sources =\
  [ (INPUTB, 9), 
    (INPUTB2, 11),
    (INPUTC, 37), (INPUTD, 17),
    (INPUTE, 10), (INPUTF, 33),
    (INPUTG, answerG), (INPUTG2, answerG2),
    (INPUTB3, 31), (INPUTB4, 35),
    (INPUTH, answerH),
    (INPUTH2, 13),
    (INPUTE3, "3(ast.Constant)"), (INPUTE2, "Expr( 3(ast.Constant) )"),
    (INPUTE4, "AO( import ast; return ast.parse('3') )"),
    (INPUTE5, "AO( return 'cat' )"),
    (INPUTE6, 'cat'),
    (INPUTE7, 'catdog'),
    (INPUTE8, 'catdogcat'),
    #(INPUTI, "b'cat'dog"), # FIXME: ditto
    #(INPUTI2, 4), # FIXME: ditto
    #(INPUTI3, 0)
    #(INPUTI4, 4), # FIXME: c lang file Permission error
    #(INPUTI5, 'FuncLib( fn )'), # FIXME: ditto
    #(INPUTI6, "FuncLib( <class 'ctypes.CDLL'> )"), # FIXME: ditto
    #(INPUTI7, 8), # FIXME: ditto
    #(INPUTI8, 8), # FIXME: ditto
    #(INPUTI9, "b'cat'dog"), # FIXME: ditto
    #(INPUTJ, None),
    (INPUTK, 3),
    (INPUTK2, 3),
    (INPUTK3, 7),
    (INPUTK4, 11),
    (INPUTK5, 10),
    #(INPUTK6, 5), # FIXME: ditto # TODO: wrap in main function and call with automatic ReturnConversion
  ]
  sources = sources + [
  #sources = [
     #(INPUTL, None),
     (INPUTL2, "[1, 2, 3, 4, 5, 6]\n"),
     (INPUTL3, 90),
     (INPUTL4, 16),
     (INPUTM, 2),
     (INPUTM2, 3),
     (INPUTM3, 3),
     (INPUTM4, "[3, 2, 1]\n"),
     (INPUTM5, 4),
     (INPUTM6, "[3, 2]\n"),
     (INPUTM7, "[3, 2]\n"),
     (INPUTM8, 4),
     (INPUTN, 2),
     (INPUTN2, "-2\n"),
     (INPUTN3, 8),
     (INPUTN4, 8),
     (INPUTO, "catcat"),
     #(INPUTO2, "catcatdog"),
     (INPUTO3, "-4\n"),
     (INPUTO4, 0),
     (INPUTP, 6),
     #(INPUTP3, 2),
     (INPUTQ, 6),
     (INPUTQ2, 5),
  ]
  sources = sources + [
     (INPUTQ3, 6),
     (INPUTQ4, "6\n24\n120\n"),
     (INPUTQ5, 15),
     (INPUTQ6, "15\n25\n1\n"),
     (INPUTR, "True\n"),
     (INPUTR2, "True\n"),
     (INPUTR3, "True\n"),
     (INPUTS, "2\n3\n5\n8\n13"),
     (INPUTT, "True\nFalse\n"),
     (INPUTT2, "True\n"),
     (INPUTT3, "3\n7\n5\n13\n29\n61\n"),
     (INPUTT4, 2),
     (INPUTU, "catdog"),
     (INPUTU2, 2),
     (INPUTU3, "hi\nNone\n"),
     (INPUTU4, "print('hi')"),
     (INPUTU5, 2),
     (INPUTU6, 4),
     (INPUTU7, 2),
     (INPUTU8, "False\nFalse\nTrue\n"),
     (INPUTU9, "<class '__main__.Agg'>"),
     (INPUTU10, 42),
     (INPUTV, "3 2"),
     (INPUTV2, "3 2"),
     (INPUTV3, "3\n20\n1\n"),
     (INPUTV4, 20),
     (INPUTW, "0\n0\n0\n1\n1\n2\n"),
     (INPUTX, "Agg<Ty>(x 42,y 24,)\nTy\nTy\nAgg<Yy>(x 42,y 24,)\nYy\nYy\nTrue\nTrue\n"),
  ]
  global old_stdout, old_stderr, mystdout, mystderr
  for idx, (src, ans) in enumerate(sources):
    print('#########')
    print(src)
    #print('Expected:', ans)
    print('test #', idx, sep='')
    mystdout = StringIO()
    mystderr = StringIO()
    try:
      compile_and_evaluate(src)
    except Error:
      sys.stdout = sys.__stdout__
      sys.stderr = sys.__stderr__
      traceback.print_exc()
    else:
      print('Passed compilation.')
    finally:

      sys.stdout = sys.__stdout__
      sys.stderr = sys.__stderr__
      evaluated_ans = mystdout.getvalue()
      evaluated_err = mystderr.getvalue()
      print('Evaluated:', evaluated_ans.strip())
      print('Error:', evaluated_err.strip() if evaluated_err else repr(''))

      if all(c.isdigit() for c in evaluated_ans.strip()):
        assert int(evaluated_ans) == ans
      elif type(ans) == str:
        assert evaluated_ans.strip() == ans.strip()
      elif type(ans) == list:
        print(repr(evaluated_ans))
        print(repr(ans))
        assert tuple(evaluated_ans) == tuple(ans)
      else:
        raise Error(type(evaluated_ans), evaluated_ans, type(ans), ans)

      print('Passed evaluation!')

  logging = oldlog

def hello(x):
  return x + 2

if __name__ == "__main__":
  mystdout = sys.__stdout__
  mystderr = sys.__stderr__
  parser = argparse.ArgumentParser(prog='lang', description='language', epilog='this language')
  parser.add_argument('compilername', nargs='?', default=None)
  parser.add_argument('filename', nargs='?', default=None)
  args = parser.parse_args()
  if args.compilername != None:
    with open(args.compilername) as f:
      contents = f.read()

    global inputfile
    if args.filename != None:
      inputfile = args.filename
    else:
      inputfile = None

    compile(contents)

    exit()
  else:
    print('hello')
    tests()
    exit()
  #tests()
  #exit()
  #logging=False
  #print(INPUTI)
  #compile(INPUTI)

  #import pdb
  #pdb.run('compile(INPUTE8)')
  #compile(INPUTB4)
  #logging = True
  #compile(INPUTE)
  if False:
    import cProfile
    cProfile.run('compile_and_evaluate(INPUTE)')
