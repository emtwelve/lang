dbl_quote : double_quote()
single_quote : single_quote()
langblock : "@(\w+)\s*{\s*((?:[()" + single_quote + "\]\[\w\d\s+]|\$\$)+)\s*}!"
cegex_str : "(?:" + langblock + "|" + single_quote + "([;\^.#\-$@<>,|+?\w\d*\\\\=!" + dbl_quote + "{}\s\[\]:()]*)" + single_quote + "|" + dbl_quote + "([;\^.#\-$@<>,|+?\w\d*\\\\\\=!" + single_quote + "{}\s\[\]:()" + "]*)" + dbl_quote + "|(\d+)|(if|else|for|while)|(ret)|(type)|(print)|(def\s*\w+\([\w\s,]*\))|(in)|(\w+)|(\+|-)|(<:)|(<|==|!=)|(:|=)|(;)|([.])|([*])|([{}])|([\(])|([\)])|([,])|([\[])|([\]])|(\^)" +  ")\s*"

reset_idx()

cegex : reg_compile(cegex_str)
contents : read_inputfile()

emptylist : ^ @l { [] }!

type Num { x }
type Control { x }
type Return { x }
type Print { x }
type Punctuation { x }
type Trmop { x }
type Binop { x }
type Cmpop { x }
type Typunc { x }
type Dot { x }
type Ident { x }
type Brace { x }
type LParen { x }
type RParen { x }
type Quote { x }
type Comma { x }
type LBrket { x }
type RBrket { x }
type FnDefn {
  name
  parameters
  x
}
type Type { x }
type Langblock {
  body
  arguments
}
type Carrot { x }
type Semi { x }
type Inn { x }
type EndOfTokens { x }

eottok <: EndOfTokens = { 0, }

type Token {
  lexeme
  tagmeme
}

EOT <: EndOfTokens = { 0, }

type IdentFactor {
  HS
}

def lexer(cegex_, contents_) {

  cegex : cegex_
  contents : contents_

  print("about to reg_split on cegex and contents")
  lst : reg_finditer(cegex, contents)
  print("##############################")
  num_rgroups : reg_numgroups(cegex)
  
  print(num_rgroups)
  
  startToken <: Token = { "START", 0, }
  tokens : [ startToken ]
  
  print("LST is")
  print(lst)
  
  for match in lst {
      print("##MYMATCH")
      print(match)
      last_index = reg_lastindex(match)
      m = strp(reg_match(match))
      print(m)
      match_langblock; match_langbody; match_quote; match_double; match_num; match_control; match_ret; match_typ; match_print; match_fndefn; match_inn; match_ident; match_binop; match_typunc; match_cmpop; match_punc; match_semi; match_dot; match_trmop; match_brace; match_lparen; match_rparen; match_comma; match_lbrket; match_rbrket; match_carrot = match_all(last_index)
      if match_num {
        numtag <: Num = { 0, }
        tok <: Token = { m, numtag, }
        tokens = tokens + [ tok ]
      }
      if match_control {
        tag <: Control = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_ret {
        tag <: Return = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_typ {
        tag <: Type = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_print {
        tag <: Print = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_ident {
        tag <: Ident = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_punc {
        tag <: Punctuation = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_binop {
        tag <: Binop = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_cmpop {
        tag <: Cmpop = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_typunc {
        tag <: Typunc = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_dot {
        tag <: Dot = { 1, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_trmop {
        tag <: Trmop = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_brace {
        tag <: Brace = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_lparen {
        tag <: LParen = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_rparen {
        tag <: RParen = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_quote {
        s : reg_search_squote(m)
        tag <: Quote = { 0, }
        tok <: Token = { s, tag, }
        tokens = tokens + [ tok ]
      }
      if match_double {
        print("Matched double")
        print(m)
        s : reg_search_dquote(m)
        tag <: Quote = { 0, }
        tok <: Token = { s, tag, }
        tokens = tokens + [ tok ]
      }
      if match_comma {
        tag <: Comma = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_lbrket {
        tag <: LBrket = { 1, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_rbrket {
        tag <: RBrket = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_fndefn {
        print("Matched fndefn")
        m : strp(reg_full_match(match))
        print(m)
        name : reg_search(m, "name")
        param : reg_search(m, "param")
        tag <: FnDefn = { name, param, 0, }
        tok <: Token = { match_fndefn, tag, }
        tokens = tokens + [ tok ]
      }
      if match_langbody {
        print("Matched langblock")
        match_langblock : reg_matchleft(match)
        match_langbody : reg_matchright(match)
        print(match_langblock)
        print(match_langbody)
        if has_ddollar(match_langbody) {
          print("has ddollar")
          replaced_body : replace_ddollar(match_langbody)
          ddident : ddollar_ident(match_langbody)
          ddid <: IdentFactor = { ddident, }
          print(replaced_body)
          print(ddid)
          tag <: Langblock = { replaced_body, ddid, }
          tok <: Token = { match_langblock, tag, }
          tokens = tokens + [ tok ]
        } else {
          tag <: Langblock = { match_langbody, -7, }
          tok <: Token = { match_langblock, tag, }
          tokens = tokens + [ tok ]
        }
      }
      if match_carrot {
        tag <: Carrot = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_semi {
        tag <: Semi = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      if match_inn {
        tag <: Inn = { 0, }
        tok <: Token = { m, tag, }
        tokens = tokens + [ tok ]
      }
      cb_print(tok)
  }
  
  print("##########")
  print("Tokens self")
  
  q : "EOT"
  endtok <: Token = { q, eottok, }
  print("HELLO")
  tokens : tokens + [ endtok ]
  ret tokens
}

tokens = lexer(cegex, contents)

type IdentFactor {
  HS
}

type NumFactor {
  HS
}

type StringFactor {
  HS
}

type ListFactor {
  elems
  numelems
}

type TypeInitFactor {
  items
  numitems
}

type CallFactor {
  HS
  argument
  numargs
}

type MemberFactor {
  this
  member
}

type AccessFactor {
  this
  expidx
}

type LangblockFactor {
  name
  body
  arguments
}

type AssignStatement {
  LHS
  RHS
}

type TypeStatement {
  LHS
  typname
  RHS
}

type BinopMul {
  LHS
  RHS
}

type BinopAdd {
  LHS
  RHS
}

type BinopEq {
  LHS
  RHS
}

type BinopNq {
  LHS
  RHS
}

type BinopLt {
  LHS
  RHS
}

type Structure {
  LHS
  RHS
}

type NameStructure {
  LHS
  RHS
}

type UnaryCarrot {
  HS
}

type UnaryNegate {
  HS
}

type IfElseStatement {
  condition
  truepath
  falsepath
}

type ForLoopStatement {
  loopvar
  iterable
  path
}

type WhileLoopStatement {
  condition
  path
}

type TypeDefinition {
  typname
  members
}

type FuncDefinition {
  name
  parameters
  path
}

type PrintStatement {
  HS
}

type ReturnStatement {
  IS
}

type EndOfStatements {
  epi
}

def identifier(tokens, tokidx) {
  token = tokens[tokidx]
  print(token.tagmeme)
  print(ty(token.tagmeme))
  if ty(token.tagmeme) == Ident {
    res <: IdentFactor = { strp(token.lexeme), }
    ret res; tokidx+1
  } else {
    print("ERROR: Expected an IDENT")
    cr_print(ty(token))
    cr_print(tokens[tokidx-1])
    cr_print(token)
    cr_print(tokens[tokidx+1])
  }
}

def itemsinit(tokens, tokidx_) {
  tokidx = tokidx_

  items : emptylist
  numitems : 0
  while ty(tokens[tokidx].tagmeme) != Brace {
    print("in arg loop0")
    print(tokens[tokidx])
    item; tokidx : expression(tokens, tokidx)
    numitems : numitems + 1
    items : items + [ item ]

    if ty(tokens[tokidx].tagmeme) == Comma {
      tokidx = tokidx + 1
    } else {
      ret items; numitems; tokidx+1
    }
    print(items)
  }
  if tokens[tokidx].lexeme != "}" {
    print("ERROR: Expected an rbrace")
    print("Got:")
    print(tokens[tokidx].lexeme)
  }
  ret items; numitems; tokidx+1
}


def arguments(tokens, tokidx_) {
  tokidx = tokidx_

  arguments : emptylist
  numargs : 0
  while ty(tokens[tokidx].tagmeme) != RParen {
    arg; tokidx : expression(tokens, tokidx)
    numargs : numargs + 1
    arguments : arguments + [ arg ]

    if ty(tokens[tokidx].tagmeme) == Comma {
      tokidx = tokidx + 1
    } else {
      ret arguments; numargs; tokidx+1
    }
  }
  if tokens[tokidx].lexeme != ")" {
    print("ERROR: Expected an rparen")
    print("Got:")
    print(tokens[tokidx].lexeme)
  }
  ret arguments; numargs; tokidx+1
}

def elements(tokens, tokidx_) {
  tokidx = tokidx_

  elems : emptylist
  numelems : 0
  while ty(tokens[tokidx].tagmeme) != RBrket {
    print("in arg loop2")
    print(tokens[tokidx])
    elem; tokidx : factor(tokens, tokidx)
    numelems : numelems + 1
    elems : elems + [ elem ]

    if ty(tokens[tokidx].tagmeme) == Comma {
      tokidx = tokidx + 1
    } else {
      ret elems; numelems; tokidx+1
    }
    print(elems)
  }
  if tokens[tokidx].lexeme != "]" {
    print("ERROR: Expected an rparen")
    print("Got:")
    print(tokens[tokidx].lexeme)
  }
  ret elems; numelems; tokidx+1
}

def factor(tokens, tokidx_) {
  token = tokens[tokidx_]

  if ty(token.tagmeme) == LBrket {
    tokidx = tokidx_ + 1

    elems; numelems; tokidx = elements(tokens, tokidx)
    print("LBrket ListFactor")
    print(elems)
    print(numelems)
    res <: ListFactor = { elems, numelems, }
    ret res; tokidx
  }

  if ty(token.tagmeme) == Langblock {
    res <: LangblockFactor = { token.lexeme, token.tagmeme.body, token.tagmeme.arguments, }
    ret res; tokidx_+1
  }

  if ty(token.tagmeme) == Quote {
    res <: StringFactor = { token.lexeme, }
    ret res; tokidx_+1
  }

  if ty(token.tagmeme) == Num {
    res <: NumFactor = { cast(token.lexeme), }
    ret res; tokidx_+1
  }
  if ty(token.tagmeme) == Ident {
    tokidx = tokidx_ + 1

    if ty(tokens[tokidx].tagmeme) == LParen {
      tokidx = tokidx + 1

      arguments; numargs; tokidx = arguments(tokens, tokidx)

      res <: CallFactor = { token.lexeme, arguments, numargs, }
      ret res; tokidx
    }
    res <: IdentFactor = { token.lexeme, }
    ret res; tokidx
  }
  if ty(token.tagmeme) == Brace { if token.lexeme == "{" {
    tokidx : tokidx_ + 1
    print("LBrace Type init items")
    items; numitems; tokidx = itemsinit(tokens, tokidx)
    print(items)
    print(numitems)
    res <: TypeInitFactor = { items, numitems, }
    ret res; tokidx
  }}
  print("ERROR: Expected a NUM or IDENT or TYPE")
  print("Got:")
  print(tokens[tokidx_-1])
  print(token)
  print(tokens[tokidx_+1])
  ret token
}

def unary(tokens, tokidx_) {
  tokidx : tokidx_
  if ty(tokens[tokidx].tagmeme) == Carrot {
    tokidx : tokidx + 1

    print("Carrot")
    HS; tokidx : factor(tokens, tokidx)
    print(HS)

    HS <: UnaryCarrot = { HS, }
    ret HS; tokidx
  }
  if ty(tokens[tokidx].tagmeme) == Binop { if tokens[tokidx].lexeme == "-" {
    tokidx : tokidx + 1

    print("Unary negate")
    HS; tokidx : factor(tokens, tokidx)
    print(HS)

    HS <: UnaryNegate = { HS, }
    ret HS; tokidx
  }}

  HS; tokidx : factor(tokens, tokidx)
  while tokens[tokidx].tagmeme.x == 1 {
    if ty(tokens[tokidx].tagmeme) == Dot {
      tokidx : tokidx + 1
      print("Found member access")

      field; tokidx : identifier(tokens, tokidx)

      HS <: MemberFactor = { HS, field, }
    }
    if ty(tokens[tokidx].tagmeme) == LBrket {
      tokidx : tokidx + 1
      print("Found array access")

      expidx; tokidx = expression(tokens, tokidx)

      if ty(tokens[tokidx].tagmeme) == RBrket {
        tokidx : tokidx + 1

        HS <: AccessFactor = { HS, expidx, }
      } else {
        print("PARSE ERROR: expected an rbrcket")
        ret -1; -1
      }
    }
  }
  ret HS; tokidx
}

def term(tokens, tokidx_) {
  LHS; tokidx = unary(tokens, tokidx_)
  while ty(tokens[tokidx].tagmeme) == Trmop {
    tokidx = tokidx + 1

    RHS; tokidx = unary(tokens, tokidx)

    LHS <: BinopMul = { LHS, RHS, }
  }
  ret LHS; tokidx
}

def comparable(tokens, tokidx_) {
  LHS; tokidx : term(tokens, tokidx_)

  while ty(tokens[tokidx].tagmeme) == Binop {
    tokidx = tokidx + 1

    RHS; tokidx = term(tokens, tokidx)

    LHS <: BinopAdd = { LHS, RHS, }
  }
  ret LHS; tokidx
}

def structurable(tokens, tokidx_) {
  LHS; tokidx : comparable(tokens, tokidx_)

  tokidxx : tokidx
  while ty(tokens[tokidx].tagmeme) == Cmpop {
    tokidx = tokidx + 1

    RHS; tokidx = comparable(tokens, tokidx)

    if tokens[tokidxx].lexeme == "==" {
      LHS <: BinopEq = { LHS, RHS, }
    }
    if tokens[tokidxx].lexeme == "!=" {
      LHS <: BinopNq = { LHS, RHS, }
    }
    if tokens[tokidxx].lexeme == "<" {
      LHS <: BinopLt = { LHS, RHS, }
    }
  }
  ret LHS; tokidx
}

def expression(tokens, tokidx_) {
  LHS; tokidx : structurable(tokens, tokidx_)

  while ty(tokens[tokidx].tagmeme) == Semi {
    tokidx = tokidx + 1

    RHS; tokidx = structurable(tokens, tokidx)

    LHS <: Structure = { LHS, RHS, }
  }
  ret LHS; tokidx
}

def structured_identifiers(tokens, tokidx_) {
  LHS; tokidx : identifier(tokens, tokidx_)

  while ty(tokens[tokidx].tagmeme) == Semi {
    tokidx = tokidx + 1

    RHS; tokidx = identifier(tokens, tokidx)

    LHS <: NameStructure = { LHS, RHS, }
  }
  ret LHS; tokidx
}

def fields(tokens, tokidx__) {
  tokidx : tokidx__
  flds : emptylist
  while ty(tokens[tokidx].tagmeme) != Brace {
    ident; tokidx = identifier(tokens, tokidx)
    flds : flds + [ ident ]
  }
  if tokens[tokidx].lexeme == "}" {
    tokidx = tokidx + 1
    ret flds ; tokidx
  }
  print("ERROR: Expected an rbrace. Found:")
  print(tokens[tokidx-1])
  print(tokens[tokidx])
  print(tokens[tokidx+1])
}


def statements(tokens, tokidx__) {
  tokidx : tokidx__
  stmts : emptylist
  while ty(tokens[tokidx].tagmeme) != Brace {
    stmt; tokidx = statement(tokens, tokidx)
    stmts : stmts + [ stmt ]
  }
  if tokens[tokidx].lexeme == "}" {
    tokidx = tokidx + 1
    ret stmts ; tokidx
  }
  print("ERROR: Expected an rbrace. Found:")
  print(tokens[tokidx-1])
  print(tokens[tokidx])
  print(tokens[tokidx+1])
}

def statement(tokens, tokidx_) {
  if ty(tokens[tokidx_].tagmeme) == Control {
    tokidx = tokidx_ + 1

    if tokens[tokidx_].lexeme == "if" {
      control; tokidx = expression(tokens, tokidx)

      if ty(tokens[tokidx].tagmeme) == Brace { if tokens[tokidx].lexeme == "{" {
        tokidx = tokidx + 1

        print(tokidx)
        truepath; tokidx = statements(tokens, tokidx)

        falsepath = emptylist
        if ty(tokens[tokidx].tagmeme) == Control { if tokens[tokidx].lexeme == "else" {
          tokidx = tokidx + 1
          if ty(tokens[tokidx].tagmeme) == Brace { if tokens[tokidx].lexeme == "{" {
            tokidx = tokidx + 1
            falsepath; tokidx = statements(tokens, tokidx)
        }}}}

        res <: IfElseStatement = { control, truepath, falsepath, }
        ret res; tokidx
      }}
      print("Expected an lbrace")
    }
    if tokens[tokidx_].lexeme == "for" {
      tokidx : tokidx_ + 1

      loopvar; tokidx = identifier(tokens, tokidx)

      if ty(tokens[tokidx].tagmeme) == Inn {
        tokidx : tokidx + 1

        iterable; tokidx : expression(tokens, tokidx)

        if ty(tokens[tokidx].tagmeme) == Brace { if tokens[tokidx].lexeme == "{" {
          tokidx : tokidx + 1
          path; tokidx = statements(tokens, tokidx)

          res <: ForLoopStatement = { loopvar, iterable, path, }
          ret res; tokidx
        }}
        print("ERROR: parse statement, expected lbrace.")
      }
      print("ERROR: parse statement, expected 'in'.")
    }
    if tokens[tokidx_].lexeme == "while" {
      tokidx : tokidx_ + 1

      condition; tokidx = expression(tokens, tokidx)

      if ty(tokens[tokidx].tagmeme) == Brace { if tokens[tokidx].lexeme == "{" {
        tokidx : tokidx + 1
        path; tokidx = statements(tokens, tokidx)

        res <: WhileLoopStatement = { condition, path, }
        ret res; tokidx
      }}
      print("ERROR: parse statement, expected lbrace.")
    } 
    else {
      print("ERROR: parse statement. Instead found")
      print(tokens[tokidx_].lexeme)
    }
  }
  if ty(tokens[tokidx_].tagmeme) == Print {
    tokidx = tokidx_ + 1
    if ty(tokens[tokidx].tagmeme) == LParen {
      tokidx : tokidx + 1
    }

    HS; tokidx = expression(tokens, tokidx)

    if ty(tokens[tokidx].tagmeme) == RParen {
      tokidx : tokidx + 1
    }

    res <: PrintStatement = { HS, }
    ret res; tokidx
  }
  if ty(tokens[tokidx_].tagmeme) == Return {
    tokidx = tokidx_ + 1

    IS; tokidx = expression(tokens, tokidx)

    res <: ReturnStatement = { IS, }
    ret res; tokidx
  }
  if ty(tokens[tokidx_].tagmeme) == Type {
    tokidx = tokidx_ + 1

    typname; tokidx = identifier(tokens, tokidx)

    if ty(tokens[tokidx].tagmeme) == Brace { if tokens[tokidx].lexeme == "{" {
      tokidx = tokidx + 1

      flds; tokidx : fields(tokens, tokidx)

      res <: TypeDefinition = { typname, flds, }
      ret res; tokidx
    }}
    print("ERROR: expected an lbrace")
    ret -1; -1
  }

  if ty(tokens[tokidx_].tagmeme) == FnDefn {
    tokidx = tokidx_ + 1

    if ty(tokens[tokidx].tagmeme) == Brace { if tokens[tokidx].lexeme == "{" {
      tokidx = tokidx + 1

      path; tokidx = statements(tokens, tokidx)

      name = tokens[tokidx_].tagmeme.name
      params = tokens[tokidx_].tagmeme.parameters

      res <: FuncDefinition = { name, params, path, }
      ret res; tokidx
    }}
  }

  if ty(tokens[tokidx_].tagmeme) == Ident {
    tokidx : tokidx_
    token = tokens[tokidx_]
    if ty(tokens[tokidx+1].tagmeme) == LParen {
      tokidx = tokidx + 2

      arguments; numargs; tokidx = arguments(tokens, tokidx)

      res <: CallFactor = { token.lexeme, arguments, numargs, }
      ret res; tokidx
    }
  }

  if ty(tokens[tokidx_].tagmeme) == EndOfTokens {
    res <: EndOfStatements = { 0, }
    ret res; tokidx+1
  } else {
    LHS; tokidx = structured_identifiers(tokens, tokidx_)

    if ty(tokens[tokidx].tagmeme) == Punctuation {
      tokidx = tokidx + 1

      RHS; errtokidx = expression(tokens, tokidx)

      res <: AssignStatement = { LHS, RHS, }
      ret res; errtokidx
    }
    if ty(tokens[tokidx].tagmeme) == Typunc {
      tokidx : tokidx + 1
      print("Found typunc")

      typname; tokidx = identifier(tokens, tokidx)
      print(typname)
      if ty(tokens[tokidx].tagmeme) == Punctuation {
        tokidx = tokidx + 1

        RHS ; tokidx = expression(tokens, tokidx)
        print(RHS)

        res <: TypeStatement = { LHS, typname, RHS, }
        ret res; tokidx
      } else {
        print("ERROR: Expected a COLON here")
      }
    }
    else {
      print("ERROR: Expected an COLON")
      print("Got:")
      print(tokens[tokidx-1])
      print(tokens[tokidx])
      print(tokens[tokidx+1])
    }
  }
}

def parse_statements(tokens, tokidx_) {
  if ty(tokens[tokidx_].tagmeme) == EndOfTokens {
    res <: EndOfStatements = { 0, }
    ret [ res ]
  }

  stmt; tokidx = statement(tokens, tokidx_)

  ret [ stmt ] + parse_statements(tokens, tokidx)
}

stmts = parse_statements(tokens, 1)
print("##########")
print("Statements self:")

type String {
  strval
}

type SignatureDefn {
  parameters
  path
}

type TypeDefn {
  typname
  members
  idx
}

type Typd {
  vals
  numericidx
  typname
}

type Aggregate {
  vals
}

type FunctionObj {
  fn
}

type AbstractObject {
  name
  body
  arguments
}

type Closure {
  val
}

print("##########")
print("Execution self:")

def empty(k) { ret -80 }
def add(k, v, l_) {
  def ll(k1) {
    if k1.HS == k.HS { ret v }
    else { ret l_(k1) }
  }
  ret ll
}

symbols = new_set()

def normalize(valexpr_) {
  valexpr = valexpr_
  if ty(valexpr) == NumFactor {
    valexpr = valexpr.HS
  }
  if kind(valexpr) == kind(eval("True")) {
    valexpr = cast(valexpr)
  }
  if kind(valexpr) == kind(eval("3")) {
    valexpr = cast(valexpr)
  }
  ret valexpr
}

def string_normalize(strexpr_) {
  strexpr = strexpr_
  if ty(strexpr) == String {
   strexpr = single_quote() + str(strexpr.strval) + single_quote()
   strexpr = single_quote() + strexpr + single_quote()
   strexpr = single_quote() + strexpr + single_quote()
  }
  ret strexpr
}

def rewrite(exp, bound_var, new_expr) {
  if ty(exp) == FuncDefinition {
    print("Rewriting FuncDefintion")
    print(repr(bound_var))
    print(repr(new_expr))
    stmts : emptylist
    for stmt in exp.path {
      stmts : stmts + [ rewrite(stmt, bound_var, new_expr) ]
    }
    res <: FuncDefinition = { exp.name, exp.parameters, stmts, }
    print("FuncDefinition rewrote")
    ret res
  }
  if ty(exp) == TypeInitFactor {
    items : emptylist
    for item in exp.items {
      items : items + [ rewrite(item, bound_var, new_expr) ]
    }
    res <: TypeInitFactor = { items, exp.numitems, }
    ret res
  }
  if ty(exp) == AccessFactor {
    res <: AccessFactor = { rewrite(exp.this, bound_var, new_expr), rewrite(exp.expidx, bound_var, new_expr), }
    ret res
  }
  if ty(exp) == IfElseStatement {
    truepath : emptylist
    for point in exp.truepath {
      truepath : truepath + [ rewrite(point, bound_var, new_expr) ]
    }
    falsepath : emptylist
    for point in exp.falsepath {
      falsepath : falsepath + [ rewrite(point, bound_var, new_expr) ]
    }
    res <: IfElseStatement = { rewrite(exp.condition, bound_var, new_expr), truepath, falsepath, }
    ret res
  }
  if ty(exp) == ForLoopStatement {
    path : emptylist
    for point in exp.path {
      path : path + [ rewrite(point, bound_var, new_expr) ]
    }
    res <: ForLoopStatement = { exp.loopvar, rewrite(exp.iterable, bound_var, new_expr), path, }
    ret res
  }
  if ty(exp) == ListFactor {
    elems : emptylist
    for elem in exp.elems {
      elems : elems + [ rewrite(elem, bound_var, new_expr) ]
    }
    res <: ListFactor = { elems, exp.numelems, }
    ret res
  }
  if ty(exp) == LangblockFactor {
    res <: LangblockFactor = { exp.name, exp.body, exp.arguments, }
    ret res
  }
  if kind(exp) == kind(eval("3")) {
    ret exp
  }
  if kind(exp) == kind(eval("'string'")) {
    ret exp
  }
  if kind(exp) == kind(fn_kind()) {
    ret exp
  }
  if kind(exp) == kind(pattern()) {
    ret exp
  }
  if kind(exp) == kind(match_obj2()) {
    ret exp
  }
  if kind(exp) == kind(match_obj()) {
    ret exp
  }
  if ty(exp) == StringFactor {
    ret exp
  }
  if ty(exp) == CallFactor {
    args : emptylist
    numargs : exp.numargs
    for arg in exp.argument {
      args = args + [ rewrite(arg, bound_var, new_expr) ]
    }
    print("Rewriting CallFactor")
    print(exp.HS)
    print(bound_var)
    print(new_expr)
    if exp.HS == bound_var {
      print("Found match hehe")
      print(new_expr)
      res <: CallFactor = { new_expr, args, numargs, }
    } else {
      res <: CallFactor = { exp.HS, args, numargs, }
    }
    ret res
  }
  if ty(exp) == ReturnStatement {
    print("Rewriting return")
    print(exp)
    res <: ReturnStatement = { rewrite(exp.IS, bound_var, new_expr), }
    ret res
  }
  if ty(exp) == NumFactor {
    ret exp
  }
  if ty(exp) == IdentFactor {
    if exp.HS == bound_var {
      ret new_expr
    } else {
      ret exp
    }
  }
  if ty(exp) == PrintStatement {
    res <: PrintStatement = { rewrite(exp.HS, bound_var, new_expr), }
    ret res
  }
  if ty(exp) == UnaryNegate {
    res <: UnaryNegate = { rewrite(exp.HS, bound_var, new_expr), }
    ret res
  }
  if ty(exp) == UnaryCarrot {
    res <: UnaryCarrot = { rewrite(exp.HS, bound_var, new_expr), }
    ret res
  }
  if ty(exp) == BinopEq {
    res <: BinopEq = { rewrite(exp.LHS, bound_var, new_expr), rewrite(exp.RHS, bound_var, new_expr), }
    ret res
  }
  if ty(exp) == BinopAdd {
    res <: BinopAdd = { rewrite(exp.LHS, bound_var, new_expr), rewrite(exp.RHS, bound_var, new_expr), }
    ret res
  }
  if ty(exp) == TypeStatement {
    res <: TypeStatement = { rewrite(exp.LHS, bound_var, new_expr), exp.typname, rewrite(exp.RHS, bound_var, new_expr), }
    ret res
  }
  if ty(exp) == AssignStatement {
    res <: AssignStatement = { rewrite(exp.LHS, bound_var, new_expr), rewrite(exp.RHS, bound_var, new_expr), }
    ret res
  } else {
    ret exp
    print("ERROR: Rewrite case missing")
    print(ty(exp))
    print(exp)
    ret -1
  }
}

def add_names(names_, structure_, symbols_) {
  names : names_
  structure : structure_
  symbols : symbols_
  if ty(names.LHS) == NameStructure {
    symbols : add_names(names.LHS, structure.LHS, symbols)
    symbols : add_to_set(names.RHS, structure.RHS, symbols)
    ret symbols
  }
  else {
    symbols : add_to_set(names.LHS, structure.LHS, symbols)
    symbols : add_to_set(names.RHS, structure.RHS, symbols)
    ret symbols
  }
}

def rewrite_path(parameter_, path_, val_) {
  parameter = parameter_
  path = path_
  val = val_

  stmts = emptylist
  for stmt in path {
    print("Rewriting")
    print(stmt)
    stmts : stmts + [ rewrite(stmt, parameter, val) ]
  }
  ret stmts
}

def asdf(here, hi, symbols) {
  print("here5")
  symbols_ : symbols
  exp : here
  valR = execute_expression(exp.RHS, symbols)
  print("here6")
  if ty(exp.LHS) == Structure {
    print("here7")
    valL = asdf(exp.LHS, symbols)
    res <: Structure : { valL, valR, }
    ret res
  }
  else {
    print("here8")
    valL = execute_expression(exp.LHS, symbols)
    res <: Structure : { valL, valR, }
    ret res
  }
}

def execute_callfactor(exp, symbols) {
    print("## Call Factor")
    print(exp.HS)
    if exp.HS == "ty" {
      print(exp)
      arg : exp.argument[0]
      print("Evaluate builtin ty")
      print(arg)
      val : execute_expression(arg, symbols)
      print("Result")
      print(val)
      numericIdent <: IdentFactor = { str(val.numericidx), }
      print("Findingty")
      symval : find_in_set(symbols, numericIdent)
      print(symval)
      res : cast(symval.idx)
      ret res
    }

    if exp.numargs == 3 {
      print("my_eval3")

      args : exp.argument
      fnvarname <: IdentFactor = { exp.HS, }
      print("Finding3")
      fnval : find_in_set(symbols, fnvarname)

      if ty(fnval) == SignatureDefn {
        print("SignatureDefn")
        print(fnvarname)
        print(fnval)
        params : split(fnval.parameters)
        print(params)

        val = normalize(execute_expression(args[0], symbols))
        val2 = normalize(execute_expression(args[1], symbols))
        val3 = normalize(execute_expression(args[2], symbols))

        vparam0 <: IdentFactor = { strp(params[0]), }
        vparam1 <: IdentFactor = { strp(params[1]), }
        vparam2 <: IdentFactor = { strp(params[2]), }
        symbols_ : add_to_set(vparam0, val, symbols)
        symbols_ : add_to_set(vparam1, val2, symbols_)
        symbols_ : add_to_set(vparam2, val3, symbols_)

        if ty(val) == Closure {
          print("Found closure30")
        }
        if ty(val2) == Closure {
          print("Found closure31")
        }
        if ty(val3) == Closure {
          print("Found closure31")
        }

        if params[1] == " contents_" {
          print("CONTENTS")
        } else {
          print(val2)
        }

        closure_val <: Closure = { val, }
        closure_val2 <: Closure = { val2, }
        closure_val3 <: Closure = { val3, }
        print(repr(params[0]))
        print(repr(params[1]))
        print(repr(params[2]))
        rewritten_path = rewrite_path(strp(params[0]), fnval.path, closure_val)
        rewritten_path = rewrite_path(strp(params[1]), rewritten_path, closure_val2)
        rewritten_path = rewrite_path(strp(params[2]), rewritten_path, closure_val3)
        res : execute_path(rewritten_path, symbols_)
        ret res
      } else {
        val = normalize(execute_expression(args[0], symbols))
        val2 = normalize(execute_expression(args[1], symbols))
        val3 = normalize(execute_expression(args[2], symbols))
        if ty(val) == Closure { val : val.val }
        if ty(val2) == Closure { val2 : val2.val }
        if ty(val3) == Closure { val3 : val3.val }
        if kind(val) == kind(eval("'string'")) {
          if doesnt_contain_triple(val) {
            val : '"' + '"' + '"' + val + '"' + '"' + '"'
          }
        }
        print(ty(val2))
        print(kind(val2))
        if kind(val2) == kind(eval("'string'")) {
          if doesnt_contain_triple(val2) {
            val2 : '"' + '"' + '"' + val2 + '"' + '"' + '"'
          }
        }
        if kind(val3) == kind(eval("'string'")) {
          if doesnt_contain_triple(val3) {
            val3 : '"' + '"' + '"' + val3 + '"' + '"' + '"'
          }
        }
        s : exp.HS + "(" + str(val) + "," + str(val2) + "," + str(val3) + ")"
        print("my_eval on")
        print(s)
        res : my_eval(s, exp.HS, fnval)
        ret res
      }
    }

    if exp.numargs == 2 {
      print("my_eval2")

      args : exp.argument
      fnvarname <: IdentFactor = { exp.HS, }
      print("Finding2")
      fnval : find_in_set(symbols, fnvarname)
      val = normalize(execute_expression(args[0], symbols))
      val2 = normalize(execute_expression(args[1], symbols))
      if fnval != -100 {
        if ty(fnval) == SignatureDefn {
          print("SignatureDefn")
          print(fnvarname)
          print(fnval)
          params : split(fnval.parameters)
          print(params)
        
        
          vparam0 <: IdentFactor = { strp(params[0]), }
          vparam1 <: IdentFactor = { strp(params[1]), }
          symbols_ : add_to_set(vparam0, val, symbols)
          symbols_ : add_to_set(vparam1, val2, symbols_)
        
          if ty(val) == Closure {
            print("Found closure20")
            print("end")
          }
          if ty(val2) == Closure {
            print("Found closure21")
          }
        
          if params[1] == " contents_" {
            print("CONTENTS")
          } else {
            print(val2)
          }
        
          closure_val <: Closure = { val, }
          closure_val2 <: Closure = { val2, }
          print(repr(params[0]))
          print(repr(params[1]))
          rewritten_path = rewrite_path(strp(params[0]), fnval.path, closure_val)
          rewritten_path = rewrite_path(strp(params[1]), rewritten_path, closure_val2)
          res : execute_path(rewritten_path, symbols_)
          ret res
        } else {
          val = normalize(execute_expression(args[0], symbols))
          val2 = normalize(execute_expression(args[1], symbols))
          if ty(val) == Closure { val : val.val }
          if ty(val2) == Closure { val2 : val2.val }
          if kind(val) == kind(eval("'string'")) {
            if doesnt_contain_triple(val) {
              val : '"' + '"' + '"' + val + '"' + '"' + '"'
            }
          }
          print(ty(val2))
          print(kind(val2))
          if kind(val2) == kind(eval("'string'")) {
            if doesnt_contain_triple(val2) {
              val2 : '"' + '"' + '"' + val2 + '"' + '"' + '"'
            }
          }
          s : exp.HS + "(" + str(val) + "," + str(val2) + ")"
          print("my_eval on")
          print(s)
          res : my_eval(s, exp.HS, fnval)
          ret res
        }
      } else {
        if ty(val) == Closure { val : val.val }
        if kind(val) == kind(eval("'string'")) {
          if doesnt_contain_triple(val) {
            val : '"' + '"' + '"' + strp(val) + '"' + '"' + '"'
            val : remove_quadruple(val) 
          }
        }
        if ty(val2) == Closure { val2 : val2.val }
        if kind(val2) == kind(eval("'string'")) {
          if doesnt_contain_triple(val2) {
            val2 : '"' + '"' + '"' + strp(val2) + '"' + '"' + '"'
            val2 : remove_quadruple(val2) 
          }
        }
        if ty(val) == String { val : val.strval }
        if ty(val2) == String { val2 : val2.strval }
        res : fval2(exp.HS, val, val2)
        print("Result is")
        print(res)
        ret res
      }
    }
    if exp.numargs == 1 {
      print("my_eval1")
      print(exp.HS)
      args : exp.argument
      val = normalize(execute_expression(args[0], symbols))
      fnvarname <: IdentFactor = { exp.HS, }
      fnval : find_in_set(symbols, fnvarname)
      if ty(val) == Closure {
        print("Found closure1")
      }
      if fnval != -100 {
        if ty(fnval) == SignatureDefn {
          print("SignatureDefn")
          print(fnvarname)
          print(fnval)
        
          param : fnval.parameters
          vparam0 <: IdentFactor = { strp(param), }
          symbols_ : add_to_set(vparam0, val, symbols)
        
          closure_val <: Closure = { val, }
          rewritten_path = rewrite_path(strp(fnval.parameters), fnval.path, closure_val)
          res : execute_path(rewritten_path, symbols_)
          ret res
        } else {
          print("my_eval")
          print(val)
          if kind(val) == match_obj2() {
            print("Found match_obj2")
            print(exp.HS)
            res : fval(val, exp.HS)
            ret res
          }
          if ty(val) == Closure { val : val.val }
          if kind(val) == kind(eval("'string'")) {
            if doesnt_contain_triple(val) {
              val : '"' + '"' + '"' + strp(val) + '"' + '"' + '"'
              val : remove_quadruple(val) 
            }
          }
          print(val)
          print(exp.HS)
          if kind(exp.HS) == fn_kind() {
            res : fn_eval(exp.HS, val)
            ret res 
          } else {
            s : exp.HS + "(" + str(val) + ")"
            print(s)
            res : my_eval(s, exp.HS, fnval)
            ret res
          }
        }
      } else {
        if kind(val) == match_obj2() {
          print("Found match_obj2")
          print(exp.HS)
          res : fval1(exp.HS, val)
          ret res
        }
        print(exp)
        print(args)
        print(val)
        if ty(val) == Closure { val : val.val }
        if kind(val) == kind(eval("'string'")) {
          if doesnt_contain_triple(val) {
            val : '"' + '"' + '"' + strp(val) + '"' + '"' + '"'
            print(val)
            val : remove_quadruple(val) 
          }
        }
        print(val)
        res : fval1(exp.HS, val)
        print("Result is")
        print(res)
        ret res
      }
    }
    if exp.numargs == 0 {
      print("my_eval0")
      fnvarname <: IdentFactor = { exp.HS, }
      print("Finding0")
      print(symbols)
      fnval : find_in_set(symbols, fnvarname)
      if fnval != -100 {
        if ty(fnval) == SignatureDefn {
          res : execute_path(fnval.path, symbols)
          ret res
        } else {
          s : exp.HS + "(" + ")"
          print(s)
          res : my_eval(s, exp.HS, fnval)
          ret res
        }
      } else {
        res : fval0(exp.HS)
        ret res
      }
    }

}

def execute_expression(exp, symbols) {
  print("execute_expression#####")
  res : execute_expression_print(exp, symbols)
  if ty(exp) == IdentFactor { if exp.HS == "spl" {
    ret res
  }}
  ret res 
}

def execute_expression_print(exp, symbols) {
  if ty(exp) == Structure {
    print("Structure")
    print(exp)
    valL = execute_expression(exp.LHS, symbols)
    valR = execute_expression(exp.RHS, symbols)
    val <: Structure : { valL, valR, }
    print("End")
    ret val
  }
  if ty(exp) == UnaryCarrot {
    vall = execute_expression(exp.HS, symbols)
    print(ty(vall))
    if ty(vall) == AbstractObject {
      print("here3")
      argument : vall.arguments
      print(argument)
      if argument == -7 {
        vall : vval(vall.body)
      } else {
        print("Findingcarrot")
        argval : normalize(find_in_set(symbols, argument))
        vall : tval(vall.body, argument.HS, argval)
      }
      ret vall
    }
    print("RUNTIME ERROR: expected an AbstractObject")
    print(vall)
  }
  if ty(exp) == UnaryNegate {
    print("UnaryNegate")
    print(exp)
    valll = normalize(execute_expression(exp.HS, symbols))
    ret -valll
  }
  if ty(exp) == LangblockFactor {
    res <: AbstractObject = { exp.name, exp.body, exp.arguments, }
    ret res
  }
  if ty(exp) == AccessFactor {
    val : normalize(execute_expression(exp.this, symbols))
    if ty(val) == Closure { val : val.val }
    idx : normalize(execute_expression(exp.expidx, symbols))
    print("Access factor")
    print(ty(val))
    print(kind(val))
    cb_print(val)
    print(idx)
    ret val[idx]
  }
  if ty(exp) == MemberFactor {
    print("## execute_expression MemberFactor")
    val = execute_expression(exp.this, symbols)
    i : 0
    aggr : val.vals
    nameidx <: IdentFactor = { str(val.numericidx), }
    print("Findingmember")
    tydef : find_in_set(symbols, nameidx)

    values : aggr.vals
    for member in tydef.members {
      expmem : exp.member
      if member.HS == expmem.HS {
        print("Returning MemberFactor member value")
        print(values[i])
        ret values[i]
      }
      i : i + 1
    }
    ret -1
  }
  if ty(exp) == CallFactor {
    ret execute_callfactor(exp, symbols)
  }
  if ty(exp) == BinopMul {
    valL = normalize(execute_expression(exp.LHS, symbols))
    valR = normalize(execute_expression(exp.RHS, symbols))
    ret valL * valR
  }
  if ty(exp) == BinopAdd {
    valL = normalize(execute_expression(exp.LHS, symbols))
    valR = normalize(execute_expression(exp.RHS, symbols))
    if ty(valL) == String { valL = rem_quo(valL.strval) }
    if ty(valR) == String { valR = rem_quo(valR.strval) }
    ret valL + valR
  }
  if ty(exp) == BinopEq {
    valL = normalize(execute_expression(exp.LHS, symbols))
    valR = normalize(execute_expression(exp.RHS, symbols))
    print("BinopEq of vals")
    print(valL)
    print(valR)
    print(ty(valL))
    print(ty(valR))
    ret valL == valR
  }
  if ty(exp) == BinopNq {
    valL = normalize(execute_expression(exp.LHS, symbols))
    valR = normalize(execute_expression(exp.RHS, symbols))
    ret valL != valR
  }
  if ty(exp) == BinopLt {
    valL = normalize(execute_expression(exp.LHS, symbols))
    valR = normalize(execute_expression(exp.RHS, symbols))
    ret valL < valR
  }
  if ty(exp) == StringFactor {
    res <: String = { exp.HS, }
    ret res
  }
  if ty(exp) == String {
    ret exp
  }
  if ty(exp) == ListFactor {
    res : emptylist
    for elem in exp.elems {
      val : execute_expression(elem, symbols)
      if ty(val) == String { val = val.strval }
      res : res + [ normalize(val) ]
    }
    ret res
  }
  if ty(exp) == TypeInitFactor {
    res : emptylist
    print("TypeInitFactor")
    for item in exp.items {
      val : execute_expression(item, symbols)
      if ty(val) == String { val = val.strval }
      res : res + [ normalize(val) ]
    }
    res <: Aggregate = { res, }
    ret res
  }
  if ty(exp) == ReturnStatement {
    print("here1")
    ret exp
  }
  if kind(exp) == fn_kind() {
    fn <: FunctionObj = { exp, }
    ret fn
  }
  if ty(exp) == FunctionObj {
    ret exp
  }
  if kind(exp) == pattern() {
    ret exp
  }
  if kind(exp) == match_obj2() {
    ret exp
  }
  if kind(exp) == match_obj() {
    ret exp
  }
  if ty(exp) == NumFactor {
    ret exp
  }
  if ty(exp) == Closure {
    ret exp.val
  }
  if ty(exp) == IdentFactor {
    r : find_in_set(symbols, exp)
    if ty(r) == Typd {
      ret r
    }
    if ty(r) == SignatureDefn {
      print("Found a SignatureDefn")
      print(exp)
      print(r)
      ret r
    }
    ret r
  }
  if kind(exp) == kind(eval("'string'")) {
    ret exp
  }
  if kind(exp) == kind(eval("3")) {
    ret exp
  }
  if kind(exp) == kind(eval("[]")) {
    ret exp
  }
  else {
    print("ERROR: execute_expression found:")
    print(exp)
  }
}

def evaluate_builtin_ty(arg___, symbols) {
  print("Evaluate builtin tyyy")
  print(symbols)
  val : symbols(arg___)
  print(val)
  ret ty(arg___)
  print("BUILTIN ERROR: ty")
  print(arg)
  ret -9
}


def execute_path(path, symbols_) {
  print("In execute_path")
  symbols : symbols_
  for stmt in path {
    cr_print(stmt)
    symbols; rtnstmt = execute(stmt, symbols)
    if ty(rtnstmt) == ReturnStatement {
      print("Found a return")
      rtn = execute_expression(rtnstmt.IS, symbols)
      print("Returning evaluation of return")
      print(rtn)
      ret rtn
    }
  }
  print("No return")
  ret -8
}

def execute_statements(_stmts, _symbols) {
  thesesymbols = _symbols
  for mystmt in _stmts {
    thesesymbols; rtnstmt = execute(mystmt, thesesymbols)
    if ty(rtnstmt) == ReturnStatement {
      ret thesesymbols; rtnstmt
    }
  }
  ret thesesymbols; -1
}

def recur_remove_String(s) {
  if ty(s) == String {
    ret recur_remove_String(s.strval)
  }
  ret s
}

def execute(stmt, symbols) {
  cr_print("execute_statement#####")
  if ty(stmt) == IfElseStatement {
    eval_condition : normalize(execute_expression(stmt.condition, symbols))
    thesesyms : symbols
    if eval_condition {
      print("Executing true")
      thesesyms; rtnstmt = execute_statements(stmt.truepath, thesesyms)
      if ty(rtnstmt) == ReturnStatement {
        ret thesesyms; rtnstmt
      }
    } else {
      if stmt.falsepath == emptylist {
        print("Nonexistent false")
      } else {
        print("Executing false")
        thesesyms; rtnstmt = execute_statements(stmt.falsepath, thesesyms)
        if ty(rtnstmt) == ReturnStatement {
	  print("Found a return falsepath")
          ret thesesyms; rtnstmt
        }
      }
    }
    ret thesesyms; -1
  }
  if ty(stmt) == FuncDefinition {
    name <: IdentFactor = { stmt.name, }
    param_body <: SignatureDefn = { stmt.parameters, stmt.path, }
    print("Adding a FuncDefinition to symbols")
    print(stmt)
    symbols_ = add_to_set(name, param_body, symbols)
    ret symbols_; -1
  }
  if ty(stmt) == TypeDefinition {
    idx : getnext()
    numericname <: IdentFactor = { str(idx), }
    name_fields <: TypeDefn = { stmt.typname, stmt.members, idx, }
    symbols_ = add_to_set(stmt.typname, cast(idx), symbols)
    symbols_ = add_to_set(numericname, name_fields, symbols_)
    ret symbols_; -1
  }
  if ty(stmt) == AssignStatement {
    cb_print(stmt)
    if ty(stmt.LHS) == NameStructure {
      names = stmt.LHS
      eval_rhs = execute_expression(stmt.RHS, symbols)
      symbols_ = add_names(names, eval_rhs, symbols)
      ret symbols_; -1
    } else {
      eval_rhs = execute_expression(stmt.RHS, symbols)
      symbols_ = add_to_set(stmt.LHS, eval_rhs, symbols)
      ret symbols_; -1
    }
  }
  if ty(stmt) == TypeStatement {
    print("Findingtypestmt")
    numericidx = find_in_set(symbols, stmt.typname)
    eval_rhs = execute_expression(stmt.RHS, symbols)
    typd_rhs <: Typd = { eval_rhs, cast(numericidx), stmt.typname, }
    symbols_ = add_to_set(stmt.LHS, typd_rhs, symbols)
    ret symbols_; -1
  }
  if ty(stmt) == ReturnStatement {
    print("Returning return statement")
    print(stmt)
    ret symbols; stmt
  }
  if ty(stmt) == PrintStatement {
    val = execute_expression(stmt.HS, symbols)
    if ty(val) == Closure {
      val : execute_expression(val.val, symbols)
    }
    if ty(val) == ListFactor { val = val.elems }
    if ty(val) == String { val = val.strval }
    if ty(val) == Typd {
      aggr = val.vals
      idx = val.numericidx
      nameidx <: IdentFactor = { str(idx), }
      print("Findingtypd")
      typdefn : find_in_set(symbols, nameidx)

      if ty(aggr) == Aggregate {
        membs : typdefn.members
        i : 0
        for e in aggr.vals {
          memb : membs[i]
          print(memb.HS + " " + str(e))
          i : i + 1
        }
      } else {
        print("RUNTIME ERROR: expected an aggregate")
      }
      ret symbols; -1
    }
    if ty(val) == AbstractObject {
      res : "AbstractObject(" + str(val.body) + ")"
      print(res)
      ret symbols; -1
    }

    if ty(val) == TypeDefn {
      print("Found typdefn")
      res : "TypeDefn(" + str(val) + ")"
      print(res)
      ret symbols; -1
    }
    if ty(val) == String { val = recur_remove_String(val.strval) }

    if ty(val) == SignatureDefn {
      print("Found signaturedefn")
      res : "SignatureDefn(" + str(val.parameters) + str(val.path) + ")"
      print(res)
      ret symbols; -1
    }

    if kind(val) == kind(eval("[]")) {
      print(val)
      ret symbols; -1
    }
    if kind(val) == kind(eval("int")) {
      print(val)
      ret symbols; -1
    }
    if kind(val) == kind(eval("{}")) {
      print(val)
      ret symbols; -1
    }
    if kind(val) == kind(eval("'string'")) {
      print(val)
      ret symbols; -1
    }
    if kind(val) == kind(eval("True")) {
      print(val)
      ret symbols; -1
    }
    if kind(val) == fn_kind() {
      print("Printing a function")
      print("function")
      ret symbols; -1
    }
    if kind(val) == pattern() {
      print("Printing a pattern")
      print(val)
      ret symbols; -1
    }
    if kind(val) == match_obj2() {
      print("Printing a match_obj")
      print(val)
      ret symbols; -1
    }
    if kind(val) == match_obj() {
      print("Printing a match_obj")
      print(val)
      ret symbols; -1
    }
    if kind(val) == kind(eval("dir")) {
      print(val)
      ret symbols; -1
    }
    if kind(val) == kind(eval("None")) {
      print(val)
      ret symbols; -1
    }
    if kind(val) == kind(eval("3")) {
      print(val)
      ret symbols; -1
    } else {
      cb_print(ty(val))
      cb_print(String)
      if ty(val) == String {
        print(val.strval)
      } else {
        print(val.HS)
        ret symbols; -1
      }
    }
    ret symbols; -1
  }
  if ty(stmt) == ForLoopStatement {
    symbols_ = symbols
    iter = execute_expression(stmt.iterable, symbols_)
    for e in iter {
      symbols_ = add_to_set(stmt.loopvar, e, symbols_)
      symbols_; rtnstmt = execute_statements(stmt.path, symbols_)
      if ty(rtnstmt) == ReturnStatement {
        ret symbols_; rtnstmt
      }
    }
    ret symbols_; -1
  }
  if ty(stmt) == CallFactor {
    execute_callfactor(stmt, symbols)
    ret symbols; -1
  }
  if ty(stmt) == WhileLoopStatement {
    symbols_ = symbols
    condition = execute_expression(stmt.condition, symbols_)
    while condition {
      symbols_; rtnstmt = execute_statements(stmt.path, symbols_)
      if ty(rtnstmt) == ReturnStatement {
        ret symbols_; rntstmt
      }
      condition = execute_expression(stmt.condition, symbols_)
    }
    ret symbols_; -1
  }
  if ty(stmt) == EndOfStatements {
    ret symbols; -1
  }
  else {
    print("ERROR: execute (statement) found:")
    print(stmt)
    print("In:")
    print(symbols)
  }
}

for stmt in stmts {
  symbols; rtnstmt = execute(stmt, symbols)
}
