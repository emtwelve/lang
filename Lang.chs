{-# LANGUAGE FlexibleContexts, DeriveAnyClass, UndecidableInstances #-}
-- file: Lang.chs
module Lang where
import Foreign
import Foreign.C
import Foreign.C.String
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec
import GHC.Float
import Control.Monad
import System.IO.Unsafe
import GHC.Base(returnIO, breakpoint)
import Debug.Trace

#include "ffi_lang.h"

{#enum expr_cons_e as ExprCons {} deriving (Show) #}

genToEnum :: (Integral a, Enum b) => a -> b
genToEnum = toEnum . fromIntegral

genFromEnum :: (Integral a, Enum b) => b -> a
genFromEnum = fromIntegral . fromEnum

data Expr a = Num Int32
          | Var a
          | Dec Float
          | Negate (Expr a)
          | Add (Expr a) (Expr a)
          | Sub (Expr a) (Expr a)
          | Mult (Expr a) (Expr a)
          | Div (Expr a) (Expr a)
          | Assign (Expr a) (Expr a)
          | Bundle [Expr a] Int
          | Print (Expr a)
          | Call (a) [Expr a] Int
          | FuncDef (a) [Expr a] Int (Expr a)
          deriving (Show)

data C_Expr a = C_Num Int32
          | C_Var (Ptr a)
          | C_Negate (Ptr (C_Expr a))
          | C_Add (Ptr (C_Expr a)) (Ptr (C_Expr a))
          | C_Sub (Ptr (C_Expr a)) (Ptr (C_Expr a))
          | C_Mult (Ptr (C_Expr a)) (Ptr (C_Expr a))
          | C_Div (Ptr (C_Expr a)) (Ptr (C_Expr a))
          | C_Assign (Ptr (C_Expr a)) (Ptr (C_Expr a))
          | C_Print (Ptr (C_Expr a))
          | C_Call (Ptr a) (Ptr (C_Expr a)) Int32
          | C_FuncDef (Ptr a) (Ptr (Expr a)) Int32 (Ptr (Expr a)) Int32
          deriving (Show)

instance Storable (C_Expr a) where
  sizeOf _ = {#sizeof expr_s#}
  alignment _ = {#alignof expr_s#}
  peek p = do
    typ <- genToEnum <$> {#get struct expr_s->expr_cons#} p
    print(typ)
    case typ of
      EXPRINT -> C_Num . (\(CInt x) -> x)
                 <$> {#get struct expr_s->expr_value.expr_int #} p
      EXPRADD -> do
        q1 <- {#get struct expr_s->expr_value.expr_add.left#} p
        q2 <- {#get struct expr_s->expr_value.expr_add.right#} p
        return $ C_Add (castPtr q1) (castPtr q2)
      EXPRSUB -> do
        q1 <- {#get struct expr_s->expr_value.expr_sub.left#} p
        q2 <- {#get struct expr_s->expr_value.expr_sub.right#} p
        return $ C_Sub (castPtr q1) (castPtr q2)
      EXPRMULT -> do
        q1 <- {#get struct expr_s->expr_value.expr_mult.left#} p
        q2 <- {#get struct expr_s->expr_value.expr_mult.right#} p
        return $ C_Mult (castPtr q1) (castPtr q2)
      EXPRASSIGN -> do
        q1 <- {#get struct expr_s->expr_value.expr_assign.left#} p
        q2 <- {#get struct expr_s->expr_value.expr_assign.right#} p
        return $ C_Assign (castPtr q1) (castPtr q2)
      EXPRVAR -> C_Var . castPtr
                 <$> {#get struct expr_s->expr_value.expr_var #} p
      EXPRPRINT -> do
        q1 <- {#get struct expr_s->expr_value.expr_print#} p
        return $ C_Print (castPtr q1)
      EXPRCALL -> do
        qname <- {#get struct expr_s->expr_value.expr_call.name#} p
        q2 <- {#get struct expr_s->expr_value.expr_call.arg#} p
        q3 <- returnIO $ (\(CInt x)->x) $ unsafePerformIO $ {#get struct expr_s->expr_value.expr_call.argcount#} p
        return $ C_Call (castPtr qname) (castPtr q2) q3
      EXPRFUNCDEF -> do
        qname <- {#get struct expr_s->expr_value.expr_fndef.name#} p
        q2 <- {#get struct expr_s->expr_value.expr_fndef.arg#} p
        q3 <- returnIO $ (\(CInt x)->x) $ unsafePerformIO $ {#get struct expr_s->expr_value.expr_fndef.argcount#} p
        q4 <- {#get struct expr_s->expr_value.expr_fndef.stmts#} p
        q5 <- returnIO $ (\(CInt x)->x) $ unsafePerformIO $ {#get struct expr_s->expr_value.expr_fndef.stmtcount#} p
        return $ C_FuncDef (castPtr qname) (castPtr q2) q3 (castPtr q4) q5
      
  poke p t = do
    case t of
      C_Var q -> do
        tag EXPRVAR
        {#set struct expr_s->expr_value.expr_var#} p (castPtr q)
      C_Print q -> do
        tag EXPRPRINT
        {#set struct expr_s->expr_value.expr_print#} p (castPtr q)
      C_Call n q c -> do
        tag EXPRCALL
        {#set struct expr_s->expr_value.expr_call.name#} p (castPtr n)
        {#set struct expr_s->expr_value.expr_call.arg#} p (castPtr q)
        {#set struct expr_s->expr_value.expr_call.argcount#} p (CInt c)
      C_FuncDef n a ac s sc -> do
        tag EXPRFUNCDEF
        {#set struct expr_s->expr_value.expr_fndef.name #} p (castPtr n)
        {#set struct expr_s->expr_value.expr_fndef.arg#} p (castPtr a)
        {#set struct expr_s->expr_value.expr_fndef.argcount#} p (CInt ac)
        {#set struct expr_s->expr_value.expr_fndef.stmts#} p (castPtr s)
        {#set struct expr_s->expr_value.expr_fndef.stmtcount#} p (CInt sc)
      C_Num x -> do
        tag EXPRINT
        {#set struct expr_s->expr_value.expr_int#} p (CInt x)
      C_Add q1 q2 -> do
        tag EXPRADD
        {#set struct expr_s->expr_value.expr_add.left #} p (castPtr q1)
        {#set struct expr_s->expr_value.expr_add.right#} p (castPtr q2)
      C_Sub q1 q2 -> do
        tag EXPRSUB
        {#set struct expr_s->expr_value.expr_sub.left #} p (castPtr q1)
        {#set struct expr_s->expr_value.expr_sub.right#} p (castPtr q2)
      C_Mult q1 q2 -> do
        tag EXPRMULT
        {#set struct expr_s->expr_value.expr_mult.left #} p (castPtr q1)
        {#set struct expr_s->expr_value.expr_mult.right#} p (castPtr q2)
      C_Assign q1 q2 -> do
        tag EXPRASSIGN
        {#set struct expr_s->expr_value.expr_assign.left #} p (castPtr q1)
        {#set struct expr_s->expr_value.expr_assign.right#} p (castPtr q2)
      where
        tag = {#set struct expr_s->expr_cons #} p . genFromEnum

-- Symbol marshalling
newtype Symbol = Symbol String deriving (Show)

newExpr :: (Show a) => (a -> IO (Ptr a)) -> Expr a -> IO (Ptr (C_Expr a))
newExpr newA t = do
  case t of
    Num x -> do
      p <- malloc
      poke p (C_Num x)
      return (castPtr p)
    Var x -> do
      p <- malloc
      poke p . C_Var =<< newA x
      return (castPtr p)
    Add t1 t2 -> do
      p <- malloc
      q1 <- newExpr newA t1
      q2 <- newExpr newA t2
      poke p (C_Add (castPtr q1) (castPtr q2))
      return (castPtr p)
    Print t -> do
      p <- malloc
      q <- newExpr newA t
      poke p (C_Print (castPtr q))
      return (castPtr p)
    Call name (t:ts) n -> do
      parg <- mallocArray n
      q <- returnIO $ newExprs newA (t:ts)
      qq <- returnIO $ fmap (unsafePerformIO . peek . unsafePerformIO) (newExprs newA (t:ts))
      pokeArray0 (C_Num (negate 1)) parg qq
      p <- malloc
      qname <- newA name
      poke p (C_Call (qname) (castPtr parg) (fromIntegral n))
      return (castPtr p)
    FuncDef name (t:ts) n b -> do

      parg <- mallocArray n
      qarg <- returnIO $ fmap (unsafePerformIO . peek . unsafePerformIO) (newExprs newA (t:ts))
      pokeArray0 (C_Num (negate 1)) parg qarg

      stmts <- returnIO $ (\(Bundle x n) -> x) b
      nstmts <- returnIO $ (\(Bundle x n) -> n) b
      pstmts <- mallocArray nstmts
      qstmts <- returnIO $ fmap (unsafePerformIO . peek . unsafePerformIO) (newExprs newA stmts)
      pokeArray0 (C_Num (negate 1)) pstmts qstmts

      p <- malloc
      qname <- newA name
      poke p (C_FuncDef (qname) (castPtr parg) (fromIntegral n) (castPtr pstmts) (fromIntegral nstmts))
      return (castPtr p)

    Sub t1 t2 -> do
      p <- malloc
      q1 <- newExpr newA t1
      q2 <- newExpr newA t2
      poke p (C_Sub (castPtr q1) (castPtr q2))
      return (castPtr p)
    Mult t1 t2 -> do
      p <- malloc
      q1 <- newExpr newA t1
      q2 <- newExpr newA t2
      poke p (C_Mult (castPtr q1) (castPtr q2))
      return (castPtr p)
    Assign t1 t2 -> do
      p <- malloc
      q1 <- newExpr newA t1
      q2 <- newExpr newA t2
      poke p (C_Assign (castPtr q1) (castPtr q2))
      return (castPtr p)

newExprs :: (Show a) => (a -> IO (Ptr a)) -> [Expr a] -> [IO (Ptr (C_Expr a))]
newExprs newA ts = do
  case ts of
    [] -> []
    t:xs -> (newExpr newA t):(newExprs newA xs)

newExprArr :: (Show a) => (a -> IO (Ptr a)) -> (Expr a) -> IO (Ptr (C_Expr a))
newExprArr newA t = do
  case t of
    Bundle (t:ts) n -> do
      p <- mallocArray n
      q <- returnIO $ newExprs newA (t:ts)
      qq <- returnIO $ fmap (unsafePerformIO . peek . unsafePerformIO) q
      pokeArray0 (C_Num (negate 1)) p qq
      return (castPtr p)

-- Haskell functions
compareSymb :: Symbol -> Symbol -> OrdSymb
compareSymb (Symbol x) (Symbol y) =
  case compare x y of { EQ -> SEQ; LT -> SLT; GT -> SGT }

checkSymb :: Symbol -> OrdSymb -> Symbol -> Bool
checkSymb x ord y = compareSymb x y == ord

-- OrdSymb mapping
{#enum ord_symb as OrdSymb {} deriving (Show, Eq)#}
type C_OrdSymb = CInt

peekSymbol :: Ptr Symbol -> IO Symbol
peekSymbol ptr = Symbol <$> peekCString (castPtr ptr)

newSymbol :: Symbol -> IO (Ptr Symbol)
newSymbol (Symbol str) = castPtr <$> newCString str

freeSymbol :: Ptr Symbol -> IO ()
freeSymbol = free

-- C wrapper and exports
mylib_parse_expression :: Ptr Symbol -> IO (Ptr (C_Expr Symbol))
mylib_parse_expression sym = do
  let adt = parse_program (unsafePerformIO $ peekCString $ castPtr sym) --"a : 8 + 9\n b : 3 + a\nc : b * a"
  breakpoint (newExprArr newSymbol adt)

mylib_compare_symb :: Ptr Symbol -> Ptr Symbol -> IO C_OrdSymb
mylib_compare_symb px py = do
  x <- peekSymbol px
  y <- peekSymbol py
  return $ genFromEnum (compareSymb x y)

mylib_check_symb :: Ptr Symbol -> C_OrdSymb -> Ptr Symbol -> IO CInt
mylib_check_symb px ord py = do
  x <- peekSymbol px
  y <- peekSymbol py
  return $ genFromEnum (checkSymb x (genToEnum ord) y)

foreign export ccall mylib_parse_expression :: Ptr Symbol -> IO (Ptr (C_Expr Symbol))
foreign export ccall mylib_compare_symb :: Ptr Symbol -> Ptr Symbol -> IO C_OrdSymb
foreign export ccall mylib_check_symb :: Ptr Symbol -> C_OrdSymb -> Ptr Symbol -> IO CInt

num :: GenParser Char st (Expr a)
num = do
  n <- many1 digit
  spaces
  return (Num(read(n)))

ident :: GenParser Char st (Expr Symbol)
ident = do
  n <- many1 letter
  spaces
  return (Var(Symbol n))

funcident :: GenParser Char st (Symbol)
funcident = do
  n <- many1 letter
  spaces
  return (Symbol n)

keyword kw = do { try(string kw); spaces }

opTab = [[prefix "-" Negate],
         [binary "*" Mult, binary "/" Div],
         [binary "+" Add, binary "-" Sub],
         [binary ":" Assign]]
binary name fn = Infix (do { keyword name; return fn}) AssocLeft
prefix name fn = Prefix (do { keyword name; return fn})

paren = do
  keyword "("
  r <- expr
  keyword ")"
  return r

expr_comma = do
  e <- expr
  optional (keyword ",")
  return e

arguments = do
  exprs <- many1 expr_comma
  return $ exprs

call = do
  id <- funcident
  keyword "("
  r <- arguments
  keyword ")"
  return (Call id r (length r))

stmtsinner = do
  s1 <- many stmt
  return (Bundle s1 (length s1))

fndef = do
  keyword "def"
  id <- funcident
  keyword "("
  r <- arguments
  keyword ")"
  keyword "{"
  s <- stmtsinner
  keyword "}"
  return (FuncDef id r (length r) s)

factor = try call <|> num <|> paren <|> ident

expr = do
  e <- buildExpressionParser opTab factor
  spaces
  return e

stmt = do
  e <- expr
  return e

stmts = do
  s1 <- many (try fndef <|> stmt)
  return (Bundle s1 (length s1))

program = do
  e <- stmts
  eof
  return e

assumeRight (Right v) = v
assumeRight (Left v) = error (show v)
parse_program s = assumeRight $ parse program "Parse error" s
main :: IO ()
main = do
  --print (parse_program "(3 + 2 + 7) * 8")
  return ()
