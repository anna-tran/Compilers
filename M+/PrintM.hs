{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintM where

-- pretty-printer generated by the BNF converter

import AbsM
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print TokenID where
  prt _ (TokenID i) = doc (showString ( i))


instance Print TokenReal where
  prt _ (TokenReal i) = doc (showString ( i))


instance Print TokenInt where
  prt _ (TokenInt i) = doc (showString ( i))



instance Print Prog where
  prt i e = case e of
    Prog block -> prPrec i 0 (concatD [prt 0 block])

instance Print Block where
  prt i e = case e of
    Block decls progbody -> prPrec i 0 (concatD [prt 0 decls, prt 0 progbody])

instance Print Decls where
  prt i e = case e of
    DeclDecls decl decls -> prPrec i 0 (concatD [prt 0 decl, doc (showString ";"), prt 0 decls])
    NoDecls -> prPrec i 0 (concatD [])

instance Print Decl where
  prt i e = case e of
    VarDecl vardecl -> prPrec i 0 (concatD [prt 0 vardecl])
    FunDecl fundecl -> prPrec i 0 (concatD [prt 0 fundecl])

instance Print VarDecl where
  prt i e = case e of
    MVar tokenid arrdim type_ -> prPrec i 0 (concatD [doc (showString "var"), prt 0 tokenid, prt 0 arrdim, doc (showString ":"), prt 0 type_])

instance Print Type where
  prt i e = case e of
    MInt -> prPrec i 0 (concatD [doc (showString "int")])
    MReal -> prPrec i 0 (concatD [doc (showString "real")])
    MBool -> prPrec i 0 (concatD [doc (showString "bool")])

instance Print ArrDim where
  prt i e = case e of
    ExprArrDim expr arrdim -> prPrec i 0 (concatD [doc (showString "["), prt 0 expr, doc (showString "]"), prt 0 arrdim])
    NoArrDim -> prPrec i 0 (concatD [])

instance Print FunDecl where
  prt i e = case e of
    MFun tokenid paramlist type_ funblock -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 tokenid, prt 0 paramlist, doc (showString ":"), prt 0 type_, doc (showString "{"), prt 0 funblock, doc (showString "}")])

instance Print FunBlock where
  prt i e = case e of
    DeclFunBody decls funbody -> prPrec i 0 (concatD [prt 0 decls, prt 0 funbody])

instance Print ParamList where
  prt i e = case e of
    Params params -> prPrec i 0 (concatD [doc (showString "("), prt 0 params, doc (showString ")")])

instance Print Params where
  prt i e = case e of
    DeclMoreParams basicdecl moreparams -> prPrec i 0 (concatD [prt 0 basicdecl, prt 0 moreparams])
    NoParams -> prPrec i 0 (concatD [])

instance Print MoreParams where
  prt i e = case e of
    CommaDeclMoreParams basicdecl moreparams -> prPrec i 0 (concatD [doc (showString ","), prt 0 basicdecl, prt 0 moreparams])
    NoMoreParams -> prPrec i 0 (concatD [])

instance Print BasicDecl where
  prt i e = case e of
    BasicDecl tokenid basicarraydim type_ -> prPrec i 0 (concatD [prt 0 tokenid, prt 0 basicarraydim, doc (showString ":"), prt 0 type_])

instance Print BasicArrayDim where
  prt i e = case e of
    BasicArrDim solidparen basicarraydim -> prPrec i 0 (concatD [prt 0 solidparen, prt 0 basicarraydim])
    NoBasicArrDim -> prPrec i 0 (concatD [])

instance Print SolidParen where
  prt i e = case e of
    SolidParen -> prPrec i 0 (concatD [doc (showString "["), doc (showString "]")])

instance Print ProgBody where
  prt i e = case e of
    ProgStmtsBody progstmts -> prPrec i 0 (concatD [doc (showString "begin"), prt 0 progstmts, doc (showString "end")])

instance Print FunBody where
  prt i e = case e of
    FunBody progstmts expr -> prPrec i 0 (concatD [doc (showString "begin"), prt 0 progstmts, doc (showString "return"), prt 0 expr, doc (showString ";"), doc (showString "end")])

instance Print ProgStmts where
  prt i e = case e of
    ProgStmts stmt progstmts -> prPrec i 0 (concatD [prt 0 stmt, doc (showString ";"), prt 0 progstmts])
    NoProgStmts -> prPrec i 0 (concatD [])

instance Print Stmt where
  prt i e = case e of
    MCond expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr, doc (showString "then"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    MWhile expr stmt -> prPrec i 0 (concatD [doc (showString "while"), prt 0 expr, doc (showString "do"), prt 0 stmt])
    MRead identifier -> prPrec i 0 (concatD [doc (showString "read"), prt 0 identifier])
    MAss identifier expr -> prPrec i 0 (concatD [prt 0 identifier, doc (showString ":="), prt 0 expr])
    MPrint expr -> prPrec i 0 (concatD [doc (showString "print"), prt 0 expr])
    MBlock block -> prPrec i 0 (concatD [doc (showString "{"), prt 0 block, doc (showString "}")])

instance Print Identifier where
  prt i e = case e of
    MId tokenid arrdim -> prPrec i 0 (concatD [prt 0 tokenid, prt 0 arrdim])

instance Print Expr where
  prt i e = case e of
    ExprBintTerm expr bintterm -> prPrec i 0 (concatD [prt 0 expr, doc (showString "||"), prt 0 bintterm])
    BintTerm bintterm -> prPrec i 0 (concatD [prt 0 bintterm])

instance Print BintTerm where
  prt i e = case e of
    BintTermBintFactor bintterm bintfact -> prPrec i 0 (concatD [prt 0 bintterm, doc (showString "&&"), prt 0 bintfact])
    BintFactor bintfact -> prPrec i 0 (concatD [prt 0 bintfact])

instance Print BintFact where
  prt i e = case e of
    NotBintFactor bintfact -> prPrec i 0 (concatD [doc (showString "not"), prt 0 bintfact])
    IntECompareIntE intexpr1 compareop intexpr2 -> prPrec i 0 (concatD [prt 0 intexpr1, prt 0 compareop, prt 0 intexpr2])
    IntE intexpr -> prPrec i 0 (concatD [prt 0 intexpr])

instance Print CompareOp where
  prt i e = case e of
    MEq -> prPrec i 0 (concatD [doc (showString "=")])
    MLt -> prPrec i 0 (concatD [doc (showString "<")])
    MGt -> prPrec i 0 (concatD [doc (showString ">")])
    MLe -> prPrec i 0 (concatD [doc (showString "<=")])
    MGe -> prPrec i 0 (concatD [doc (showString ">=")])

instance Print IntExpr where
  prt i e = case e of
    IntEAddIntT intexpr addop intterm -> prPrec i 0 (concatD [prt 0 intexpr, prt 0 addop, prt 0 intterm])
    IntT intterm -> prPrec i 0 (concatD [prt 0 intterm])

instance Print AddOp where
  prt i e = case e of
    MAdd -> prPrec i 0 (concatD [doc (showString "+")])
    MSub -> prPrec i 0 (concatD [doc (showString "-")])

instance Print IntTerm where
  prt i e = case e of
    IntTMulIntF intterm mulop intfactor -> prPrec i 0 (concatD [prt 0 intterm, prt 0 mulop, prt 0 intfactor])
    IntF intfactor -> prPrec i 0 (concatD [prt 0 intfactor])

instance Print MulOp where
  prt i e = case e of
    MMul -> prPrec i 0 (concatD [doc (showString "*")])
    MDiv -> prPrec i 0 (concatD [doc (showString "/")])

instance Print IntFactor where
  prt i e = case e of
    EnclosedExpr expr -> prPrec i 0 (concatD [doc (showString "("), prt 0 expr, doc (showString ")")])
    MSize tokenid basicarraydim -> prPrec i 0 (concatD [doc (showString "size"), doc (showString "("), prt 0 tokenid, prt 0 basicarraydim, doc (showString ")")])
    MFloat expr -> prPrec i 0 (concatD [doc (showString "float"), doc (showString "("), prt 0 expr, doc (showString ")")])
    MFloor expr -> prPrec i 0 (concatD [doc (showString "floor"), doc (showString "("), prt 0 expr, doc (showString ")")])
    MCeil expr -> prPrec i 0 (concatD [doc (showString "ceil"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Id_modlist tokenid modifierlist -> prPrec i 0 (concatD [prt 0 tokenid, prt 0 modifierlist])
    MIval tokenint -> prPrec i 0 (concatD [prt 0 tokenint])
    MRval tokenreal -> prPrec i 0 (concatD [prt 0 tokenreal])
    MBval mbool -> prPrec i 0 (concatD [prt 0 mbool])
    MNval intfactor -> prPrec i 0 (concatD [doc (showString "-"), prt 0 intfactor])

instance Print Mbool where
  prt i e = case e of
    NoTrue -> prPrec i 0 (concatD [doc (showString "true")])
    NoFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print ModifierList where
  prt i e = case e of
    EnclosedArgs args -> prPrec i 0 (concatD [doc (showString "("), prt 0 args, doc (showString ")")])
    ArrDim arrdim -> prPrec i 0 (concatD [prt 0 arrdim])

instance Print Args where
  prt i e = case e of
    MoreArgs expr moreargs -> prPrec i 0 (concatD [prt 0 expr, prt 0 moreargs])
    NoArgs -> prPrec i 0 (concatD [])

instance Print MoreArgs where
  prt i e = case e of
    ExprMoreArgs expr moreargs -> prPrec i 0 (concatD [doc (showString ","), prt 0 expr, prt 0 moreargs])
    NoMoreArgs -> prPrec i 0 (concatD [])


