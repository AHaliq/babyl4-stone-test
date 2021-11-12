{-# LANGUAGE MultiParamTypeClasses #-}

module Utils.L4D3Json (D3Json (d3json)) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import L4.Annotation
import L4.KeyValueMap
import L4.Syntax

emptyJsonString :: String
emptyJsonString = "null"

class D3Json a where
  d3json'' :: D3Json t => String -> t -> a -> String -- inherit with name
  d3json'' n t x = d3node n t [d3json' t x]
  d3json' :: D3Json t => t -> a -> String -- inherit t from parent
  d3json' _ x = d3json x
  d3json :: a -> String -- data structure contains t
  d3json x = d3json' () x

instance D3Json t => D3Json (Program t) where
  d3json Program {annotOfProgram = t, elementsOfProgram = xs} = d3node "Program" t $ map d3json xs

-- Top Level --------------------------------------------------------

instance D3Json t => D3Json (TopLevelElement t) where
  d3json (MappingTLE x@Mapping {annotOfMapping = t}) = d3json'' "MappingTLE" t x
  d3json (ClassDeclTLE x@ClassDecl {annotOfClassDecl = t}) = d3json'' "ClassDeclTLE" t x
  d3json (VarDeclTLE x@VarDecl {annotOfVarDecl = t}) = d3json'' "VarDeclTLE" t x
  d3json (RuleTLE x@Rule {annotOfRule = t}) = d3json'' "RuleTLE" t x
  d3json (AssertionTLE x@Assertion {annotOfAssertion = t}) = d3json'' "AssertionTLE" t x
  d3json (AutomatonTLE x@TA {annotOfTA = t}) = d3json'' "AutomatonTLE" t x

instance D3Json t => D3Json (TA t) where
  d3json TA {annotOfTA = t, nameOfTA = n, locsOfTA = ls, channelsOfTA = ns, clocksOfTA = cs, transitionsOfTA = ts, initialLocOfTA = il, invarsOfTA = is, labellingOfTA = lls} =
    d3node
      "TA"
      t
      [ d3one t "nameOfTA" n,
        d3json'' "locsOfTA" t ls,
        d3json'' "channelsOfTA" t ns,
        d3json'' "clocksOfTA" t cs,
        d3json'' "transitionsOfTA" t ts,
        d3json'' "initialLocOfTA" t il,
        d3json'' "invarsOfTA" t is,
        d3json'' "labellingOfTA" t lls
      ]

instance D3Json t => D3Json (Assertion t) where
  d3json Assertion {annotOfAssertion = t, nameOfAssertion = n, instrOfAssertion = m, exprOfAssertion = e} =
    d3node
      "Assertion"
      t
      [ d3one t "annotOfAssertion" $ fromMaybe emptyJsonString n,
        d3jsonKVMap "instrOfAssertion" t m,
        d3json'' "exprOfAssertion" t e
      ]

instance D3Json t => D3Json (Rule t) where
  d3json Rule {annotOfRule = t, nameOfRule = n, instrOfRule = m, varDeclsOfRule = vs, precondOfRule = pre, postcondOfRule = post} =
    d3node
      "Rule"
      t
      [ d3node "nameOfRule" t [maybe emptyJsonString (\x -> d3node x t []) n],
        d3jsonKVMap "instrOfRule" t m,
        d3node "varDeclsOfRule" t $ map d3json vs,
        d3json'' "precondOfRule" t pre,
        d3json'' "postcondOfRule" t post
      ]

instance D3Json t => D3Json (VarDecl t) where
  d3json VarDecl {annotOfVarDecl = t, nameOfVarDecl = n, tpOfVarDecl = y} =
    d3node
      "VarDecl"
      t
      [ d3one t "nameOfVarDecl" n,
        d3json'' "tpOfVarDecl" t y
      ]

instance D3Json t => D3Json (ClassDecl t) where
  d3json ClassDecl {annotOfClassDecl = t, nameOfClassDecl = n, defOfClassDecl = def} =
    d3node
      "ClassDecl"
      t
      [ d3json'' "nameOfClassDecl" t n,
        d3json'' "defOfClassDecl" t def
      ]

instance D3Json t => D3Json (Mapping t) where
  d3json Mapping {annotOfMapping = t, fromMapping = f, toMapping = g} =
    d3node
      "Mapping"
      t
      [ d3one t "fromMapping" f,
        d3json'' "toMapping" t g
      ]

-- Expr -------------------------------------------------------------

instance D3Json t => D3Json (Expr t) where
  d3json ValE {annotOfExpr = t, valOfExprValE = v} = d3node "ValE" t [d3json' t v]
  d3json VarE {annotOfExpr = t, varOfExprVarE = e} = d3node "VarE" t [d3json' t e]
  d3json UnaOpE {annotOfExpr = t, unaOpOfExprUnaOpE = op, subEOfExprUnaOpE = e} =
    d3node
      "UnaOpE"
      t
      [ d3json'' "unaOpOfExprUnaOpE" t op,
        d3json'' "subEOfExprUnaOpE" t e
      ]
  d3json BinOpE {annotOfExpr = t, binOpOfExprBinOpE = op, subE1OfExprBinOpE = e1, subE2OfExprBinOpE = e2} =
    d3node
      "BinOpE"
      t
      [ d3json'' "binOpOfExprBinOpE" t op,
        d3json'' "subE1OfExprBinOpE" t e1,
        d3json'' "subE2OfExprBinOpE" t e2
      ]
  d3json IfThenElseE {annotOfExpr = t, condOfExprIf = e1, thenofExprIf = e2, elseOfExprIf = e3} =
    d3node
      "IfThenElseE"
      t
      [ d3json'' "condOfExprIf" t e1,
        d3json'' "thenofExprIf" t e2,
        d3json'' "elseOfExprIf" t e3
      ]
  d3json AppE {annotOfExpr = t, funOfExprAppE = e1, argOfExprAppE = e2} =
    d3node
      "AppE"
      t
      [ d3json'' "funOfExprAppE" t e1,
        d3json'' "argOfExprAppE" t e2
      ]
  d3json FunE {annotOfExpr = t, varOfFunE = v, bodyOfFunE = b} =
    d3node
      "FunE"
      t
      [ d3json'' "varOfFunE" t v,
        d3json'' "bodyOfFunE" t b
      ]
  d3json QuantifE {annotOfExpr = t, quantifOfExprQ = q, varOfExprQ = v, bodyOfExprQ = b} =
    d3node
      "QuantifE"
      t
      [ d3json'' "quantifOfExprQ" t q,
        d3json'' "varOfExprQ" t v,
        d3json'' "bodyOfExprQ" t b
      ]
  d3json FldAccE {annotOfExpr = t, subEOfExprFldAccE = e, fieldNameOfExprFldAccE = f} =
    d3node
      "FldAccE"
      t
      [ d3json'' "subOfExprFldAccE" t e,
        d3json'' "fieldNameOfExprFldAccE" t f
      ]
  d3json TupleE {annotOfExpr = t, componentsOfExprTupleE = xs} = d3json'' "TupleE" t xs
  d3json CastE {annotOfExpr = t, tpOfExprCastE = tp, subEOfExprCastE = e} =
    d3node
      "CastE"
      t
      [ d3json'' "tpOfExprCastE" t tp,
        d3json'' "subEOfExprCastE" t e
      ]
  d3json ListE {annotOfExpr = t, listOpOfExprListE = op, componentsOfExprListE = xs} =
    d3node
      "ListE"
      t
      [ d3json'' "listOpOfExprListE" t op,
        d3json'' "componentsOfExprListE" t xs
      ]

-- Auxilliary -------------------------------------------------------

instance D3Json t => D3Json (Cmd t) where
  d3json (Skip t) = d3one t "Cmd" "Skip"
  d3json (VAssign t v e) = d3node "Cmd" t [d3json'' "Var" () v, d3json'' "Expr" () e]
  d3json (FAssign t e1 f e2) = d3node "Cmd" t [d3json'' "e1" () e1, d3json' t f, d3json'' "e2" () e2]

instance D3Json Sync where
  d3json' t x = d3one t "Sync" (show x)

instance D3Json Action where
  d3json' t Internal = d3xs t ["Action", "Internal"]
  d3json' t (Act c s) = d3node "Action" t [d3node "Act" t [d3json c, d3json' t s]]

instance D3Json t => D3Json (TransitionAction t) where
  d3json' t (TransitionAction a cs c) =
    d3node
      "TransitionAction"
      t
      [ d3json' t a,
        d3json'' "[Clock]" t cs,
        d3json c
      ]

instance D3Json BComparOp where
  d3json' t x = d3one t "BComparOp" (show x)

instance D3Json ClConstr where
  d3json' t (ClConstr c op i) =
    d3node
      "ClConstr"
      t
      [ d3json' t c,
        d3json' t op,
        d3one t "Integer" (show i)
      ]

instance D3Json t => D3Json (TransitionGuard t) where
  d3json' t (TransitionGuard xs e) =
    d3node
      "TransitionGuard"
      t
      [ d3json'' "[ClConstr]" t xs,
        d3json e
      ]

instance D3Json t => D3Json (Transition t) where
  d3json' t Transition {sourceOfTransition = s, guardOfTransition = g, actionOfTransition = a, targetOfTransition = tg} =
    d3node
      "Transition"
      t
      [ d3json'' "sourceOfTransition" t s,
        d3json'' "guardOfTransition" t g,
        d3json'' "actionOfTransition" t a,
        d3json'' "targetOfTransition" t tg
      ]

instance D3Json Clock where
  d3json' t Clock {nameOfClock = n} = d3xs t ["Clock", "nameOfClock", n]

instance D3Json Loc where
  d3json' t (Loc s) = d3one t "Loc" s

instance D3Json ListOp where
  d3json' t x = d3node (show x) t []

instance D3Json Quantif where
  d3json' t x = d3node (show x) t []

instance D3Json BinOp where
  d3json' t (BArith op) = d3one t "BArith" (show op)
  d3json' t (BCompar op) = d3one t "BCompar" (show op)
  d3json' t (BBool op) = d3one t "BBool" (show op)

instance D3Json UnaOp where
  d3json' t (UArith x) = d3one t "UArith" (show x)
  d3json' t (UBool x) = d3one t "UBool" (show x)
  d3json' t (UTemporal op) = d3node "UTemporal" t [d3json' t op]

instance D3Json UTemporalOp where
  d3json' t x = d3one t "UTemporalOp" (show x)

instance D3Json t => D3Json (Var t) where
  d3json' t GlobalVar {nameOfVar = n} = d3xsb t ["GlobalVar", "nameOfVar"] [d3json n]
  d3json' t LocalVar {nameOfVar = n, indexOfVar = i} =
    d3node
      "LocalVar"
      t
      [ d3json'' "nameOfVar" t n,
        d3one t "indexOfVar" (show i)
      ]

instance D3Json t => D3Json (QVarName t) where
  d3json QVarName {annotOfQVarName = t, nameOfQVarName = n} = d3one t "QVarName" n

instance D3Json Val where
  d3json' t (BoolV b) = d3one t "BoolV" (show b)
  d3json' t (IntV i) = d3one t "IntV" (show i)
  d3json' t (FloatV f) = d3one t "FloatV" (show f)
  d3json' t (StringV s) = d3one t "StringV" s
  d3json' t ErrV = d3node "ErrV" t []

d3jsonKVMap :: D3Json t => String -> t -> KVMap -> String
d3jsonKVMap name t xs = d3node name t $ zipWith aux xs [0 .. length xs]
  where
    aux (k, v) i =
      d3node
        ("ele-" ++ show i)
        t
        [ d3one t "KeyKVM" k,
          d3json' t v
        ]

instance D3Json ValueKVM where
  d3json' t (IdVM s) = d3one t "IdVM" s
  d3json' t (BoolVM b) = d3one t "BoolVM" $ show b
  d3json' t (IntVM i) = d3one t "IntVM" $ show i
  d3json' t (MapVM m) = d3jsonKVMap "MapVM" t m

instance D3Json t => D3Json (ClassDef t) where
  d3json' t ClassDef {supersOfClassDef = xs, fieldsOfClassDef = ys} =
    d3node
      "ClassDef"
      t
      [ d3json'' "supersOfClassDef" t xs,
        d3json'' "fieldOfClassDef" t ys
      ]

instance D3Json t => D3Json (FieldDecl t) where
  d3json FieldDecl {annotOfFieldDecl = t, nameOfFieldDecl = n, tpOfFieldDecl = tp} =
    d3node
      "FieldDecl"
      t
      [ d3json'' "nameOfFieldDecl" t n,
        d3json'' "tpOfFieldDecl" t tp
      ]

instance D3Json t => D3Json (Tp t) where
  d3json' _ ClassT {annotOfTp = t, classNameOfTp = n} =
    d3node
      "ClassT"
      t
      [d3json'' "classNameOfTp" t n]
  d3json' _ FunT {annotOfTp = t, paramTp = p, resultTp = r} =
    d3node
      "FunT"
      t
      [ d3json'' "paramTp" t p,
        d3json'' "resultTp" t r
      ]
  d3json' _ TupleT {annotOfTp = t, componentsOfTpTupleT = xs} =
    d3node
      "TupleT"
      t
      [d3json'' "componentsOfTpTupleT" t xs]
  d3json' t ErrT = d3node "ErrT" t []
  d3json' t OkT = d3node "OkT" t []
  d3json' t KindT = d3node "KindT" t []

instance D3Json FieldName where
  d3json' t FldNm {stringOfFieldName = s} = d3one t "FldNm" s

instance D3Json ClassName where
  d3json' t ClsNm {stringOfClassName = s} = d3one t "ClsNm" s

instance D3Json Description where
  d3json' t Descr {predOfDescription = x, argsOfDescription = xs} =
    d3node
      "Description"
      t
      [ d3one t "predOfDescription" x,
        if null xs then "" else d3node "argsOfDescription" t $ map (\y -> d3node y t []) xs
      ]

-- general ----------------------------------------------------------

instance D3Json a => D3Json [a] where
  d3json'' n t x = d3node n t $ map (d3json' t) x

instance (D3Json a, D3Json b) => D3Json (a, b) where
  d3json'' n t (a, b) = d3node n t [d3json' t a, d3json' t b]

-- SRng -------------------------------------------------------------

instance D3Json SRng where
  d3json (RealSRng x) = jsonkv' "SRng" $ d3json x
  d3json (DummySRng _) = jsonkv' "SRng" emptyJsonString

instance D3Json RealSRng where
  d3json SRng {start = s, end = e} =
    obj $
      concat
        [ jsonkv' "start" $ d3json s,
          ",",
          jsonkv' "end" $ d3json e
        ]

instance D3Json Pos where
  d3json Pos {line = l, col = c} =
    obj $
      concat
        [ jsonkv "line" l,
          ",",
          jsonkv "col" c
        ]

instance D3Json () where
  d3json () = emptyJsonString

-- util -------------------------------------------------------------

d3xs :: D3Json t => t -> [String] -> String
d3xs t xs = d3xsb t xs []

d3xsb :: D3Json t => t -> [String] -> [String] -> String
d3xsb t xs b = let r = foldr (\x a -> [d3node x t a]) b xs in if null r then emptyJsonString else head r

d3one :: D3Json t => t -> String -> String -> String
d3one t x y = d3node x t [d3node y t []]

d3node :: D3Json t => String -> t -> [String] -> String
d3node name t children =
  obj $
    concat $
      [ jsonkv' "name" $ surround name,
        ",",
        d3json t
      ]
        ++ if null children
          then []
          else
            [ ",",
              jsonkv' "children" $ concat ["[", intercalate "," children, "]"]
            ]

jsonkv :: Show a => String -> a -> String
jsonkv k v = jsonkv' k $ surround . show $ v

jsonkv' :: String -> String -> String
jsonkv' k v = concat [surround k, ":", v]

obj :: String -> String
obj s = concat ["{", s, "}"]

surround :: String -> String
surround s = concat ["\"", s, "\""]