-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Values
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for values
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Values (
    prettyPrintValue,
    prettyPrintBinder,
    prettyPrintBinderAtom
) where

import Data.List (intercalate)

import Control.Arrow (second)

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Types (typeAsBox, typeAtomAsBox)
import Text.PrettyPrint.Boxes

-- | Render an aligned list of items separated with commas
list :: Char -> Char -> (a -> Box) -> [a] -> Box
list open close _ [] = text [open, close]
list open close f xs = vcat left (zipWith toLine [0 :: Int ..] xs ++ [ text [ close ] ])
  where
  toLine i a = text [ if i == 0 then open else ',', ' ' ] <> f a

ellipsis :: Box
ellipsis = text "..."

prettyPrintObject :: Int -> [(String, Maybe Expr)] -> Box
prettyPrintObject d = list '{' '}' prettyPrintObjectProperty
  where
  prettyPrintObjectProperty :: (String, Maybe Expr) -> Box
  prettyPrintObjectProperty (key, value) = text (prettyPrintObjectKey key ++ ": ") <> maybe (text "_") (prettyPrintValue (d - 1)) value

-- | Pretty-print an expression
prettyPrintValue :: Int -> Expr -> Box
prettyPrintValue d _ | d < 0 = text "..."
prettyPrintValue d (IfThenElse cond th el) =
  (text "if " <> prettyPrintValueAtom (d - 1) cond)
  // moveRight 2 (vcat left [ text "then " <> prettyPrintValueAtom (d - 1) th
                            , text "else " <> prettyPrintValueAtom (d - 1) el
                            ])
prettyPrintValue d (Accessor prop val) = prettyPrintValueAtom (d - 1) val <> text ("." ++ show prop)
prettyPrintValue d (ObjectUpdate o ps) = prettyPrintValueAtom (d - 1) o <> text " " <> list '{' '}' (\(key, val) -> text (key ++ " = ") <> prettyPrintValue (d - 1) val) ps
prettyPrintValue d (ObjectUpdater o ps) = maybe (text "_") (prettyPrintValueAtom (d - 1)) o <> text " " <> list '{' '}' (\(key, val) -> text (key ++ " = ") <> maybe (text "_") (prettyPrintValue (d - 1)) val) ps
prettyPrintValue d (App val arg) = prettyPrintValueAtom (d - 1) val `beforeWithSpace` prettyPrintValueAtom (d - 1) arg
prettyPrintValue d (Abs (Left arg) val) = text ('\\' : showIdent arg ++ " -> ") // moveRight 2 (prettyPrintValue (d - 1) val)
prettyPrintValue d (TypeClassDictionaryConstructorApp className ps) =
  text (runProperName (disqualify className) ++ " ") <> prettyPrintValueAtom (d - 1) ps
prettyPrintValue d (Case values binders) =
  (text "case " <> foldl1 beforeWithSpace (map (prettyPrintValueAtom (d - 1)) values) <> text " of") //
    moveRight 2 (vcat left (map (prettyPrintCaseAlternative (d - 1)) binders))
prettyPrintValue d (Let ds val) =
  text "let" //
    moveRight 2 (vcat left (map (prettyPrintDeclaration (d - 1)) ds)) //
    (text "in " <> prettyPrintValue (d - 1) val)
prettyPrintValue d (Do els) =
  text "do " <> vcat left (map (prettyPrintDoNotationElement (d - 1)) els)
prettyPrintValue _ (TypeClassDictionary (name, tys) _) = foldl1 beforeWithSpace $ text ("#dict " ++ runProperName (disqualify name)) : map typeAtomAsBox tys
prettyPrintValue _ (SuperClassDictionary name _) = text $ "#dict " ++ runProperName (disqualify name)
prettyPrintValue d (TypedValue _ val _) = prettyPrintValue d val
prettyPrintValue d (PositionedValue _ _ val) = prettyPrintValue d val
prettyPrintValue d expr = prettyPrintValueAtom d expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyPrintValueAtom :: Int -> Expr -> Box
prettyPrintValueAtom _ (NumericLiteral n) = text $ either show show n
prettyPrintValueAtom _ (StringLiteral s) = text $ show s
prettyPrintValueAtom _ (CharLiteral c) = text $ show c
prettyPrintValueAtom _ (BooleanLiteral True) = text "true"
prettyPrintValueAtom _ (BooleanLiteral False) = text "false"
prettyPrintValueAtom d (ArrayLiteral xs) = list '[' ']' (prettyPrintValue (d - 1)) xs
prettyPrintValueAtom d (ObjectLiteral ps) = prettyPrintObject (d - 1) $ second Just `map` ps
prettyPrintValueAtom d (ObjectConstructor ps) = prettyPrintObject (d - 1) ps
prettyPrintValueAtom _ (ObjectGetter prop) = text $ "_." ++ show prop
prettyPrintValueAtom _ (Constructor name) = text $ runProperName (disqualify name)
prettyPrintValueAtom _ (Var ident) = text $ showIdent (disqualify ident)
prettyPrintValueAtom d (OperatorSection op (Right val)) = ((text "(" <> prettyPrintValue (d - 1) op) `beforeWithSpace` prettyPrintValue (d - 1) val) `before` text ")"
prettyPrintValueAtom d (OperatorSection op (Left val)) = ((text "(" <> prettyPrintValue (d - 1) val) `beforeWithSpace` prettyPrintValue (d - 1) op) `before` text ")"
prettyPrintValueAtom d (TypedValue _ val _) = prettyPrintValueAtom d val
prettyPrintValueAtom d (PositionedValue _ _ val) = prettyPrintValueAtom d val
prettyPrintValueAtom d expr = (text "(" <> prettyPrintValue d expr) `before` text ")"

prettyPrintDeclaration :: Int -> Declaration -> Box
prettyPrintDeclaration d _ | d < 0 = ellipsis
prettyPrintDeclaration _ (TypeDeclaration ident ty) =
  text (showIdent ident ++ " :: ") <> typeAsBox ty
prettyPrintDeclaration d (ValueDeclaration ident _ [] (Right val)) =
  text (showIdent ident ++ " = ") <> prettyPrintValue (d - 1) val
prettyPrintDeclaration d (BindingGroupDeclaration ds) =
  vsep 1 left (map (prettyPrintDeclaration (d - 1) . toDecl) ds)
  where
  toDecl (nm, t, e) = ValueDeclaration nm t [] (Right e)
prettyPrintDeclaration d (PositionedDeclaration _ _ decl) = prettyPrintDeclaration d decl
prettyPrintDeclaration _ _ = internalError "Invalid argument to prettyPrintDeclaration"

prettyPrintCaseAlternative :: Int -> CaseAlternative -> Box
prettyPrintCaseAlternative d _ | d < 0 = ellipsis
prettyPrintCaseAlternative d (CaseAlternative binders result) =
  text (unwords (map prettyPrintBinderAtom binders)) <> prettyPrintResult result
  where
  prettyPrintResult :: Either [(Guard, Expr)] Expr -> Box
  prettyPrintResult (Left gs) =
    vcat left (map prettyPrintGuardedValue gs)
  prettyPrintResult (Right v) = text " -> " <> prettyPrintValue (d - 1) v

  prettyPrintGuardedValue :: (Guard, Expr) -> Box
  prettyPrintGuardedValue (grd, val) = foldl1 before
    [ text " | "
    , prettyPrintValue (d - 1) grd
    , text " -> "
    , prettyPrintValue (d - 1) val
    ]

prettyPrintDoNotationElement :: Int -> DoNotationElement -> Box
prettyPrintDoNotationElement d _ | d < 0 = ellipsis
prettyPrintDoNotationElement d (DoNotationValue val) =
  prettyPrintValue d val
prettyPrintDoNotationElement d (DoNotationBind binder val) =
  text (prettyPrintBinder binder ++ " <- ") <> prettyPrintValue d val
prettyPrintDoNotationElement d (DoNotationLet ds) =
  text "let" //
    moveRight 2 (vcat left (map (prettyPrintDeclaration (d - 1)) ds))
prettyPrintDoNotationElement d (PositionedDoNotationElement _ _ el) = prettyPrintDoNotationElement d el

prettyPrintBinderAtom :: Binder -> String
prettyPrintBinderAtom NullBinder = "_"
prettyPrintBinderAtom (StringBinder str) = show str
prettyPrintBinderAtom (CharBinder c) = show c
prettyPrintBinderAtom (NumberBinder num) = either show show num
prettyPrintBinderAtom (BooleanBinder True) = "true"
prettyPrintBinderAtom (BooleanBinder False) = "false"
prettyPrintBinderAtom (VarBinder ident) = showIdent ident
prettyPrintBinderAtom (ConstructorBinder ctor []) = runProperName (disqualify ctor)
prettyPrintBinderAtom (ObjectBinder bs) =
  "{ "
  ++ intercalate ", " (map prettyPrintObjectPropertyBinder bs)
  ++ " }"
  where
  prettyPrintObjectPropertyBinder :: (String, Binder) -> String
  prettyPrintObjectPropertyBinder (key, binder) = prettyPrintObjectKey key ++ ": " ++ prettyPrintBinder binder
prettyPrintBinderAtom (ArrayBinder bs) =
  "[ "
  ++ intercalate ", " (map prettyPrintBinder bs)
  ++ " ]"
prettyPrintBinderAtom (NamedBinder ident binder) = showIdent ident ++ "@" ++ prettyPrintBinder binder
prettyPrintBinderAtom (PositionedBinder _ _ binder) = prettyPrintBinderAtom binder
prettyPrintBinderAtom b = parens (prettyPrintBinder b)

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder -> String
prettyPrintBinder (ConstructorBinder ctor []) = runProperName (disqualify ctor)
prettyPrintBinder (ConstructorBinder ctor args) = runProperName (disqualify ctor) ++ " " ++ unwords (map prettyPrintBinderAtom args)
prettyPrintBinder (PositionedBinder _ _ binder) = prettyPrintBinder binder
prettyPrintBinder (TypedBinder _ binder) = prettyPrintBinder binder
prettyPrintBinder b = prettyPrintBinderAtom b
