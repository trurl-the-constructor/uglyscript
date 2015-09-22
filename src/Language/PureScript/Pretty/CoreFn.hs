module Language.PureScript.Pretty.CoreFn ( prettyPrintModule ) where

import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Literals
import Language.PureScript.Names

import Text.PrettyPrint.HughesPJ
import Prelude hiding (mod) -- conflicts with 'mod' used as arg name
    
prettyPrintModule :: Module a -> String
prettyPrintModule = render . ppModule

ppModule :: Module a -> Doc
ppModule mod = vcat $
               [ hcat $ map text ["core module", show (moduleName mod), "where"],
                 (text "exports ..."),
                 (text "imports ..."),
                 (text "foreign ...")
               ] ++ (map ppDecl (moduleDecls mod))

ppDecl :: Bind a -> Doc
ppDecl (NonRec name expr) = text "let" $$ nest 4 (ppBind name expr)
ppDecl (Rec binds)        = text "let rec" $$ nest 4 (vcat . map (uncurry ppBind) $ binds)

ppBind :: Ident -> Expr a -> Doc
ppBind name expr = text (show name) <+> text "=" <+> ppExpr expr

ppExpr :: Expr a -> Doc
ppExpr (Literal _ (NumericLiteral num)) = text (either show show $ num)
ppExpr (Literal _ _) 			= text "<literal>"
ppExpr (Let _ decls expr) 		= vcat (map ppDecl decls) $$ text "in" $$ nest 4 (ppExpr expr)
ppExpr (Abs _ name expr)  		= hcat (map text ["\\", show name, "->"]) <+> ppExpr expr
ppExpr (App _ e1 e2) 			= ppExpr e1 <+> parens (ppExpr e2)
ppExpr (Var _ ident)                    = text (show ident)
ppExpr _ 				= text "<expression>"

