module Language.PureScript.Pretty.CoreFn (prettyPrintCoreModule) where

import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Literals
import Language.PureScript.Names

import Text.PrettyPrint.HughesPJ
import Prelude hiding (mod) -- conflicts with 'mod' used as arg name
    
prettyPrintCoreModule :: Module a -> String
prettyPrintCoreModule = render . ppModule

ppModule :: Module a -> Doc
ppModule mod = vcat $
               [ hcat $ map text ["core module ", show (moduleName mod), " where"],
                 (text "exports ..."),
                 (text "imports ..."),
                 (text "foreign ...")
               ] ++ (map ppDecl (moduleDecls mod))

ppDecl :: Bind a -> Doc
ppDecl (NonRec name expr) = ppBind name expr
ppDecl (Rec binds)        = text "rec" $$ nest 4 (vcat . map (uncurry ppBind) $ binds)

ppBind :: Ident -> Expr a -> Doc
ppBind name expr = text (show name) <+> text "=" <+> ppExpr expr

ppLiteral :: (a -> Doc) -> Literal a -> Doc
ppLiteral _  (NumericLiteral num)   = text (either show show $ num)
ppLiteral pp (ObjectLiteral fields) = braces $ hsep (punctuate comma fieldsDocs)
    where fieldsDocs = map (\(label, val) -> text label <+> equals <+> pp val) fields
ppLiteral _ _ = text "<literal>"

ppExpr :: Expr a -> Doc
ppExpr (Literal _ lit)     = ppLiteral ppExpr lit
ppExpr (Let _ decls expr)  = text "let" $$ nest 4 (vcat (map ppDecl decls))
                             $$ text "in" $$ nest 4 (ppExpr expr)
ppExpr (Seq _ e1 e2)       = text "seq" $$ nest 4 (vcat [ppExpr e1,ppExpr e2])
ppExpr (Abs _ names expr)  = parens (hsep (punctuate comma (map (text . show) names)))
                             <+> text "->" <+> ppExpr expr
ppExpr (App _ e es)        = ppExpr e <+> parens (hsep (punctuate comma (map ppExpr es)))
ppExpr (Var _ ident)       = text (show ident)
ppExpr (Case _ exprs alts) = text "case" <+> hsep (map ppExpr exprs) <+> text "of"
                             $$ nest 4 (vcat (map ppAlt alts))
    where ppAlt :: CaseAlternative a -> Doc
          ppAlt (CaseAlternative binders (Right result)) = hsep (map ppBinder binders) <+> text "->" <+> ppExpr result
          ppAlt (CaseAlternative {})                     = text "<case alt with guards>"
ppExpr _ 				= text "<expression>"


ppBinder :: Binder a -> Doc
ppBinder (NullBinder _)        = text "_"
ppBinder (LiteralBinder _ lit) = ppLiteral ppBinder lit
ppBinder (VarBinder _ ident)   = text (show ident)
ppBinder (ConstructorBinder _ tyName consName binders)
    = text (show tyName) <> text "." <> text (show consName) <+>
      parens (hsep (punctuate comma (map ppBinder binders)))
ppBinder (NamedBinder _ ident binder) = text (show ident) <> text "@" <> ppBinder binder
