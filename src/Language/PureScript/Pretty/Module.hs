-- |
-- Pretty printing modules
--
---------------------------------------------------------------------------
module Language.PureScript.Pretty.Module (prettyPrintModule) where

import Language.PureScript.AST.Declarations
import Language.PureScript.Pretty.Values (prettyPrintDeclaration)
import Text.PrettyPrint.Boxes


prettyPrintModule :: Module -> Box
prettyPrintModule (Module _ _ name decls _) =
    text ("module " ++ show name ++ " where") //
    vcat left (map (prettyPrintDeclaration 10) decls)
