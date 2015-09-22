{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Sugar.Variables (desugarVarsInModule) where

import Language.PureScript.AST
import Language.PureScript.Environment (NameKind(..))
import Language.PureScript.Errors
import Language.PureScript.Names
import qualified Language.PureScript.Constants as C


#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class

desugarVarsInModule :: (Applicative m, MonadSupply m, MonadError MultipleErrors m) => Module -> m Module
desugarVarsInModule (Module ss coms mn ds exts) = return (Module ss coms mn (map desugarDecl ds) exts)
    where
      (desugarDecl, _, _) = everywhereOnValues handleDecl handleExpr id

      handleDecl :: Declaration -> Declaration
      handleDecl (VariableDeclaration ident (TypedValue check expr typ))
          = ValueDeclaration ident Public [] (Right (TypedValue check (newVarCall expr) typ))
      handleDecl (VariableDeclaration ident expr)
          = ValueDeclaration ident Public [] (Right (newVarCall expr))
      handleDecl decl = decl

      handleExpr :: Expr -> Expr
      handleExpr (Assign ident expr) = writeVarCall ident expr
      handleExpr (Read ident) = readVarCall ident
      handleExpr expr = expr
                        
      newVarCall :: Expr -> Expr
      newVarCall expr = (Var $ primValue "newVar") `App` expr
      writeVarCall :: Qualified Ident -> Expr -> Expr
      writeVarCall ident expr = (Var $ primValue "writeVar") `App` (Var ident) `App` expr
      readVarCall :: Qualified Ident -> Expr
      readVarCall ident = (Var $ primValue "readVar") `App` (Var ident)

      primValue = Qualified (Just (ModuleName [ProperName C.prim])) . Ident
