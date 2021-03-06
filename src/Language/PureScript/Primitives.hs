module Language.PureScript.Primitives  where

import qualified Language.PureScript.Constants as C
import Language.PureScript.AST.Declarations (Expr(..))
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Types

import qualified Data.Map as M

-- |
-- Type constructor for functions
--
tyFunction :: Type
tyFunction = primType "Function"

-- |
-- Type constructor for strings
--
tyString :: Type
tyString = primType "String"

-- |
-- Type constructor for strings
--
tyChar :: Type
tyChar = primType "Char"

-- |
-- Type constructor for numbers
--
tyNumber :: Type
tyNumber = primType "Number"

-- |
-- Type constructor for integers
--
tyInt :: Type
tyInt = primType "Int"

-- |
-- Type constructor for booleans
--
tyBoolean :: Type
tyBoolean = primType "Boolean"

-- |
-- Type constructor for arrays
--
tyArray :: Type
tyArray = primType "Array"

-- |
-- Type constructor for objects
--
tyObject :: Type
tyObject = primType "Object"

-- |
-- Check whether a type is an object
--
isObject :: Type -> Bool
isObject = isTypeOrApplied tyObject

-- |
-- Check whether a type is a function
--
isFunction :: Type -> Bool
isFunction = isTypeOrApplied tyFunction

isTypeOrApplied :: Type -> Type -> Bool
isTypeOrApplied t1 (TypeApp t2 _) = t1 == t2
isTypeOrApplied t1 t2 = t1 == t2

-- |
-- Smart constructor for function types
--
function :: Type -> Type -> Type
function t1 = TypeApp (TypeApp tyFunction t1)


    
-- |
-- Construct a ProperName in the Prim module
--
primName :: String -> Qualified ProperName
primName = Qualified (Just $ ModuleName [ProperName C.prim]) . ProperName

-- |
-- Construct a type in the Prim module
--
primType :: String -> Type
primType = TypeConstructor . primName

-- |
-- The primitive types in the external javascript environment with their associated kinds.
--
primTypes :: M.Map (Qualified ProperName) (Kind, TypeKind)
primTypes = M.fromList [ (primName "Function" , (FunKind Star (FunKind Star Star), ExternData))
                       , (primName "Array"    , (FunKind Star Star, ExternData))
                       , (primName "Object"   , (FunKind (Row Star) Star, ExternData))
                       , (primName "String"   , (Star, ExternData))
                       , (primName "Char"     , (Star, ExternData))
                       , (primName "Number"   , (Star, ExternData))
                       , (primName "Int"      , (Star, ExternData))
                       , (primName "Boolean"  , (Star, ExternData)) ]


newVarSymbol :: String
newVarSymbol = "$newVar"

writeVarSymbol :: String
writeVarSymbol = "$writeVar"

primValues :: M.Map (ModuleName, Ident) (Type, NameKind, NameVisibility)
primValues = M.fromList [
                   ((primValueName newVarSymbol),   (newVarType, Public, Defined)),
                   ((primValueName writeVarSymbol), (writeVarType, Public, Defined))
                  ]
    where
      primValueName :: String -> (ModuleName, Ident)
      primValueName name = (ModuleName [ProperName C.prim], Ident name)
          
      newVarType, writeVarType :: Type
      newVarType = ForAll "a" ((TypeVar "a") `function` (TypeVar "a")) Nothing
      writeVarType = ForAll "a" ((TypeVar "a") `function` ((TypeVar "a") `function` (TypeVar "a"))) Nothing

primValue :: String -> Qualified Ident
primValue = Qualified (Just (ModuleName [ProperName C.prim])) . Ident

newVarCall :: Expr -> Expr
newVarCall expr = (Var $ primValue newVarSymbol) `App` expr

writeVarCall :: Qualified Ident -> Expr -> Expr
writeVarCall ident expr = (Var $ primValue writeVarSymbol) `App` (Var ident) `App` expr


-- |
-- The initial environment with no values and only the default javascript types defined
--
initEnvironment :: Environment
initEnvironment = Environment {
                    names = primValues,
                    types = primTypes,
                    dataConstructors = M.empty,
                    typeSynonyms = M.empty,
                    typeClassDictionaries = M.empty,
                    typeClasses = M.empty
                  }


