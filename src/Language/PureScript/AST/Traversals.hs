-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.AST.Traversals
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | AST traversal helpers
--
-----------------------------------------------------------------------------

module Language.PureScript.AST.Traversals where

import Prelude ()
import Prelude.Compat

import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import Control.Monad
import Control.Arrow ((***), (+++), second)

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.Types
import Language.PureScript.Traversals
import Language.PureScript.Names

everywhereOnValues :: (Declaration -> Declaration) ->
                      (Expr -> Expr) ->
                      (Binder -> Binder) ->
                      (Declaration -> Declaration, Expr -> Expr, Binder -> Binder)
everywhereOnValues f g h = (f', g', h')
  where
  f' :: Declaration -> Declaration
  f' (DataBindingGroupDeclaration ds) = f (DataBindingGroupDeclaration (map f' ds))
  f' (ValueDeclaration name nameKind bs val) = f (ValueDeclaration name nameKind (map h' bs) ((map (g' *** g') +++ g') val))
  f' (VariableDeclaration name val) = f (VariableDeclaration name (g' val))
  f' (BindingGroupDeclaration ds) = f (BindingGroupDeclaration (map (\(name, nameKind, val) -> (name, nameKind, g' val)) ds))
  f' (TypeClassDeclaration name args implies ds) = f (TypeClassDeclaration name args implies (map f' ds))
  f' (TypeInstanceDeclaration name cs className args ds) = f (TypeInstanceDeclaration name cs className args (mapTypeInstanceBody (map f') ds))
  f' (PositionedDeclaration pos com d) = f (PositionedDeclaration pos com (f' d))
  f' other = f other

  g' :: Expr -> Expr
  g' (UnaryMinus v) = g (UnaryMinus (g' v))
  g' (BinaryNoParens op v1 v2) = g (BinaryNoParens (g' op) (g' v1) (g' v2))
  g' (Parens v) = g (Parens (g' v))
  g' (OperatorSection op (Left v)) = g (OperatorSection (g' op) (Left $ g' v))
  g' (OperatorSection op (Right v)) = g (OperatorSection (g' op) (Right $ g' v))
  g' (ArrayLiteral vs) = g (ArrayLiteral (map g' vs))
  g' (ObjectLiteral vs) = g (ObjectLiteral (map (fmap g') vs))
  g' (ObjectConstructor vs) = g (ObjectConstructor (map (second (fmap g')) vs))
  g' (TypeClassDictionaryConstructorApp name v) = g (TypeClassDictionaryConstructorApp name (g' v))
  g' (Accessor prop v) = g (Accessor prop (g' v))
  g' (ObjectUpdate obj vs) = g (ObjectUpdate (g' obj) (map (fmap g') vs))
  g' (ObjectUpdater obj vs) = g (ObjectUpdater (fmap g' obj) (map (second (fmap g')) vs))
  g' (Abs name v) = g (Abs name (g' v))
  g' (App v vs) = g (App (g' v) (map g' vs))
  g' (IfThenElse v1 v2 v3) = g (IfThenElse (g' v1) (g' v2) (g' v3))
  g' (Case vs alts) = g (Case (map g' vs) (map handleCaseAlternative alts))
  g' (TypedValue check v ty) = g (TypedValue check (g' v) ty)
  g' (Let ds v) = g (Let (map f' ds) (g' v))
  g' (Seq v1 v2) = g (Seq (g' v1) (g' v2))
  g' (Block v) = g (Block (g' v))
  g' (Tuple vs) = g (Tuple (map g' vs))
  g' (Assign name v) = g (Assign name (g' v))
  g' (PositionedValue pos com v) = g (PositionedValue pos com (g' v))
  g' other = g other

  h' :: Binder -> Binder
  h' (ConstructorBinder ctor bs) = h (ConstructorBinder ctor (map h' bs))
  h' (ObjectBinder bs) = h (ObjectBinder (map (fmap h') bs))
  h' (ArrayBinder bs) = h (ArrayBinder (map h' bs))
  h' (NamedBinder name b) = h (NamedBinder name (h' b))
  h' (PositionedBinder pos com b) = h (PositionedBinder pos com (h' b))
  h' (TypedBinder t b) = h (TypedBinder t (h' b))
  h' other = h other

  handleCaseAlternative :: CaseAlternative -> CaseAlternative
  handleCaseAlternative ca =
    ca { caseAlternativeBinders = map h' (caseAlternativeBinders ca)
       , caseAlternativeResult = (map (g' *** g') +++ g') (caseAlternativeResult ca)
       }

everywhereOnValuesTopDownM :: (Functor m, Applicative m, Monad m) =>
  (Declaration -> m Declaration) ->
  (Expr -> m Expr) ->
  (Binder -> m Binder) ->
  (Declaration -> m Declaration, Expr -> m Expr, Binder -> m Binder)
everywhereOnValuesTopDownM f g h = (f' <=< f, g' <=< g, h' <=< h)
  where
  f' (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f' <=< f) ds
  f' (ValueDeclaration name nameKind bs val) = ValueDeclaration name nameKind <$> traverse (h' <=< h) bs <*> eitherM (traverse (pairM (g' <=< g) (g' <=< g))) (g' <=< g) val
  f' (VariableDeclaration name val) = VariableDeclaration name <$> (g' <=< g) val
  f' (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> (g val >>= g')) ds
  f' (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> traverse (f' <=< f) ds
  f' (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse (f' <=< f)) ds
  f' (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> (f d >>= f')
  f' other = f other

  g' (UnaryMinus v) = UnaryMinus <$> (g v >>= g')
  g' (BinaryNoParens op v1 v2) = BinaryNoParens <$> (g op >>= g') <*> (g v1 >>= g') <*> (g v2 >>= g')
  g' (Parens v) = Parens <$> (g v >>= g')
  g' (OperatorSection op (Left v)) = OperatorSection <$> (g op >>= g') <*> (Left <$> (g v >>= g'))
  g' (OperatorSection op (Right v)) = OperatorSection <$> (g op >>= g') <*> (Right <$> (g v >>= g'))
  g' (ArrayLiteral vs) = ArrayLiteral <$> traverse (g' <=< g) vs
  g' (ObjectLiteral vs) = ObjectLiteral <$> traverse (sndM (g' <=< g)) vs
  g' (ObjectConstructor vs) = ObjectConstructor <$> traverse (sndM $ maybeM (g' <=< g)) vs
  g' (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> (g v >>= g')
  g' (Accessor prop v) = Accessor prop <$> (g v >>= g')
  g' (ObjectUpdate obj vs) = ObjectUpdate <$> (g obj >>= g') <*> traverse (sndM (g' <=< g)) vs
  g' (ObjectUpdater obj vs) = ObjectUpdater <$> (maybeM g obj >>= maybeM g') <*> traverse (sndM $ maybeM (g' <=< g)) vs
  g' (Abs name v) = Abs name <$> (g v >>= g')
  g' (App v vs) = App <$> (g v >>= g') <*> traverse (g' <=< g) vs
  g' (IfThenElse v1 v2 v3) = IfThenElse <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g')
  g' (Case vs alts) = Case <$> traverse (g' <=< g) vs <*> traverse handleCaseAlternative alts
  g' (TypedValue check v ty) = TypedValue check <$> (g v >>= g') <*> pure ty
  g' (Let ds v) = Let <$> traverse (f' <=< f) ds <*> (g v >>= g')
  g' (Seq v1 v2) = Seq <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (Block v) = Block <$> (g v >>= g')
  g' (Tuple vs) = Tuple <$> traverse (g' <=< g) vs
  g' (Assign name v) = Assign name <$> (g v >>= g')
  g' (PositionedValue pos com v) = PositionedValue pos com <$> (g v >>= g')
  g' other = g other

  h' (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> traverse (h' <=< h) bs
  h' (ObjectBinder bs) = ObjectBinder <$> traverse (sndM (h' <=< h)) bs
  h' (ArrayBinder bs) = ArrayBinder <$> traverse (h' <=< h) bs
  h' (NamedBinder name b) = NamedBinder name <$> (h b >>= h')
  h' (PositionedBinder pos com b) = PositionedBinder pos com <$> (h b >>= h')
  h' (TypedBinder t b) = TypedBinder t <$> (h b >>= h')
  h' other = h other

  handleCaseAlternative (CaseAlternative bs val) = CaseAlternative <$> traverse (h' <=< h) bs
                                                                   <*> eitherM (traverse (pairM (g' <=< g) (g' <=< g))) (g' <=< g) val

everywhereOnValuesM :: (Functor m, Applicative m, Monad m) =>
  (Declaration -> m Declaration) ->
  (Expr -> m Expr) ->
  (Binder -> m Binder) ->
  (Declaration -> m Declaration, Expr -> m Expr, Binder -> m Binder)
everywhereOnValuesM f g h = (f', g', h')
  where
  f' (DataBindingGroupDeclaration ds) = (DataBindingGroupDeclaration <$> traverse f' ds) >>= f
  f' (ValueDeclaration name nameKind bs val) = (ValueDeclaration name nameKind <$> traverse h' bs <*> eitherM (traverse (pairM g' g')) g' val) >>= f
  f' (VariableDeclaration name val) = (VariableDeclaration name <$> g' val) >>= f
  f' (BindingGroupDeclaration ds) = (BindingGroupDeclaration <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> g' val) ds) >>= f
  f' (TypeClassDeclaration name args implies ds) = (TypeClassDeclaration name args implies <$> traverse f' ds) >>= f
  f' (TypeInstanceDeclaration name cs className args ds) = (TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse f') ds) >>= f
  f' (PositionedDeclaration pos com d) = (PositionedDeclaration pos com <$> f' d) >>= f
  f' other = f other

  g' (UnaryMinus v) = (UnaryMinus <$> g' v) >>= g
  g' (BinaryNoParens op v1 v2) = (BinaryNoParens <$> g' op <*> g' v1 <*> g' v2) >>= g
  g' (Parens v) = (Parens <$> g' v) >>= g
  g' (OperatorSection op (Left v)) = (OperatorSection <$> g' op <*> (Left <$> g' v)) >>= g
  g' (OperatorSection op (Right v)) = (OperatorSection <$> g' op <*> (Right <$> g' v)) >>= g
  g' (ArrayLiteral vs) = (ArrayLiteral <$> traverse g' vs) >>= g
  g' (ObjectLiteral vs) = (ObjectLiteral <$> traverse (sndM g') vs) >>= g
  g' (ObjectConstructor vs) = (ObjectConstructor <$> traverse (sndM $ maybeM g') vs) >>= g
  g' (TypeClassDictionaryConstructorApp name v) = (TypeClassDictionaryConstructorApp name <$> g' v) >>= g
  g' (Accessor prop v) = (Accessor prop <$> g' v) >>= g
  g' (ObjectUpdate obj vs) = (ObjectUpdate <$> g' obj <*> traverse (sndM g') vs) >>= g
  g' (ObjectUpdater obj vs) = (ObjectUpdater <$> maybeM g' obj <*> traverse (sndM $ maybeM g') vs) >>= g
  g' (Abs name v) = (Abs name <$> g' v) >>= g
  g' (App v1 vs) = (App <$> g' v1 <*> traverse g' vs) >>= g
  g' (IfThenElse v1 v2 v3) = (IfThenElse <$> g' v1 <*> g' v2 <*> g' v3) >>= g
  g' (Case vs alts) = (Case <$> traverse g' vs <*> traverse handleCaseAlternative alts) >>= g
  g' (TypedValue check v ty) = (TypedValue check <$> g' v <*> pure ty) >>= g
  g' (Let ds v) = (Let <$> traverse f' ds <*> g' v) >>= g
  g' (Assign name v) = (Assign name <$> g' v) >>= g
  g' (PositionedValue pos com v) = (PositionedValue pos com <$> g' v) >>= g
  g' other = g other

  h' (ConstructorBinder ctor bs) = (ConstructorBinder ctor <$> traverse h' bs) >>= h
  h' (ObjectBinder bs) = (ObjectBinder <$> traverse (sndM h') bs) >>= h
  h' (ArrayBinder bs) = (ArrayBinder <$> traverse h' bs) >>= h
  h' (NamedBinder name b) = (NamedBinder name <$> h' b) >>= h
  h' (PositionedBinder pos com b) = (PositionedBinder pos com <$> h' b) >>= h
  h' (TypedBinder t b) = (TypedBinder t <$> h' b) >>= h
  h' other = h other

  handleCaseAlternative (CaseAlternative bs val) = CaseAlternative <$> traverse h' bs
                                                                   <*> eitherM (traverse (pairM g' g')) g' val

everythingOnValues :: (r -> r -> r) ->
                      (Declaration -> r) ->
                      (Expr -> r) ->
                      (Binder -> r) ->
                      (CaseAlternative -> r) ->
                      (Declaration -> r, Expr -> r, Binder -> r, CaseAlternative -> r)
everythingOnValues (<>) f g h i = (f', g', h', i')
  where
  f' d@(DataBindingGroupDeclaration ds) = foldl (<>) (f d) (map f' ds)
  f' d@(ValueDeclaration _ _ bs (Right val)) = foldl (<>) (f d) (map h' bs) <> g' val
  f' d@(ValueDeclaration _ _ bs (Left gs)) = foldl (<>) (f d) (map h' bs ++ concatMap (\(grd, val) -> [g' grd, g' val]) gs)
  f' d@(VariableDeclaration _ val) = f d <> g' val
  f' d@(BindingGroupDeclaration ds) = foldl (<>) (f d) (map (\(_, _, val) -> g' val) ds)
  f' d@(TypeClassDeclaration _ _ _ ds) = foldl (<>) (f d) (map f' ds)
  f' d@(TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldl (<>) (f d) (map f' ds)
  f' d@(PositionedDeclaration _ _ d1) = f d <> f' d1
  f' d = f d

  g' v@(UnaryMinus v1) = g v <> g' v1
  g' v@(BinaryNoParens op v1 v2) = g v <> g' op <> g' v1 <> g' v2
  g' v@(Parens v1) = g v <> g' v1
  g' v@(OperatorSection op (Left v1)) = g v <> g' op <> g' v1
  g' v@(OperatorSection op (Right v1)) = g v <> g' op <> g' v1
  g' v@(ArrayLiteral vs) = foldl (<>) (g v) (map g' vs)
  g' v@(ObjectLiteral vs) = foldl (<>) (g v) (map (g' . snd) vs)
  g' v@(ObjectConstructor vs) = foldl (<>) (g v) (map g' (mapMaybe snd vs))
  g' v@(TypeClassDictionaryConstructorApp _ v1) = g v <> g' v1
  g' v@(Accessor _ v1) = g v <> g' v1
  g' v@(ObjectUpdate obj vs) = foldl (<>) (g v <> g' obj) (map (g' . snd) vs)
  g' v@(ObjectUpdater obj vs) = foldl (<>) (maybe (g v) (\x -> g v <> g' x) obj) (map g' (mapMaybe snd vs))
  g' v@(Abs _ v1) = g v <> g' v1
  g' v@(App v1 vs) = foldl (<>) (g v <> g' v1) (map g' vs)
  g' v@(IfThenElse v1 v2 v3) = g v <> g' v1 <> g' v2 <> g' v3
  g' v@(Case vs alts) = foldl (<>) (foldl (<>) (g v) (map g' vs)) (map i' alts)
  g' v@(TypedValue _ v1 _) = g v <> g' v1
  g' v@(Let ds v1) = foldl (<>) (g v) (map f' ds) <> g' v1
  g' v@(Seq v1 v2) = g v <> g' v1 <> g' v2
  g' v@(Block v1) = g v <> g' v1
  g' v@(Tuple vs) = foldl (<>) (g v) (map g' vs)
  g' v@(Assign _ v1) = g v <> g' v1
  g' v@(PositionedValue _ _ v1) = g v <> g' v1
  g' v = g v

  h' b@(ConstructorBinder _ bs) = foldl (<>) (h b) (map h' bs)
  h' b@(ObjectBinder bs) = foldl (<>) (h b) (map (h' . snd) bs)
  h' b@(ArrayBinder bs) = foldl (<>) (h b) (map h' bs)
  h' b@(NamedBinder _ b1) = h b <> h' b1
  h' b@(PositionedBinder _ _ b1) = h b <> h' b1
  h' b@(TypedBinder _ b1) = h b <> h' b1
  h' b = h b

  i' ca@(CaseAlternative bs (Right val)) = foldl (<>) (i ca) (map h' bs) <> g' val
  i' ca@(CaseAlternative bs (Left gs)) = foldl (<>) (i ca) (map h' bs ++ concatMap (\(grd, val) -> [g' grd, g' val]) gs)

everythingWithContextOnValues ::
  s ->
  r ->
  (r -> r -> r) ->
  (s -> Declaration       -> (s, r)) ->
  (s -> Expr              -> (s, r)) ->
  (s -> Binder            -> (s, r)) ->
  (s -> CaseAlternative   -> (s, r)) ->
  ( Declaration       -> r
  , Expr              -> r
  , Binder            -> r
  , CaseAlternative   -> r
  )
everythingWithContextOnValues s0 r0 (<>) f g h i = (f'' s0, g'' s0, h'' s0, i'' s0)
  where
  f'' s d = let (s', r) = f s d in r <> f' s' d

  f' s (DataBindingGroupDeclaration ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (ValueDeclaration _ _ bs (Right val)) = foldl (<>) r0 (map (h'' s) bs) <> g'' s val
  f' s (ValueDeclaration _ _ bs (Left gs)) = foldl (<>) r0 (map (h'' s) bs ++ concatMap (\(grd, val) -> [g'' s grd, g'' s val]) gs)
  f' s (VariableDeclaration _ val) = g'' s val
  f' s (BindingGroupDeclaration ds) = foldl (<>) r0 (map (\(_, _, val) -> g'' s val) ds)
  f' s (TypeClassDeclaration _ _ _ ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldl (<>) r0 (map (f'' s) ds)
  f' s (PositionedDeclaration _ _ d1) = f'' s d1
  f' _ _ = r0

  g'' s v = let (s', r) = g s v in r <> g' s' v

  g' s (UnaryMinus v1) = g'' s v1
  g' s (BinaryNoParens op v1 v2) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens v1) = g'' s v1
  g' s (OperatorSection op (Left v)) = g'' s op <> g'' s v
  g' s (OperatorSection op (Right v)) = g'' s op <> g'' s v
  g' s (ArrayLiteral vs) = foldl (<>) r0 (map (g'' s) vs)
  g' s (ObjectLiteral vs) = foldl (<>) r0 (map (g'' s . snd) vs)
  g' s (ObjectConstructor vs) = foldl (<>) r0 (map (g'' s) (mapMaybe snd vs))
  g' s (TypeClassDictionaryConstructorApp _ v1) = g'' s v1
  g' s (Accessor _ v1) = g'' s v1
  g' s (ObjectUpdate obj vs) = foldl (<>) (g'' s obj) (map (g'' s . snd) vs)
  g' s (ObjectUpdater obj vs) = foldl (<>) (maybe r0 (g'' s) obj) (map (g'' s) (mapMaybe snd vs))
  g' s (Abs _ v1) = g'' s v1
  g' s (App v vs) = foldl (<>) (g'' s v) (map (g'' s) vs)
  g' s (IfThenElse v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case vs alts) = foldl (<>) (foldl (<>) r0 (map (g'' s) vs)) (map (i'' s) alts)
  g' s (TypedValue _ v1 _) = g'' s v1
  g' s (Let ds v) = foldl (<>) r0 (map (f'' s) ds) <> g'' s v
  g' s (Seq v1 v2) = g'' s v1 <> g'' s v2
  g' s (Block v) = g'' s v
  g' s (Tuple vs) = foldl (<>) r0 (map (g'' s) vs)
  g' s (Assign _ v1) = g'' s v1
  g' s (PositionedValue _ _ v1) = g'' s v1
  g' _ _ = r0

  h'' s b = let (s', r) = h s b in r <> h' s' b

  h' s (ConstructorBinder _ bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (ObjectBinder bs) = foldl (<>) r0 (map (h'' s . snd) bs)
  h' s (ArrayBinder bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (NamedBinder _ b1) = h'' s b1
  h' s (PositionedBinder _ _ b1) = h'' s b1
  h' s (TypedBinder _ b1) = h'' s b1
  h' _ _ = r0

  i'' s ca = let (s', r) = i s ca in r <> i' s' ca

  i' s (CaseAlternative bs (Right val)) = foldl (<>) r0 (map (h'' s) bs) <> g'' s val
  i' s (CaseAlternative bs (Left gs)) = foldl (<>) r0 (map (h'' s) bs ++ concatMap (\(grd, val) -> [g'' s grd, g'' s val]) gs)

everywhereWithContextOnValuesM :: (Functor m, Applicative m, Monad m) =>
  s ->
  (s -> Declaration       -> m (s, Declaration)) ->
  (s -> Expr              -> m (s, Expr)) ->
  (s -> Binder            -> m (s, Binder)) ->
  (s -> CaseAlternative   -> m (s, CaseAlternative)) ->
  ( Declaration       -> m Declaration
  , Expr              -> m Expr
  , Binder            -> m Binder
  , CaseAlternative   -> m CaseAlternative)
everywhereWithContextOnValuesM s0 f g h i = (f'' s0, g'' s0, h'' s0, i'' s0)
  where
  f'' s = uncurry f' <=< f s

  f' s (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f'' s) ds
  f' s (ValueDeclaration name nameKind bs val) = ValueDeclaration name nameKind <$> traverse (h'' s) bs <*> eitherM (traverse (pairM (g'' s) (g'' s))) (g'' s) val
  f' s (VariableDeclaration name val) = VariableDeclaration name <$> g'' s val
  f' s (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> traverse (thirdM (g'' s)) ds
  f' s (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> traverse (f'' s) ds
  f' s (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse (f'' s)) ds
  f' s (PositionedDeclaration pos com d1) = PositionedDeclaration pos com <$> f'' s d1
  f' _ other = return other

  g'' s = uncurry g' <=< g s

  g' s (UnaryMinus v) = UnaryMinus <$> g'' s v
  g' s (BinaryNoParens op v1 v2) = BinaryNoParens <$> g'' s op <*> g'' s v1 <*> g'' s v2
  g' s (Parens v) = Parens <$> g'' s v
  g' s (OperatorSection op (Left v)) = OperatorSection <$> g'' s op <*> (Left <$> g'' s v)
  g' s (OperatorSection op (Right v)) = OperatorSection <$> g'' s op <*> (Right <$> g'' s v)
  g' s (ArrayLiteral vs) = ArrayLiteral <$> traverse (g'' s) vs
  g' s (ObjectLiteral vs) = ObjectLiteral <$> traverse (sndM (g'' s)) vs
  g' s (ObjectConstructor vs) = ObjectConstructor <$> traverse (sndM $ maybeM (g'' s)) vs
  g' s (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> g'' s v
  g' s (Accessor prop v) = Accessor prop <$> g'' s v
  g' s (ObjectUpdate obj vs) = ObjectUpdate <$> g'' s obj <*> traverse (sndM (g'' s)) vs
  g' s (ObjectUpdater obj vs) = ObjectUpdater <$> maybeM (g'' s) obj <*> traverse (sndM $ maybeM (g'' s)) vs
  g' s (Abs name v) = Abs name <$> g'' s v
  g' s (App v vs) = App <$> g'' s v <*> traverse (g'' s) vs
  g' s (IfThenElse v1 v2 v3) = IfThenElse <$> g'' s v1 <*> g'' s v2 <*> g'' s v3
  g' s (Case vs alts) = Case <$> traverse (g'' s) vs <*> traverse (i'' s) alts
  g' s (TypedValue check v ty) = TypedValue check <$> g'' s v <*> pure ty
  g' s (Let ds v) = Let <$> traverse (f'' s) ds <*> g'' s v
  g' s (Seq v1 v2) = Seq <$> g'' s v1 <*> g'' s v2
  g' s (Block v) = Block <$> g'' s v
  g' s (Tuple vs) = Tuple <$> traverse (g'' s) vs
  g' s (Assign name v) = Assign name <$> g'' s v
  g' s (PositionedValue pos com v) = PositionedValue pos com <$> g'' s v
  g' _ other = return other

  h'' s = uncurry h' <=< h s

  h' s (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> traverse (h'' s) bs
  h' s (ObjectBinder bs) = ObjectBinder <$> traverse (sndM (h'' s)) bs
  h' s (ArrayBinder bs) = ArrayBinder <$> traverse (h'' s) bs
  h' s (NamedBinder name b) = NamedBinder name <$> h'' s b
  h' s (PositionedBinder pos com b) = PositionedBinder pos com <$> h'' s b
  h' s (TypedBinder t b) = TypedBinder t <$> h'' s b
  h' _ other = return other

  i'' s = uncurry i' <=< i s

  i' s (CaseAlternative bs val) = CaseAlternative <$> traverse (h'' s) bs <*> eitherM (traverse (pairM (g'' s) (g'' s))) (g'' s) val



everythingWithScope ::
  (Monoid r) =>
  (S.Set Ident -> Declaration -> r) ->
  (S.Set Ident -> Expr -> r) ->
  (S.Set Ident -> Binder -> r) ->
  (S.Set Ident -> CaseAlternative -> r) ->
  ( S.Set Ident -> Declaration       -> r
  , S.Set Ident -> Expr              -> r
  , S.Set Ident -> Binder            -> r
  , S.Set Ident -> CaseAlternative   -> r)
everythingWithScope f g h i = (f'', g'', h'', i'')
  where
  -- Avoid importing Data.Monoid and getting shadowed names above
  (<>) = mappend

  f'' s a = f s a <> f' s a

  f' s (DataBindingGroupDeclaration ds) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in foldMap (f'' s') ds
  f' s (ValueDeclaration name _ bs (Right val)) =
    let s' = S.insert name s
    in foldMap (h'' s') bs <> g'' s' val
  f' s (ValueDeclaration name _ bs (Left gs)) =
    let s' = S.insert name s
        s'' = S.union s' (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s') bs <> foldMap (\(grd, val) -> g'' s'' grd <> g'' s'' val) gs
  f' s (BindingGroupDeclaration ds) =
    let s' = S.union s (S.fromList (map (\(name, _, _) -> name) ds))
    in foldMap (\(_, _, val) -> g'' s' val) ds
  f' s (TypeClassDeclaration _ _ _ ds) = foldMap (f'' s) ds
  f' s (TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldMap (f'' s) ds
  f' s (PositionedDeclaration _ _ d) = f'' s d
  f' _ _ = mempty

  g'' s a = g s a <> g' s a

  g' s (UnaryMinus v1) = g'' s v1
  g' s (BinaryNoParens op v1 v2) = g' s op <> g' s v1 <> g' s v2
  g' s (Parens v1) = g'' s v1
  g' s (OperatorSection op (Left v)) = g'' s op <> g'' s v
  g' s (OperatorSection op (Right v)) = g'' s op <> g'' s v
  g' s (ArrayLiteral vs) = foldMap (g'' s) vs
  g' s (ObjectLiteral vs) = foldMap (g'' s . snd) vs
  g' s (ObjectConstructor vs) = foldMap (g'' s) (mapMaybe snd vs)
  g' s (TypeClassDictionaryConstructorApp _ v1) = g'' s v1
  g' s (Accessor _ v1) = g'' s v1
  g' s (ObjectUpdate obj vs) = g'' s obj <> foldMap (g'' s . snd) vs
  g' s (ObjectUpdater obj vs) = foldMap (g'' s) obj <> foldMap (g'' s) (mapMaybe snd vs)
  g' s (Abs (Left names) v1) =
    let s' = foldl (flip S.insert) s names
    in g'' s' v1
  g' s (Abs (Right b) v1) =
    let s' = S.union (S.fromList (binderNames b)) s
    in g'' s' v1
  g' s (App v1 vs) = foldMap (g'' s) (v1 : vs)
  g' s (IfThenElse v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case vs alts) = foldMap (g'' s) vs <> foldMap (i'' s) alts
  g' s (TypedValue _ v1 _) = g'' s v1
  g' s (Let ds v1) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in foldMap (f'' s') ds <> g'' s' v1
  g' s (Seq v1 v2) = g'' s v1 <> g'' s v2
  g' s (Tuple vs) = foldMap (g'' s) vs
  g' s (Assign _ v1) = g'' s v1
  g' s (PositionedValue _ _ v1) = g'' s v1
  g' _ _ = mempty

  h'' s a = h s a <> h' s a

  h' s (ConstructorBinder _ bs) = foldMap (h'' s) bs
  h' s (ObjectBinder bs) = foldMap (h'' s . snd) bs
  h' s (ArrayBinder bs) = foldMap (h'' s) bs
  h' s (NamedBinder name b1) =
    let s' = S.insert name s
    in h'' s' b1
  h' s (PositionedBinder _ _ b1) = h'' s b1
  h' s (TypedBinder _ b1) = h'' s b1
  h' _ _ = mempty

  i'' s a = i s a <> i' s a

  i' s (CaseAlternative bs (Right val)) =
    let s' = S.union s (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s) bs <> g'' s' val
  i' s (CaseAlternative bs (Left gs)) =
    let s' = S.union s (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s) bs <> foldMap (\(grd, val) -> g'' s' grd <> g'' s' val) gs

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent (PositionedDeclaration _ _ d) = getDeclIdent d
  getDeclIdent (ValueDeclaration ident _ _ _) = Just ident
  getDeclIdent (TypeDeclaration ident _) = Just ident
  getDeclIdent _ = Nothing

accumTypes :: (Monoid r) => (Type -> r) -> (Declaration -> r, Expr -> r, Binder -> r, CaseAlternative -> r)
accumTypes f = everythingOnValues mappend forDecls forValues (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ _ dctors) = mconcat (concatMap (map f . snd) dctors)
  forDecls (ExternDeclaration _ ty) = f ty
  forDecls (TypeClassDeclaration _ _ implies _) = mconcat (concatMap (map f . snd) implies)
  forDecls (TypeInstanceDeclaration _ cs _ tys _) = mconcat (concatMap (map f . snd) cs) `mappend` mconcat (map f tys)
  forDecls (TypeSynonymDeclaration _ _ ty) = f ty
  forDecls (TypeDeclaration _ ty) = f ty
  forDecls _ = mempty

  forValues (TypeClassDictionary (_, cs) _) = mconcat (map f cs)
  forValues (SuperClassDictionary _ tys) = mconcat (map f tys)
  forValues (TypedValue _ _ ty) = f ty
  forValues _ = mempty
