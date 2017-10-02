module React.Redux.Reducer where

import Prelude

import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, wrap, unwrap)

newtype Reducer action state state' = Reducer (action -> state -> state')

derive instance newtypeReducer :: Newtype (Reducer action state state') _

instance semigroupoidReducer :: Semigroupoid (Reducer action) where
  compose (Reducer f) (Reducer g) = Reducer (\action -> f action <<< g action)

instance categoryReducer :: Category (Reducer action) where
  id = Reducer (const id)

instance functorReducer :: Functor (Reducer action state) where
  map f (Reducer g) = Reducer ((<<<) f <<< g)

instance applyReducer :: Apply (Reducer action state) where
  apply (Reducer f) (Reducer g) = Reducer (\action state -> (f action) state (g action state))

instance applicativeReducer :: Applicative (Reducer action state) where
  pure a = Reducer (\_ _ -> a)

instance bindReducer :: Bind (Reducer action state) where
  bind (Reducer m) f = Reducer (\action state -> unwrap (f (m action state)) action state)

instance monadReducer :: Monad (Reducer action state)

instance semigroupReducer :: Semigroup state' => Semigroup (Reducer action state state') where
  append (Reducer f) (Reducer g) = Reducer (\action state -> f action state <> g action state)

instance monoidReducer :: Monoid state' => Monoid (Reducer action state state') where
  mempty = Reducer (const (const (mempty)))

reducerFlipped :: forall action state state'. (state -> action -> state') -> Reducer action state state'
reducerFlipped = wrap <<< flip
