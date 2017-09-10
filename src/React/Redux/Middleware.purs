module React.Redux.Middleware where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)

import React.Redux.Types (ReduxEffect, Dispatch)

type MiddlewareAPI eff state action
  = { getState :: Eff (ReduxEffect eff) state
    , dispatch :: Dispatch eff action action
    }

newtype Middleware eff state action a b = Middleware (MiddlewareAPI eff state action -> Dispatch eff action a -> Dispatch eff action b)

derive instance newtypeMiddleware :: Newtype (Middleware eff state action a b) _

instance semigroupoidMiddleware :: Semigroupoid (Middleware eff state action) where
  compose (Middleware f) (Middleware g) = Middleware (\api -> f api <<< g api)

instance categoryMiddleware :: Category (Middleware eff state action) where
  id = Middleware (const id)

instance functorMiddleware :: Functor (Middleware eff state action a) where
  map f (Middleware g) = Middleware (\api next action -> f <$> g api next action)

instance applyMiddleware :: Apply (Middleware eff state action a) where
  apply (Middleware f) (Middleware g) =
    Middleware $ \api next action -> do
      a <- g api next action

      k <- f api next action

      pure (k a)

instance applicativeMiddleware :: Applicative (Middleware eff state action a) where
  pure a = Middleware (\_ _ _ -> pure a)

instance bindMiddleware :: Bind (Middleware eff state action a) where
  bind (Middleware m) f =
    Middleware $ \api next action -> do
      a <- m api next action

      unwrap (f a) api next action

instance monadMiddleware :: Monad (Middleware eff state action a)

instance semigroupMiddleware :: Semigroup b => Semigroup (Middleware eff state action a b) where
  append (Middleware f) (Middleware g) =
    Middleware $ \api next action -> do
      b1 <- f api next action

      b2 <- g api next action

      pure (b1 <> b2)

instance monoidMiddleware :: Monoid b => Monoid (Middleware eff state action a b) where
  mempty = Middleware (const (const (const (pure mempty))))
