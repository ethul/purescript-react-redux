module React.Redux.Middleware where

import Prelude

import Effect (Effect)

import Data.Newtype (class Newtype, unwrap)

import React.Redux.Types (Dispatch)

type MiddlewareAPI state action
  = { getState :: Effect state
    , dispatch :: Dispatch action action
    }

newtype Middleware state action a b = Middleware (MiddlewareAPI state action -> Dispatch action a -> Dispatch action b)

derive instance newtypeMiddleware :: Newtype (Middleware state action a b) _

instance semigroupoidMiddleware :: Semigroupoid (Middleware state action) where
  compose (Middleware f) (Middleware g) = Middleware (\api -> f api <<< g api)

instance categoryMiddleware :: Category (Middleware state action) where
  identity = Middleware (const identity)

instance functorMiddleware :: Functor (Middleware state action a) where
  map f (Middleware g) = Middleware (\api next action -> f <$> g api next action)

instance applyMiddleware :: Apply (Middleware state action a) where
  apply (Middleware f) (Middleware g) =
    Middleware $ \api next action -> do
      a <- g api next action

      k <- f api next action

      pure (k a)

instance applicativeMiddleware :: Applicative (Middleware state action a) where
  pure a = Middleware (\_ _ _ -> pure a)

instance bindMiddleware :: Bind (Middleware state action a) where
  bind (Middleware m) f =
    Middleware $ \api next action -> do
      a <- m api next action

      unwrap (f a) api next action

instance monadMiddleware :: Monad (Middleware state action a)

instance semigroupMiddleware :: Semigroup b => Semigroup (Middleware state action a b) where
  append (Middleware f) (Middleware g) =
    Middleware $ \api next action -> do
      b1 <- f api next action

      b2 <- g api next action

      pure (b1 <> b2)

instance monoidMiddleware :: Monoid b => Monoid (Middleware state action a b) where
  mempty = Middleware (const (const (const (pure mempty))))
