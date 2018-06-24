module React.Redux.Types where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)

import Data.Function.Uncurried (Fn2)
import Data.Nullable (Nullable)

import React.Redux.Reducer as Reducer

-- | Reducer that does not change the type of the `state`.
type Reducer action state = Reducer.Reducer action state state

-- | Dispatching function that returns the action it was passed.
type BaseDispatch action = Dispatch action action

-- | Dispatching function that returns a `result` type given an action.
type Dispatch action result = action -> Effect result

-- | Redux actions must be a record with a `type` field.
type ReduxAction r = { type :: String | r }

-- | Convenience type for converting an `action` in this module to a `ReduxAction`.
type ReduxAction' action = ReduxAction (action :: action)

-- | Reducing function that takes a `state` and `ReduxAction` and returns a `state`.
type ReduxReducer state action = Fn2 state (ReduxAction' (Nullable action)) state

-- | The `ReduxBaseDispatch` is the dispatching function provided to the store without any middleware.
type ReduxBaseDispatch action = EffectFn1 (ReduxAction' action) (ReduxAction' action)

-- | Allows `ReduxMiddleware` to wrap the `ReduxBaseDispatch` function to return a different result to be passed to the next `ReduxMiddleware`.
type ReduxDispatch action result = EffectFn1 (ReduxAction' action) result

-- | Simplified `Store` representation passed to each middleware.
type ReduxMiddlewareAPI state action
  = { dispatch :: ReduxBaseDispatch action
    , getState :: Effect state
    }

-- | Function that composes dispatch functions. Purposely restricted to dispatching `action` types here.
type ReduxMiddleware state action a b = ReduxMiddlewareAPI state action -> ReduxDispatch action a -> ReduxDispatch action b

-- | Foreign Redux store creator function.
foreign import data ReduxStoreCreator :: Type -> Type -> Type

-- | Type alias for a foreign Redux store enhancer, taking a `ReduxStoreCreator` and returning a `ReduxStoreCreator`.
type ReduxStoreEnhancer state action = ReduxStoreCreator state action -> ReduxStoreCreator state action

-- | Type alias for a foreign `ReduxStore`
type ReduxStore state action
  = { dispatch :: ReduxBaseDispatch action
    , getState :: Effect state
    , subscribe :: Effect Unit -> Effect Unit
    , replaceReducer :: EffectFn1 (ReduxReducer state action) Unit
    }
