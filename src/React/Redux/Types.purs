module React.Redux.Types where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Uncurried (EffFn1)

import Data.Function.Uncurried (Fn2)
import Data.Nullable (Nullable)

import React.Redux.Reducer as Reducer

-- | Effect type for Redux.
foreign import data REDUX :: Effect

-- | Convenience type alias for the Redux effect.
type ReduxEffect eff = (redux :: REDUX | eff)

-- | Reducer that does not change the type of the `state`.
type Reducer action state = Reducer.Reducer action state state

-- | Dispatching function that returns the action it was passed.
type BaseDispatch eff action = Dispatch eff action action

-- | Dispatching function that returns a `result` type given an action.
type Dispatch eff action result = action -> Eff (ReduxEffect eff) result

-- | Redux actions must be a record with a `type` field.
type ReduxAction r = { type :: String | r }

-- | Convenience type for converting an `action` in this module to a `ReduxAction`.
type ReduxAction' action = ReduxAction (action :: action)

-- | Reducing function that takes a `state` and `ReduxAction` and returns a `state`.
type ReduxReducer state action = Fn2 state (ReduxAction' (Nullable action)) state

-- | The `ReduxBaseDispatch` is the dispatching function provided to the store without any middleware.
type ReduxBaseDispatch eff action = EffFn1 (ReduxEffect eff) (ReduxAction' action) (ReduxAction' action)

-- | Allows `ReduxMiddleware` to wrap the `ReduxBaseDispatch` function to return a different result to be passed to the next `ReduxMiddleware`.
type ReduxDispatch eff action result = EffFn1 (ReduxEffect eff) (ReduxAction' action) result

-- | Simplified `Store` representation passed to each middleware.
type ReduxMiddlewareAPI eff state action
  = { dispatch :: ReduxBaseDispatch eff action
    , getState :: Eff (ReduxEffect eff) state
    }

-- | Function that composes dispatch functions. Purposely restricted to dispatching `action` types here.
type ReduxMiddleware eff state action a b = ReduxMiddlewareAPI eff state action -> ReduxDispatch eff action a -> ReduxDispatch eff action b

-- | Foreign Redux store creator function.
foreign import data ReduxStoreCreator :: # Effect -> Type -> Type -> Type

-- | Type alias for a foreign Redux store enhancer, taking a `ReduxStoreCreator` and returning a `ReduxStoreCreator`.
type ReduxStoreEnhancer eff state action = ReduxStoreCreator eff state action -> ReduxStoreCreator eff state action

-- | Type alias for a foreign `ReduxStore`
type ReduxStore eff state action
  = { dispatch :: ReduxBaseDispatch eff action
    , getState :: Eff (ReduxEffect eff) state
    , subscribe :: Eff (ReduxEffect eff) Unit -> Eff (ReduxEffect eff) Unit
    , replaceReducer :: EffFn1 (ReduxEffect eff) (ReduxReducer state action) Unit
    }
