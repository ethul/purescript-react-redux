module React.Redux.Internal where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, mkEffFn1, mkEffFn2, runEffFn1, runEffFn2)

import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap, unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String (indexOf)

import Unsafe.Coerce (unsafeCoerce)

import React.Redux.Middleware (Middleware, MiddlewareAPI)

import React.Redux.Types
  ( BaseDispatch
  , Dispatch
  , Reducer
  , Store
  , StoreCreator
  , StoreEnhancer
  , ReduxEffect
  )

-- | Redux actions must be a record with a `type` field.
type ReduxAction r = { type :: String | r }

-- | Convenience type for converting an `action` in this module to a `ReduxAction`.
type ReduxAction' action = ReduxAction (action :: action)

-- | Reducing function that takes a `state` and `ReduxAction` and returns a `state`.
type ReduxReducer state action = Fn2 state (ReduxAction' action) state

-- | The `ReduxBaseDispatch` is the dispatching function provided to the store witout any middleware.
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

type ReduxStore eff state action
  = { dispatch :: ReduxBaseDispatch eff action
    , getState :: Eff (ReduxEffect eff) state
    , subscribe :: Eff (ReduxEffect eff) Unit -> Eff (ReduxEffect eff) Unit
    , replaceReducer :: EffFn1 (ReduxEffect eff) (ReduxReducer state action) Unit
    }

type ReduxStoreCreator eff state action = EffFn2 (ReduxEffect eff) (ReduxReducer state action) state (ReduxStore eff state action)

type ReduxStoreEnhancer eff state action = ReduxStoreCreator eff state action -> ReduxStoreCreator eff state action

reduxActionType :: String
reduxActionType = "@@PURESCRIPT_REACT_REDUX"

reduxActionToAction :: forall action. ReduxAction' action -> action
reduxActionToAction { action } = action

actionToReduxAction :: forall action. action -> ReduxAction' action
actionToReduxAction action =
  { type: reduxActionType <> "/" <> suffix
  , action
  }
  where
  constructor :: Maybe { name :: Nullable String}
  constructor = toMaybe (unsafeCoerce action).constructor

  suffix :: String
  suffix = fromMaybe "UnknownConstructorName" (constructor >>= \a -> toMaybe a.name)

reduxActionToMaybeAction :: forall action. ReduxAction' action -> Maybe action
reduxActionToMaybeAction { type: type_, action } =
  if indexOf (wrap reduxActionType) type_ == Just 0
     then Just action
     else Nothing

reduxReducerToReducer :: forall state action. ReduxReducer state action -> Reducer action state
reduxReducerToReducer = wrap <<< (>>>) actionToReduxAction <<< flip <<< runFn2

reducerToReduxReducer :: forall state action. Reducer action state -> ReduxReducer state action
reducerToReduxReducer = mkFn2 <<< (<<<) ((>>>) reduxActionToAction) <<< flip <<< unwrap

reduxBaseDispatchToBaseDispatch :: forall eff action. ReduxBaseDispatch eff action -> BaseDispatch eff action
reduxBaseDispatchToBaseDispatch = (>>>) actionToReduxAction <<< (<<<) (map reduxActionToAction) <<< runEffFn1

reduxDispatchToDispatch :: forall eff action result. ReduxDispatch eff action result -> Dispatch eff action result
reduxDispatchToDispatch = (>>>) actionToReduxAction <<< runEffFn1

baseDispatchToReduxBaseDispatch :: forall eff action. Dispatch eff action action -> ReduxBaseDispatch eff action
baseDispatchToReduxBaseDispatch = mkEffFn1 <<< (>>>) reduxActionToAction <<< (<<<) (map actionToReduxAction)

dispatchToReduxDispatch :: forall eff action result. Dispatch eff action result -> ReduxDispatch eff action result
dispatchToReduxDispatch = mkEffFn1 <<< (>>>) reduxActionToAction

reduxMiddlewareApiToMiddlewareApi :: forall eff state action. ReduxMiddlewareAPI eff state action -> MiddlewareAPI eff state action
reduxMiddlewareApiToMiddlewareApi { getState, dispatch } = { getState, dispatch: reduxBaseDispatchToBaseDispatch dispatch }

middlewareApiToReduxMiddlewareApi :: forall eff state action. MiddlewareAPI eff state action -> ReduxMiddlewareAPI eff state action
middlewareApiToReduxMiddlewareApi { getState, dispatch } = { getState, dispatch: baseDispatchToReduxBaseDispatch dispatch }

reduxMiddlewareToMiddleware :: forall eff state action a b. ReduxMiddleware eff state action a b -> Middleware eff state action a b
reduxMiddlewareToMiddleware reduxMiddleware = wrap $ \middlewareApi dispatch -> reduxDispatchToDispatch (reduxMiddleware (middlewareApiToReduxMiddlewareApi middlewareApi) (dispatchToReduxDispatch dispatch))

middlewareToReduxMiddleware :: forall eff state action a b. Middleware eff state action a b -> ReduxMiddleware eff state action a b
middlewareToReduxMiddleware middleware reduxMiddlewareApi = reduxMiddleware
  where
  middleware' :: Dispatch eff action a -> Dispatch eff action b
  middleware' = unwrap middleware (reduxMiddlewareApiToMiddlewareApi reduxMiddlewareApi)

  reduxMiddleware :: ReduxDispatch eff action a -> ReduxDispatch eff action b
  reduxMiddleware reduxDispatch = dispatchToReduxDispatch (middleware' (reduxDispatchToDispatch reduxDispatch))

reduxStoreToStore :: forall eff state action. ReduxStore eff state action -> Store eff state action
reduxStoreToStore reduxStore =
  { dispatch: reduxBaseDispatchToBaseDispatch reduxStore.dispatch
  , getState: reduxStore.getState
  , subscribe: reduxStore.subscribe
  , replaceReducer: runEffFn1 reduxStore.replaceReducer <<< reducerToReduxReducer
  }

storeToReduxStore :: forall eff state action. Store eff state action -> ReduxStore eff state action
storeToReduxStore store =
  { dispatch: baseDispatchToReduxBaseDispatch store.dispatch
  , getState: store.getState
  , subscribe: store.subscribe
  , replaceReducer: mkEffFn1 (store.replaceReducer <<< reduxReducerToReducer)
  }

reduxStoreCreatorToStoreCreator :: forall eff state action. ReduxStoreCreator eff state action -> StoreCreator eff state action
reduxStoreCreatorToStoreCreator = (<<<) ((<<<) (map reduxStoreToStore)) <<< (>>>) reducerToReduxReducer <<< runEffFn2

storeCreatorToReduxStoreCreator :: forall eff state action. StoreCreator eff state action -> ReduxStoreCreator eff state action
storeCreatorToReduxStoreCreator = mkEffFn2 <<< (<<<) ((<<<) (map storeToReduxStore)) <<< (>>>) reduxReducerToReducer

reduxStoreEnhancerToStoreEnhancer :: forall eff state action. ReduxStoreEnhancer eff state action -> StoreEnhancer eff state action
reduxStoreEnhancerToStoreEnhancer = (<<<) reduxStoreCreatorToStoreCreator <<< (>>>) storeCreatorToReduxStoreCreator

storeEnhancerToReduxStoreEnhancer :: forall eff state action. StoreEnhancer eff state action -> ReduxStoreEnhancer eff state action
storeEnhancerToReduxStoreEnhancer = (<<<) storeCreatorToReduxStoreCreator <<< (>>>) reduxStoreCreatorToStoreCreator
