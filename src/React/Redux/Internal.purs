module React.Redux.Internal where

import Prelude

import Control.Monad.Eff.Uncurried (mkEffFn1, runEffFn1)

import Data.Function.Uncurried (mkFn2, runFn2)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (wrap, unwrap)
import Data.Nullable (Nullable, toMaybe, toNullable)

import Unsafe.Coerce (unsafeCoerce)

import React.Redux.Middleware (Middleware, MiddlewareAPI)

import React.Redux.Types
  ( BaseDispatch
  , Dispatch
  , Reducer
  , ReduxAction'
  , ReduxReducer
  , ReduxBaseDispatch
  , ReduxDispatch
  , ReduxMiddlewareAPI
  , ReduxMiddleware
  )

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

reduxReducerToReducer :: forall state action. ReduxReducer state action -> Reducer action state
reduxReducerToReducer reduxReducer = wrap (flip (runFn2 reduxReducer) <<< actionToReduxAction <<< toNullable <<< pure)

reducerToReduxReducer :: forall state action. Reducer action state -> ReduxReducer state action
reducerToReduxReducer reducer = mkFn2 \state -> maybe state (flip (unwrap reducer) state) <<< toMaybe <<< reduxActionToAction

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
  reduxMiddleware = dispatchToReduxDispatch <<< middleware' <<< reduxDispatchToDispatch
