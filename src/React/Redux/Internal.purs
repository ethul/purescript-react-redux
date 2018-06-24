module React.Redux.Internal where

import Prelude

import Effect.Uncurried (mkEffectFn1, runEffectFn1)

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

reduxBaseDispatchToBaseDispatch :: forall action. ReduxBaseDispatch action -> BaseDispatch action
reduxBaseDispatchToBaseDispatch = (>>>) actionToReduxAction <<< (<<<) (map reduxActionToAction) <<< runEffectFn1

reduxDispatchToDispatch :: forall action result. ReduxDispatch action result -> Dispatch action result
reduxDispatchToDispatch = (>>>) actionToReduxAction <<< runEffectFn1

baseDispatchToReduxBaseDispatch :: forall action. Dispatch action action -> ReduxBaseDispatch action
baseDispatchToReduxBaseDispatch = mkEffectFn1 <<< (>>>) reduxActionToAction <<< (<<<) (map actionToReduxAction)

dispatchToReduxDispatch :: forall action result. Dispatch action result -> ReduxDispatch action result
dispatchToReduxDispatch = mkEffectFn1 <<< (>>>) reduxActionToAction

reduxMiddlewareApiToMiddlewareApi :: forall state action. ReduxMiddlewareAPI state action -> MiddlewareAPI state action
reduxMiddlewareApiToMiddlewareApi { getState, dispatch } = { getState, dispatch: reduxBaseDispatchToBaseDispatch dispatch }

middlewareApiToReduxMiddlewareApi :: forall state action. MiddlewareAPI state action -> ReduxMiddlewareAPI state action
middlewareApiToReduxMiddlewareApi { getState, dispatch } = { getState, dispatch: baseDispatchToReduxBaseDispatch dispatch }

reduxMiddlewareToMiddleware :: forall state action a b. ReduxMiddleware state action a b -> Middleware state action a b
reduxMiddlewareToMiddleware reduxMiddleware = wrap $ \middlewareApi dispatch -> reduxDispatchToDispatch (reduxMiddleware (middlewareApiToReduxMiddlewareApi middlewareApi) (dispatchToReduxDispatch dispatch))

middlewareToReduxMiddleware :: forall state action a b. Middleware state action a b -> ReduxMiddleware state action a b
middlewareToReduxMiddleware middleware reduxMiddlewareApi = reduxMiddleware
  where
  middleware' :: Dispatch action a -> Dispatch action b
  middleware' = unwrap middleware (reduxMiddlewareApiToMiddlewareApi reduxMiddlewareApi)

  reduxMiddleware :: ReduxDispatch action a -> ReduxDispatch action b
  reduxMiddleware = dispatchToReduxDispatch <<< middleware' <<< reduxDispatchToDispatch
