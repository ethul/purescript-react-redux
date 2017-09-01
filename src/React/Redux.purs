module React.Redux
  ( ReduxReactClass
  , ReduxReactClass'
  , ReduxEffect
  , REDUX
  , Reducer
  , ReducerForeign
  , Enhancer
  , EnhancerForeign
  , Middleware
  , MiddlewareAPI
  , Store
  , connect
  , connect'
  , createProviderElement
  , createElement
  , createElement'
  , createStore
  , createStore'
  , dispatch
  , reducerOptic
  , applyMiddleware
  , fromEnhancerForeign
  ) where

import Prelude

import Data.Either (Either, either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Lens (Getter', Lens', Prism', matching, set, to, view)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (Tuple(..), fst)

import Control.Monad.Eff (Eff, kind Effect)

import React as React

import Unsafe.Coerce (unsafeCoerce)

type ReduxReactClass' state props = ReduxReactClass state Unit props

type ReducerForeign action state state' = Fn2 state action state'

type Enhancer eff action state = (Reducer action state state -> state -> Eff (ReduxEffect eff) (Store action state)) -> (Reducer action state state -> state -> Eff (ReduxEffect eff) (Store action state))

type EnhancerForeign action state = (Fn2 (ReducerForeign action state state) state (Store action state)) -> (Fn2 (ReducerForeign action state state) state (Store action state))

type Middleware eff action state result = MiddlewareAPI eff action state result -> (action -> Eff (ReduxEffect eff) action) -> action -> Eff (ReduxEffect eff) result

type MiddlewareAPI eff action state result = { getState :: Eff (ReduxEffect eff) state, dispatch :: action -> Eff (ReduxEffect eff) result }

type ReduxEffect eff = (redux :: REDUX | eff)

newtype Reducer action state state' = Reducer (action -> state -> state')

derive instance newtypeReducer :: Newtype (Reducer action state state') _

instance semigroupoidReducer :: Semigroupoid (Reducer action) where
  compose (Reducer f) (Reducer g) = Reducer (\action -> (f action) <<< (g action))

instance categoryReducer :: Category (Reducer action) where
  id = Reducer (const id)

createProviderElement :: forall action props state. Store action state -> ReduxReactClass' state props -> React.ReactElement
createProviderElement store reduxClass = React.createElement providerClass { store } [ createElement' reduxClass [] ]

connect :: forall props props' state. Getter' (Tuple state props) props' -> React.ReactClass props' -> ReduxReactClass state props props'
connect slens class_ = runFn3 connect_ Tuple (view slens) class_

connect' :: forall props state. Getter' state props -> React.ReactClass props -> ReduxReactClass' state props
connect' slens class_ = connect slens' class_
  where
  slens' :: Getter' (Tuple state Unit) props
  slens' = to (view slens <<< fst)

createElement :: forall state props props'. ReduxReactClass state props props' -> props -> Array React.ReactElement -> React.ReactElement
createElement reduxClass = React.createElement reactClass
  where
  reactClass :: React.ReactClass props
  reactClass = unsafeCoerce reduxClass

createElement' :: forall state props. ReduxReactClass' state props -> Array React.ReactElement -> React.ReactElement
createElement' reduxClass = createElement reduxClass unit

createStore :: forall eff action state. Reducer action state state -> state -> Enhancer eff action state -> Eff (ReduxEffect eff) (Store action state)
createStore = runFn3 createStore_

createStore' :: forall eff action state. Reducer action state state -> state -> Eff (ReduxEffect eff) (Store action state)
createStore' reducer state = createStore reducer state id

reducerOptic :: forall state state' action action'. Lens' state state' -> Prism' action action' -> Reducer action' state' state' -> Reducer action state state
reducerOptic lens prism k =
  wrap $ \action state ->
    let
      state' :: state'
      state' = view lens state

      action' :: Either action action'
      action' = matching prism action

    in either (const state) (\a -> set lens (unwrap k a state') state) action'

dispatch :: forall eff action props state. React.ReactThis props state -> action -> Eff (ReduxEffect eff) action
dispatch = runFn2 dispatch_

dispatch' :: forall eff action props state. React.ReactThis props state -> action -> Eff (ReduxEffect eff) Unit
dispatch' this action = void (dispatch this action)

foreign import data REDUX :: Effect

foreign import data Store :: Type -> Type -> Type

foreign import data ReduxReactClass :: Type -> Type -> Type -> Type

foreign import connect_ :: forall state props props'. Fn3 (state -> props -> Tuple state props) (Tuple state props -> props') (React.ReactClass props') (ReduxReactClass state props props')

foreign import dispatch_ :: forall eff action props state. Fn2 (React.ReactThis props state) action (Eff (ReduxEffect eff) action)

foreign import providerClass :: forall action state. React.ReactClass { store :: Store action state }

foreign import createStore_ :: forall eff action state. Fn3 (Reducer action state state) state (Enhancer eff action state) (Eff (ReduxEffect eff) (Store action state))

foreign import applyMiddleware :: forall eff action state result. Array (Middleware eff action state result) -> Enhancer eff action state

foreign import fromEnhancerForeign :: forall eff action state. EnhancerForeign action state -> Enhancer eff action state
