module React.Redux
  ( ReduxReactClass
  , ReduxReactClass'
  , ReduxEffect
  , REDUX
  , Reducer
  , Reducer'
  , ReducerForeign
  , Enhancer
  , EnhancerForeign
  , Middleware
  , MiddlewareAPI
  , Store
  , connect
  , connect_
  , createProviderElement
  , createProviderElement_
  , createElement
  , createElement_
  , createStore
  , createStore'
  , dispatch
  , dispatch_
  , reducerOptic
  , applyMiddleware
  , fromEnhancerForeign
  ) where

import Prelude

import Data.Either (Either, either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Lens (Getter', Lens', Prism', matching, set, to, view)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (Tuple(..), fst)

import Control.Monad.Eff (Eff, kind Effect)

import React as React

import Unsafe.Coerce (unsafeCoerce)

type ReduxReactClass' state props = ReduxReactClass state Unit props

type Reducer' action state = Reducer action state state

type ReducerForeign action state = Fn2 state action state

type Enhancer eff action state = (Reducer' action state -> state -> Eff (ReduxEffect eff) (Store action state)) -> (Reducer' action state -> state -> Eff (ReduxEffect eff) (Store action state))

type EnhancerForeign action state = (Fn2 (ReducerForeign action state) state (Store action state)) -> (Fn2 (ReducerForeign action state) state (Store action state))

type Middleware eff action state result = MiddlewareAPI eff action state result -> (action -> Eff (ReduxEffect eff) action) -> action -> Eff (ReduxEffect eff) result

type MiddlewareAPI eff action state result = { getState :: Eff (ReduxEffect eff) state, dispatch :: action -> Eff (ReduxEffect eff) result }

type ReduxEffect eff = (redux :: REDUX | eff)

newtype Reducer action state state' = Reducer (action -> state -> state')

derive instance newtypeReducer :: Newtype (Reducer action state state') _

instance semigroupoidReducer :: Semigroupoid (Reducer action) where
  compose (Reducer f) (Reducer g) = Reducer (\action -> (f action) <<< (g action))

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

createProviderElement :: forall action props props' state. Store action state -> ReduxReactClass state props props' -> props -> Array React.ReactElement -> React.ReactElement
createProviderElement store reduxClass props children = React.createElement providerClass { store } [ createElement reduxClass props children ]

createProviderElement_ :: forall action props state. Store action state -> ReduxReactClass' state props -> React.ReactElement
createProviderElement_ store reduxClass = React.createElement providerClass { store } [ createElement_ reduxClass [] ]

connect :: forall props props' state. Getter' (Tuple state props) props' -> React.ReactClass props' -> ReduxReactClass state props props'
connect slens class_ = runFn3 connectFn Tuple (view slens) class_

connect_ :: forall props state. Getter' state props -> React.ReactClass props -> ReduxReactClass' state props
connect_ slens class_ = connect slens' class_
  where
  slens' :: Getter' (Tuple state Unit) props
  slens' = to (view slens <<< fst)

createElement :: forall state props props'. ReduxReactClass state props props' -> props -> Array React.ReactElement -> React.ReactElement
createElement reduxClass = React.createElement reactClass
  where
  reactClass :: React.ReactClass props
  reactClass = unsafeCoerce reduxClass

createElement_ :: forall state props. ReduxReactClass' state props -> Array React.ReactElement -> React.ReactElement
createElement_ reduxClass = createElement reduxClass unit

createStore :: forall eff action state. Reducer' action state -> state -> Enhancer eff action state -> Eff (ReduxEffect eff) (Store action state)
createStore = runFn3 createStoreFn

createStore' :: forall eff action state. Reducer' action state -> state -> Eff (ReduxEffect eff) (Store action state)
createStore' reducer state = createStore reducer state id

reducerOptic :: forall state state' action action'. Lens' state state' -> Prism' action action' -> Reducer' action' state' -> Reducer' action state
reducerOptic lens prism k =
  wrap $ \action state ->
    let
      state' :: state'
      state' = view lens state

      action' :: Either action action'
      action' = matching prism action

    in either (const state) (\a -> set lens (unwrap k a state') state) action'

dispatch :: forall eff action props state. React.ReactThis props state -> action -> Eff (ReduxEffect eff) action
dispatch = runFn2 dispatchFn

dispatch_ :: forall eff action props state. React.ReactThis props state -> action -> Eff (ReduxEffect eff) Unit
dispatch_ this action = void (dispatch this action)

foreign import data REDUX :: Effect

foreign import data Store :: Type -> Type -> Type

foreign import data ReduxReactClass :: Type -> Type -> Type -> Type

foreign import connectFn :: forall state props props'. Fn3 (state -> props -> Tuple state props) (Tuple state props -> props') (React.ReactClass props') (ReduxReactClass state props props')

foreign import dispatchFn :: forall eff action props state. Fn2 (React.ReactThis props state) action (Eff (ReduxEffect eff) action)

foreign import providerClass :: forall action state. React.ReactClass { store :: Store action state }

foreign import createStoreFn :: forall eff action state. Fn3 (Reducer' action state) state (Enhancer eff action state) (Eff (ReduxEffect eff) (Store action state))

foreign import applyMiddleware :: forall eff action state result. Array (Middleware eff action state result) -> Enhancer eff action state

foreign import fromEnhancerForeign :: forall eff action state. EnhancerForeign action state -> Enhancer eff action state
