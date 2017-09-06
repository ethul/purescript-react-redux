module React.Redux
  ( ConnectClass
  , ConnectClass'
  , ConnectOptions
  , ReduxEffect
  , REDUX
  , Reducer
  , Reducer'
  , ReducerForeign
  , Dispatch
  , Dispatch'
  , Enhancer
  , EnhancerForeign
  , Middleware
  , MiddlewareAPI
  , Store
  , connect
  , connect_
  , createElement
  , createElement_
  , createProviderElement
  , createStore
  , createStore'
  , applyMiddleware
  , fromEnhancerForeign
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)

import Data.Function.Uncurried (Fn2, Fn3, Fn4, mkFn2, mkFn3, runFn3, runFn4)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Record.Class (class Subrow, unionMerge)

import React as React

import Unsafe.Coerce (unsafeCoerce)

type ConnectClass' state props action = ConnectClass state { } props action

type Reducer' action state = Reducer action state state

type ReducerForeign action state = Fn2 state action state

type Enhancer eff action state = (Reducer' action state -> state -> Eff (ReduxEffect eff) (Store action state)) -> (Reducer' action state -> state -> Eff (ReduxEffect eff) (Store action state))

type EnhancerForeign action state = (Fn2 (ReducerForeign action state) state (Store action state)) -> (Fn2 (ReducerForeign action state) state (Store action state))

type Middleware eff action state result = MiddlewareAPI eff action state result -> Dispatch' eff action -> action -> Eff (ReduxEffect eff) result

type MiddlewareAPI eff action state result = { getState :: Eff (ReduxEffect eff) state, dispatch :: Dispatch eff action result }

type ReduxEffect eff = (redux :: REDUX | eff)

type ActionForeign action = { type :: String, action :: action }

type Dispatch eff action action' = action -> Eff (ReduxEffect eff) action'

type Dispatch' eff action = Dispatch eff action action

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

type ConnectOptions state stateProps ownProps props
  = ( pure :: Boolean
    , areStatesEqual :: Fn2 (Record state) (Record state) Boolean
    , areOwnPropsEqual :: Fn2 (Record ownProps) (Record ownProps) Boolean
    , areStatePropsEqual :: Fn2 (Record stateProps) (Record stateProps) Boolean
    , areMergedPropsEqual :: Fn2 (Record props) (Record props) Boolean
    , storeKey :: String
    )

connect
  :: forall eff action state stateProps dispatchProps ownProps stateDispatchProps props options
   . Subrow stateDispatchProps props
  => Union stateProps dispatchProps stateDispatchProps
  => Union stateDispatchProps ownProps props
  => Subrow options (ConnectOptions state stateProps ownProps props)
  => (Record state -> Record ownProps -> Record stateProps)
  -> (Dispatch' eff action -> Record ownProps -> Record dispatchProps)
  -> Record options
  -> React.ReactClass (Record props)
  -> ConnectClass (Record state) (Record ownProps) (Record props) action
connect stateToProps dispatchToProps options =
  runFn4 connectFn (mkFn2 stateToProps)
                   (mkFn2 (dispatchToProps <<< dispatchForeignToDispatch))
                   (mkFn3 mergeProps)
                   options

connect_
  :: forall eff action state stateProps dispatchProps props options
   . Union stateProps dispatchProps props
  => Union props () props
  => Subrow options (ConnectOptions state stateProps () props)
  => (Record state -> Record stateProps)
  -> (Dispatch' eff action -> Record dispatchProps)
  -> Record options
  -> React.ReactClass (Record props)
  -> ConnectClass' (Record state) (Record props) action
connect_ stateToProps dispatchToProps options =
  runFn4 connectFn_ stateToProps
                    (dispatchToProps <<< dispatchForeignToDispatch)
                    (mkFn3 mergeProps)
                    options

mergeProps
  :: forall stateProps dispatchProps ownProps stateDispatchProps props
   . Subrow stateDispatchProps props
  => Union stateProps dispatchProps stateDispatchProps
  => Union stateDispatchProps ownProps props
  => Record stateProps
  -> Record dispatchProps
  -> Record ownProps
  -> Record props
mergeProps stateProps dispatchProps ownProps = unionMerge ownProps (unionMerge dispatchProps stateProps)

dispatchForeignToDispatch :: forall eff action. EffFn1 (ReduxEffect eff) (ActionForeign action) (ActionForeign action) -> Dispatch' eff action
dispatchForeignToDispatch dispatchForeign = map _.action <<< runEffFn1 dispatchForeign <<< makeActionForeign

createElement :: forall state ownProps props action. ConnectClass (Record state) (Record ownProps) (Record props) action -> Record ownProps -> Array React.ReactElement -> React.ReactElement
createElement reduxClass = React.createElement reactClass
  where
  reactClass :: React.ReactClass (Record ownProps)
  reactClass = unsafeCoerce reduxClass

createElement_ :: forall state props action. ConnectClass' (Record state) (Record props) action -> Array React.ReactElement -> React.ReactElement
createElement_ reduxClass = createElement reduxClass { }

createProviderElement :: forall action state. Store action state -> Array React.ReactElement -> React.ReactElement
createProviderElement store = React.createElement providerClass { store }

createStore :: forall eff action state. Reducer' action state -> state -> Enhancer eff action state -> Eff (ReduxEffect eff) (Store action state)
createStore = runFn3 createStoreFn

createStore' :: forall eff action state. Reducer' action state -> state -> Eff (ReduxEffect eff) (Store action state)
createStore' reducer state = createStore reducer state id

foreign import data REDUX :: Effect

foreign import data Store :: Type -> Type -> Type

foreign import data ConnectClass :: Type -> Type -> Type -> Type -> Type

foreign import providerClass :: forall action state. React.ReactClass { store :: Store action state }

foreign import createStoreFn :: forall eff action state. Fn3 (Reducer' action state) state (Enhancer eff action state) (Eff (ReduxEffect eff) (Store action state))

foreign import applyMiddleware :: forall eff action state result. Array (Middleware eff action state result) -> Enhancer eff action state

foreign import fromEnhancerForeign :: forall eff action state. EnhancerForeign action state -> Enhancer eff action state

foreign import makeActionForeign :: forall action. action -> ActionForeign action

foreign import connectFn
  :: forall eff action state stateProps dispatchProps ownProps props options
   . Fn4 (Fn2 (Record state) (Record ownProps) (Record stateProps))
         (Fn2 (EffFn1 (ReduxEffect eff) (ActionForeign action) (ActionForeign action)) (Record ownProps) (Record dispatchProps))
         (Fn3 (Record stateProps) (Record dispatchProps) (Record ownProps) (Record props))
         (Record options)
         (React.ReactClass (Record props) -> ConnectClass (Record state) (Record ownProps) (Record props) action)

foreign import connectFn_
  :: forall eff action state stateProps dispatchProps props options
   . Fn4 (Record state -> Record stateProps)
         (EffFn1 (ReduxEffect eff) (ActionForeign action) (ActionForeign action) -> Record dispatchProps)
         (Fn3 (Record stateProps) (Record dispatchProps) { } (Record props))
         (Record options)
         (React.ReactClass (Record props) -> ConnectClass' (Record state) (Record props) action)
