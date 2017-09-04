module React.Redux
  ( ConnectClass
  , ConnectClass'
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
  , createElement
  , createElement_
  , createProviderElement
  , createStore
  , createStore'
  , reducerOptic
  , applyMiddleware
  , fromEnhancerForeign
  ) where

import Prelude

import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)

import Data.Either (Either, either)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, mkFn3, runFn3)
import Data.Lens (Lens', Prism', matching, set, view)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Record.Class (class Subrow, unionMerge)

import Control.Monad.Eff (Eff, kind Effect)

import React as React

import Unsafe.Coerce (unsafeCoerce)

type ConnectClass' state props = ConnectClass state () props

type Reducer' action state = Reducer action state state

type ReducerForeign action state = Fn2 state action state

type Enhancer eff action state = (Reducer' action state -> state -> Eff (ReduxEffect eff) (Store action state)) -> (Reducer' action state -> state -> Eff (ReduxEffect eff) (Store action state))

type EnhancerForeign action state = (Fn2 (ReducerForeign action state) state (Store action state)) -> (Fn2 (ReducerForeign action state) state (Store action state))

type Middleware eff action state result = MiddlewareAPI eff action state result -> (action -> Eff (ReduxEffect eff) action) -> action -> Eff (ReduxEffect eff) result

type MiddlewareAPI eff action state result = { getState :: Eff (ReduxEffect eff) state, dispatch :: action -> Eff (ReduxEffect eff) result }

type ReduxEffect eff = (redux :: REDUX | eff)

type ActionForeign action = { type :: String, action :: action }

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

connect
  :: forall eff action state stateProps dispatchProps ownProps stateDispatchProps props
   . Subrow stateProps state
  => Subrow stateDispatchProps props
  => Union stateProps dispatchProps stateDispatchProps
  => Union stateDispatchProps ownProps props
  => (Record state -> Record ownProps -> Record stateProps)
  -> ((action -> Eff (ReduxEffect eff) action) -> Record ownProps -> Record dispatchProps)
  -> React.ReactClass (Record props)
  -> ConnectClass state ownProps props
connect stateToProps dispatchToProps =
  runFn3 connectFn (mkFn2 stateToProps) (mkFn2 dispatchToProps') (mkFn3 mergeProps)
  where
  dispatchToProps' :: EffFn1 (ReduxEffect eff) (ActionForeign action) (ActionForeign action) -> Record ownProps -> Record dispatchProps
  dispatchToProps' dispatchForeign = dispatchToProps dispatch
    where
    dispatch :: action -> Eff (ReduxEffect eff) action
    dispatch action = _.action <$> runEffFn1 dispatchForeign (makeActionForeign action)

  mergeProps :: Record stateProps -> Record dispatchProps -> Record ownProps -> Record props
  mergeProps stateProps dispatchProps ownProps = unionMerge ownProps (unionMerge dispatchProps stateProps)

createElement :: forall state ownProps props. ConnectClass state ownProps props -> Record ownProps -> Array React.ReactElement -> React.ReactElement
createElement reduxClass = React.createElement reactClass
  where
  reactClass :: React.ReactClass (Record ownProps)
  reactClass = unsafeCoerce reduxClass

createElement_ :: forall state props. ConnectClass' state props -> Array React.ReactElement -> React.ReactElement
createElement_ reduxClass = createElement reduxClass { }

createProviderElement :: forall action state. Store action state -> Array React.ReactElement -> React.ReactElement
createProviderElement store = React.createElement providerClass { store }

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

foreign import data REDUX :: Effect

foreign import data Store :: Type -> Type -> Type

foreign import data ConnectClass :: # Type -> # Type -> # Type -> Type

foreign import providerClass :: forall action state. React.ReactClass { store :: Store action state }

foreign import createStoreFn :: forall eff action state. Fn3 (Reducer' action state) state (Enhancer eff action state) (Eff (ReduxEffect eff) (Store action state))

foreign import applyMiddleware :: forall eff action state result. Array (Middleware eff action state result) -> Enhancer eff action state

foreign import fromEnhancerForeign :: forall eff action state. EnhancerForeign action state -> Enhancer eff action state

foreign import makeActionForeign :: forall action. action -> ActionForeign action

foreign import connectFn
  :: forall eff action state stateProps dispatchProps ownProps props
   . Fn3 (Fn2 (Record state) (Record ownProps) (Record stateProps))
         (Fn2 (EffFn1 (ReduxEffect eff) (ActionForeign action) (ActionForeign action)) (Record ownProps) (Record dispatchProps))
         (Fn3 (Record stateProps) (Record dispatchProps) (Record ownProps) (Record props))
         (React.ReactClass (Record props) -> ConnectClass state ownProps props)
