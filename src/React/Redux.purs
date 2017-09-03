module React.Redux
  ( ConnectClass
  , ConnectClass'
  , ConnectProps(..)
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
  , _connectPropsState
  , _connectPropsDispatch
  , _connectPropsOwnProps
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
import Data.Function.Uncurried (Fn2, Fn3, mkFn3, runFn3)
import Data.Lens (Getter', Lens', Prism', (^.), lens, matching, set)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, wrap, unwrap)

import Control.Monad.Eff (Eff, kind Effect)

import React as React

import Unsafe.Coerce (unsafeCoerce)

type ConnectClass' state props = ConnectClass state Unit props

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

data ConnectProps eff action state ownProps = ConnectProps state (action -> Eff (ReduxEffect eff) action) ownProps

_connectPropsState :: forall eff action state ownProps. Lens' (ConnectProps eff action state ownProps) state
_connectPropsState = lens (\(ConnectProps state _ _) -> state) (\(ConnectProps _ dispatch ownProps) state -> ConnectProps state dispatch ownProps)

_connectPropsDispatch :: forall eff action state ownProps. Lens' (ConnectProps eff action state ownProps) (action -> Eff (ReduxEffect eff) action)
_connectPropsDispatch = lens (\(ConnectProps _ dispatch _) -> dispatch) (\(ConnectProps state _ ownProps) dispatch -> ConnectProps state dispatch ownProps)

_connectPropsOwnProps :: forall eff action state ownProps. Lens' (ConnectProps eff action state ownProps) ownProps
_connectPropsOwnProps = lens (\(ConnectProps _ _ ownProps) -> ownProps) (\(ConnectProps state dispatch _) ownProps -> ConnectProps state dispatch ownProps)

connect :: forall eff action state ownProps props. Getter' (ConnectProps eff action state ownProps) props -> React.ReactClass props -> ConnectClass state ownProps props
connect slens = runFn3 connectFn stateToProps dispatchToProps (mkFn3 mergeProps)
  where
  stateToProps :: state -> state
  stateToProps = id

  dispatchToProps :: EffFn1 (ReduxEffect eff) (ActionForeign action) (ActionForeign action) -> { dispatch :: action -> Eff (ReduxEffect eff) action }
  dispatchToProps dispatchForeign = { dispatch }
    where
    dispatch :: action -> Eff (ReduxEffect eff) action
    dispatch action = _.action <$> runEffFn1 dispatchForeign (makeActionForeign action)

  mergeProps :: state -> { dispatch :: action -> Eff (ReduxEffect eff) action } -> ownProps -> props
  mergeProps stateProps dispatchProps ownProps = ConnectProps stateProps dispatchProps.dispatch ownProps ^. slens

createElement :: forall state props props'. ConnectClass state props props' -> props -> Array React.ReactElement -> React.ReactElement
createElement reduxClass = React.createElement reactClass
  where
  reactClass :: React.ReactClass props
  reactClass = unsafeCoerce reduxClass

createElement_ :: forall state props. ConnectClass' state props -> Array React.ReactElement -> React.ReactElement
createElement_ reduxClass = createElement reduxClass unit

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
      state' = state ^. lens

      action' :: Either action action'
      action' = matching prism action

    in either (const state) (\a -> set lens (unwrap k a state') state) action'

foreign import data REDUX :: Effect

foreign import data Store :: Type -> Type -> Type

foreign import data ConnectClass :: Type -> Type -> Type -> Type

foreign import providerClass :: forall action state. React.ReactClass { store :: Store action state }

foreign import createStoreFn :: forall eff action state. Fn3 (Reducer' action state) state (Enhancer eff action state) (Eff (ReduxEffect eff) (Store action state))

foreign import applyMiddleware :: forall eff action state result. Array (Middleware eff action state result) -> Enhancer eff action state

foreign import fromEnhancerForeign :: forall eff action state. EnhancerForeign action state -> Enhancer eff action state

type ActionForeign action = { type :: String, action :: action }

foreign import makeActionForeign :: forall action. action -> ActionForeign action

foreign import connectFn
  :: forall eff action state stateProps dispatchProps ownProps props
   . Fn3 (state -> stateProps)
         (EffFn1 (ReduxEffect eff) (ActionForeign action) (ActionForeign action) -> dispatchProps)
         (Fn3 stateProps dispatchProps ownProps props)
         (React.ReactClass props -> ConnectClass state ownProps props)
