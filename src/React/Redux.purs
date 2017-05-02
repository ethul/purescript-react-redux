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
  , Spec
  , Render
  , GetInitialState
  , ComponentWillMount
  , ComponentDidMount
  , ComponentWillReceiveProps
  , ShouldComponentUpdate
  , ComponentWillUpdate
  , ComponentDidUpdate
  , ComponentWillUnmount
  , createClass
  , createClass'
  , createProviderElement
  , createElement
  , createElement'
  , createStore
  , createStore'
  , reducerOptic
  , spec
  , spec'
  , applyMiddleware
  , fromEnhancerForeign
  ) where

import React as React
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Either (Either, either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Lens (Getter', Lens', Prism', matching, set, to, view)
import Data.Tuple (Tuple(..), fst)
import Prelude (Unit, (>>=), (<<<), const, pure, id, unit)
import Unsafe.Coerce (unsafeCoerce)

type ReduxReactClass' state props = ReduxReactClass state Unit props

type Reducer action state = action -> state -> state

type ReducerForeign action state = Fn2 action state state

type Enhancer eff action state = (Reducer action state -> state -> Eff (ReduxEffect eff) (Store action state)) -> (Reducer action state -> state -> Eff (ReduxEffect eff) (Store action state))

type EnhancerForeign action state = (Fn2 (ReducerForeign action state) state (Store action state)) -> (Fn2 (ReducerForeign action state) state (Store action state))

type Middleware eff action state result = MiddlewareAPI eff action state result -> (action -> Eff (ReduxEffect eff) action) -> action -> Eff (ReduxEffect eff) result

type MiddlewareAPI eff action state result = { getState :: Eff (ReduxEffect eff) state, dispatch :: action -> Eff (ReduxEffect eff) result }

type ReduxEffect eff = (redux :: REDUX | eff)

type Render props state eff f action = (f action -> f action) -> React.Render props state eff

type GetInitialState props state eff f action = (f action -> f action) -> React.GetInitialState props state eff

type ComponentWillMount props state eff f action = (f action -> f action) -> React.ComponentWillMount props state eff

type ComponentDidMount props state eff f action = (f action -> f action) -> React.ComponentDidMount props state eff

type ComponentWillReceiveProps props state eff f action = (f action -> f action) -> React.ComponentWillReceiveProps props state eff

type ShouldComponentUpdate props state eff f action = (f action -> f action) -> React.ShouldComponentUpdate props state eff

type ComponentWillUpdate props state eff f action = (f action -> f action) -> React.ComponentWillUpdate props state eff

type ComponentDidUpdate props state eff f action = (f action -> f action) -> React.ComponentDidUpdate props state eff

type ComponentWillUnmount props state eff f action = (f action -> f action) -> React.ComponentWillUnmount props state eff

type Spec props state eff f action =
  { render :: Render props state eff f action
  , displayName :: String
  , getInitialState :: GetInitialState props state eff f action
  , componentWillMount :: ComponentWillMount props state eff f action
  , componentDidMount :: ComponentDidMount props state eff f action
  , componentWillReceiveProps :: ComponentWillReceiveProps props state eff f action
  , shouldComponentUpdate :: ShouldComponentUpdate props state eff f action
  , componentWillUpdate :: ComponentWillUpdate props state eff f action
  , componentDidUpdate :: ComponentDidUpdate props state eff f action
  , componentWillUnmount :: ComponentWillUnmount props state eff f action
  }

spec :: forall props state eff f action.  GetInitialState props state eff f action -> Render props state eff f action -> Spec props state eff f action
spec getInitialState render =
  { render: render
  , displayName: ""
  , getInitialState: getInitialState
  , componentWillMount: \_ _ -> pure unit
  , componentDidMount: \_ _ -> pure unit
  , componentWillReceiveProps: \_ _ _ -> pure unit
  , shouldComponentUpdate: \_ _ _ _ -> pure true
  , componentWillUpdate: \_ _ _ _ -> pure unit
  , componentDidUpdate: \_ _ _ _ -> pure unit
  , componentWillUnmount: \_ _ -> pure unit
  }

spec' :: forall props eff f action. Render props Unit eff f action -> Spec props Unit eff f action
spec' = spec (\_ _ -> pure unit)

createProviderElement :: forall action props state. Store action state -> ReduxReactClass' state props -> React.ReactElement
createProviderElement store reduxClass = React.createElement providerClass { store: store } [ createElement' reduxClass [] ]

createClass :: forall eff f action props props' state state'. MonadEff (ReduxEffect eff) f => Getter' (Tuple state props) props' -> Spec props' state' eff f action -> ReduxReactClass state props props'
createClass slens spec_ = runFn3 connect_ Tuple (view slens) reactClass
  where
  reactClass :: React.ReactClass props'
  reactClass =
    React.createClass { render: \this -> spec_.render (dispatch this) this
                      , displayName: spec_.displayName
                      , getInitialState: \this -> spec_.getInitialState (dispatch this) this
                      , componentWillMount: \this -> spec_.componentWillMount (dispatch this) this
                      , componentDidMount: \this -> spec_.componentDidMount (dispatch this) this
                      , componentWillReceiveProps: \this -> spec_.componentWillReceiveProps (dispatch this) this
                      , shouldComponentUpdate: \this -> spec_.shouldComponentUpdate (dispatch this) this
                      , componentWillUpdate: \this -> spec_.componentWillUpdate (dispatch this) this
                      , componentDidUpdate: \this -> spec_.componentDidUpdate (dispatch this) this
                      , componentWillUnmount: \this -> spec_.componentWillUnmount (dispatch this) this
                      }
    where
    dispatch :: React.ReactThis props' state' -> f action -> f action
    dispatch this action = action >>= liftEff <<< runFn2 dispatch_ this

createClass' :: forall eff f action props state. MonadEff (ReduxEffect eff) f => Getter' state props -> Spec props Unit eff f action -> ReduxReactClass' state props
createClass' slens spec_ = createClass slens' spec_
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

createStore :: forall eff action state. Reducer action state -> state -> Enhancer eff action state -> Eff (ReduxEffect eff) (Store action state)
createStore = runFn3 createStore_

createStore' :: forall eff action state. Reducer action state -> state -> Eff (ReduxEffect eff) (Store action state)
createStore' reducer state = createStore reducer state id

reducerOptic :: forall state state' action action'. Lens' state state' -> Prism' action action' -> Reducer action' state' -> Reducer action state
reducerOptic lens prism k action state = either (const state) (\a -> set lens (k a state') state) action'
  where
  state' :: state'
  state' = view lens state

  action' :: Either action action'
  action' = matching prism action

foreign import data REDUX :: Effect

foreign import data Store :: Type -> Type -> Type

foreign import data ReduxReactClass :: Type -> Type -> Type -> Type

foreign import connect_ :: forall state props props'. Fn3 (state -> props -> Tuple state props) (Tuple state props -> props') (React.ReactClass props') (ReduxReactClass state props props')

foreign import dispatch_ :: forall eff action props state. Fn2 (React.ReactThis props state) action (Eff (ReduxEffect eff) action)

foreign import providerClass :: forall action state. React.ReactClass { store :: Store action state }

foreign import createStore_ :: forall eff action state. Fn3 (Reducer action state) state (Enhancer eff action state) (Eff (ReduxEffect eff) (Store action state))

foreign import applyMiddleware :: forall eff action state result. Array (Middleware eff action state result) -> Enhancer eff action state

foreign import fromEnhancerForeign :: forall eff action state. EnhancerForeign action state -> Enhancer eff action state
