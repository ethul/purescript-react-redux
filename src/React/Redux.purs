module React.Redux
  ( ReduxReactClass
  , Effects
  , REDUX
  , Reducer
  , Store
  , createClass
  , createElement
  , createStore
  , reducerOptic

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
  , spec
  , spec'
  ) where

import Prelude (Unit, (<<<), (>>=), const, pure, unit)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.Either (Either, either)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Lens (Getter', Lens', Prism', matching, set, view)

import Unsafe.Coerce (unsafeCoerce)

import React as React

type Reducer action state = action -> state -> state

type Effects eff = (redux :: REDUX | eff)

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

spec ::
  forall props state eff f action.
    GetInitialState props state eff f action ->
    Render props state eff f action ->
    Spec props state eff f action
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

createClass :: forall props state eff f action state'. MonadEff (Effects eff) f => Getter' state' props -> Spec props state eff f action -> ReduxReactClass state' props
createClass lens spec_ = connect (view lens) reactClass
  where
  reactClass :: React.ReactClass props
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
    dispatch :: React.ReactThis props state -> f action -> f action
    dispatch this action = action >>= liftEff <<< runFn2 dispatch_ this

createElement :: forall props action state'. Store action state' -> ReduxReactClass state' props -> React.ReactElement
createElement store reduxClass =
  React.createElement providerClass { store: store } [ reduxEl ]
  where
  reduxEl :: React.ReactElement
  reduxEl = React.createElement (unsafeCoerce reduxClass) (unsafeCoerce unit) []

createStore :: forall eff action state. Reducer action state -> state -> Eff (Effects eff) (Store action state)
createStore = runFn2 createStore_

reducerOptic :: forall state state' action action'. Lens' state state' -> Prism' action action' -> Reducer action' state' -> Reducer action state
reducerOptic lens prism k action state = either (const state) (\a -> set lens (k a state') state) action'
  where
  state' :: state'
  state' = view lens state

  action' :: Either action action'
  action' = matching prism action

foreign import data REDUX :: !

foreign import data Store :: * -> * -> *

foreign import data ReduxReactClass :: * -> * -> *

foreign import connect :: forall state' props. (state' -> props) -> React.ReactClass props -> ReduxReactClass state' props

foreign import dispatch_ :: forall eff props action state. Fn2 (React.ReactThis props state) action (Eff (Effects eff) action)

foreign import providerClass :: forall action state'. React.ReactClass { store :: Store action state' }

foreign import createStore_ :: forall eff action state. Fn2 (Reducer action state) state (Eff (Effects eff) (Store action state))
