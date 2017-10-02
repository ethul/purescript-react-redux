module React.Redux
  ( ConnectClass
  , ConnectClass'
  , ConnectOptions
  , connect
  , connect_
  , createElement
  , createElement_
  , createProviderElement
  , createStore
  , createStore'
  , applyMiddleware
  , module Redux
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn3, runEffFn3)

import Data.Function.Uncurried (Fn2, Fn3, Fn4, mkFn2, mkFn3, runFn4)
import Data.Record.Class (class Subrow, unionMerge)

import Unsafe.Coerce (unsafeCoerce)

import React as React
import React.Redux.Internal as Internal
import React.Redux.Middleware (Middleware, MiddlewareAPI) as Redux
import React.Redux.Reducer (reducerFlipped) as Redux
import React.Redux.Types
  ( REDUX
  , ReduxEffect
  , Reducer
  , BaseDispatch
  , Dispatch
  , ReduxAction
  , ReduxAction'
  , ReduxReducer
  , ReduxBaseDispatch
  , ReduxDispatch
  , ReduxMiddlewareAPI
  , ReduxMiddleware
  , ReduxStoreCreator
  , ReduxStoreEnhancer
  , ReduxStore
  ) as Redux

-- | Connected class representation parameterized by `state`, `ownProps`, `props`, and `action`.
foreign import data ConnectClass :: Type -> Type -> Type -> Type -> Type

-- | Connect class type alias for a class with no `ownProps`.
type ConnectClass' state props action = ConnectClass state { } props action

-- | Options that can be passed on calling `connect`. Additional options may also be provided as needed.
type ConnectOptions state stateProps ownProps props options
  = ( pure :: Boolean
    , areStatesEqual :: Fn2 (Record state) (Record state) Boolean
    , areOwnPropsEqual :: Fn2 (Record ownProps) (Record ownProps) Boolean
    , areStatePropsEqual :: Fn2 (Record stateProps) (Record stateProps) Boolean
    , areMergedPropsEqual :: Fn2 (Record props) (Record props) Boolean
    , storeKey :: String
    | options
    )

-- | Redux connect function that depends on `ownProps`.
-- |
-- | Redux will invoke the mapping functions whenever the connected
-- | class receives updated `ownProps`.
connect
  :: forall eff state action stateProps dispatchProps ownProps stateDispatchProps props options options'
   . Subrow stateDispatchProps props
  => Union stateProps dispatchProps stateDispatchProps
  => Union stateDispatchProps ownProps props
  => Subrow options (ConnectOptions state stateProps ownProps props options')
  => (Record state -> Record ownProps -> Record stateProps)
  -> (Redux.BaseDispatch eff action -> Record ownProps -> Record dispatchProps)
  -> Record options
  -> React.ReactClass (Record props)
  -> ConnectClass (Record state) (Record ownProps) (Record props) action
connect stateToProps dispatchToProps options =
  runFn4 reduxConnect
    (mkFn2 stateToProps)
    (mkFn2 (dispatchToProps <<< Internal.reduxDispatchToDispatch))
    (mkFn3 mergeProps)
    options

-- | Redux connect function that does not depend on `ownProps`.
connect_
  :: forall eff state action stateProps dispatchProps props options options'
   . Union stateProps dispatchProps props
  => Union props () props
  => Subrow options (ConnectOptions state stateProps () props options')
  => (Record state -> Record stateProps)
  -> (Redux.BaseDispatch eff action -> Record dispatchProps)
  -> Record options
  -> React.ReactClass (Record props)
  -> ConnectClass' (Record state) (Record props) action
connect_ stateToProps dispatchToProps options =
  runFn4 reduxConnect_
    stateToProps
    (dispatchToProps <<< Internal.reduxDispatchToDispatch)
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

createElement
  :: forall state ownProps props action
   . ConnectClass (Record state) (Record ownProps) (Record props) action
  -> Record ownProps
  -> Array React.ReactElement
  -> React.ReactElement
createElement reduxClass =
  React.createElement reactClass
  where
  reactClass :: React.ReactClass (Record ownProps)
  reactClass = unsafeCoerce reduxClass

createElement_
  :: forall state props action
   . ConnectClass' (Record state) (Record props) action
  -> Array React.ReactElement
  -> React.ReactElement
createElement_ reduxClass = createElement reduxClass { }

createProviderElement
  :: forall eff state action
   . Redux.ReduxStore eff state action
  -> Array React.ReactElement
  -> React.ReactElement
createProviderElement store = React.createElement reduxProviderClass { store }

applyMiddleware
  :: forall eff state action result
   . Array (Redux.Middleware eff state action action result)
  -> Redux.ReduxStoreEnhancer eff state action
applyMiddleware = reduxApplyMiddleware <<< map Internal.middlewareToReduxMiddleware

createStore
  :: forall eff state action
   . Redux.Reducer action state
  -> state
  -> Redux.ReduxStoreEnhancer eff state action
  -> Eff (Redux.ReduxEffect eff) (Redux.ReduxStore eff state action)
createStore reducer = runEffFn3 reduxCreateStore (Internal.reducerToReduxReducer reducer)

createStore'
  :: forall eff state action
   . Redux.Reducer action state
  -> state
  -> Eff (Redux.ReduxEffect eff) (Redux.ReduxStore eff state action)
createStore' reducer state = createStore reducer state id

foreign import reduxApplyMiddleware
  :: forall eff state action a b
   . Array (Redux.ReduxMiddleware eff state action a b)
  -> Redux.ReduxStoreEnhancer eff state action

foreign import reduxCreateStore
  :: forall eff state action
   . EffFn3 (Redux.ReduxEffect eff)
            (Redux.ReduxReducer state action)
            state
            (Redux.ReduxStoreEnhancer eff state action)
            (Redux.ReduxStore eff state action)

foreign import reduxProviderClass
  :: forall eff state action
   . React.ReactClass { store :: Redux.ReduxStore eff state action }

foreign import reduxConnect
  :: forall eff state action stateProps dispatchProps ownProps props options
   . Fn4 (Fn2 (Record state) (Record ownProps) (Record stateProps))
         (Fn2 (Redux.ReduxDispatch eff action action) (Record ownProps) (Record dispatchProps))
         (Fn3 (Record stateProps) (Record dispatchProps) (Record ownProps) (Record props))
         (Record options)
         (React.ReactClass (Record props) -> ConnectClass (Record state) (Record ownProps) (Record props) action)

foreign import reduxConnect_
  :: forall eff state action stateProps dispatchProps props options
   . Fn4 (Record state -> Record stateProps)
         (Redux.ReduxDispatch eff action action -> Record dispatchProps)
         (Fn3 (Record stateProps) (Record dispatchProps) { } (Record props))
         (Record options)
         (React.ReactClass (Record props) -> ConnectClass' (Record state) (Record props) action)
