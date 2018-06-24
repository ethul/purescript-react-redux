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

import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)

import Data.Function.Uncurried (Fn2, Fn3, Fn4, mkFn2, mkFn3, runFn4)

import Prim.Row (class Union)

import Unsafe.Coerce (unsafeCoerce)

import React as React
import React.Redux.Internal as Internal
import React.Redux.Middleware (Middleware, MiddlewareAPI) as Redux
import React.Redux.Reducer (reducerFlipped) as Redux
import React.Redux.Types
  ( Reducer
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
  :: forall state action stateProps dispatchProps ownProps stateDispatchProps props options options' options''
   . Union stateProps dispatchProps stateDispatchProps
  => Union stateDispatchProps ownProps props
  => Union options options'' (ConnectOptions state stateProps ownProps props options')
  => (Record state -> Record ownProps -> Record stateProps)
  -> (Redux.BaseDispatch action -> Record ownProps -> Record dispatchProps)
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
  :: forall state action stateProps dispatchProps props options options' options''
   . Union stateProps dispatchProps props
  => Union props () props
  => Union options options'' (ConnectOptions state stateProps () props options')
  => (Record state -> Record stateProps)
  -> (Redux.BaseDispatch action -> Record dispatchProps)
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
   . Union stateProps dispatchProps stateDispatchProps
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
  React.unsafeCreateElement reactClass
  where
  reactClass :: React.ReactClass (Record (children :: React.Children | ownProps))
  reactClass = unsafeCoerce reduxClass

createElement_
  :: forall state props action
   . ConnectClass' (Record state) (Record props) action
  -> Array React.ReactElement
  -> React.ReactElement
createElement_ reduxClass = createElement reduxClass { }

createProviderElement
  :: forall state action
   . Redux.ReduxStore state action
  -> Array React.ReactElement
  -> React.ReactElement
createProviderElement store = React.createElement reduxProviderClass { store }

applyMiddleware
  :: forall state action result
   . Array (Redux.Middleware state action action result)
  -> Redux.ReduxStoreEnhancer state action
applyMiddleware = reduxApplyMiddleware <<< map Internal.middlewareToReduxMiddleware

createStore
  :: forall state action
   . Redux.Reducer action state
  -> state
  -> Redux.ReduxStoreEnhancer state action
  -> Effect (Redux.ReduxStore state action)
createStore reducer = runEffectFn3 reduxCreateStore (Internal.reducerToReduxReducer reducer)

createStore'
  :: forall state action
   . Redux.Reducer action state
  -> state
  -> Effect (Redux.ReduxStore state action)
createStore' reducer state = createStore reducer state identity

foreign import reduxApplyMiddleware
  :: forall state action a b
   . Array (Redux.ReduxMiddleware state action a b)
  -> Redux.ReduxStoreEnhancer state action

foreign import reduxCreateStore
  :: forall state action
   . EffectFn3
       (Redux.ReduxReducer state action)
       state
       (Redux.ReduxStoreEnhancer state action)
       (Redux.ReduxStore state action)

foreign import reduxProviderClass
  :: forall state action
   . React.ReactClass { children :: React.Children, store :: Redux.ReduxStore state action }

foreign import reduxConnect
  :: forall state action stateProps dispatchProps ownProps props options
   . Fn4 (Fn2 (Record state) (Record ownProps) (Record stateProps))
         (Fn2 (Redux.ReduxDispatch action action) (Record ownProps) (Record dispatchProps))
         (Fn3 (Record stateProps) (Record dispatchProps) (Record ownProps) (Record props))
         (Record options)
         (React.ReactClass (Record props) -> ConnectClass (Record state) (Record ownProps) (Record props) action)

foreign import reduxConnect_
  :: forall state action stateProps dispatchProps props options
   . Fn4 (Record state -> Record stateProps)
         (Redux.ReduxDispatch action action -> Record dispatchProps)
         (Fn3 (Record stateProps) (Record dispatchProps) { } (Record props))
         (Record options)
         (React.ReactClass (Record props) -> ConnectClass' (Record state) (Record props) action)

-- | Temporarily including `unionMerge` to publish to Pursuit.
-- | See purescript/purescript-record#7
-- |
unionMerge :: forall l r u. Union r l u => Record l -> Record r -> Record u
unionMerge = unsafeMerge

foreign import unsafeMerge
  :: forall l r u
  .  Record l
  -> Record r
  -> Record u
