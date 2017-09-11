module React.Redux.Types where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)

import React.Redux.Reducer as Reducer

-- | Effect type for Redux.
foreign import data REDUX :: Effect

type ReduxEffect eff = (redux :: REDUX | eff)

type Reducer action state = Reducer.Reducer action state state

type BaseDispatch eff action = Dispatch eff action action

type Dispatch eff action result = action -> Eff (ReduxEffect eff) result

type Store eff state action
  = { dispatch :: BaseDispatch eff action
    , getState :: Eff (ReduxEffect eff) state
    , subscribe :: Eff (ReduxEffect eff) Unit -> Eff (ReduxEffect eff) Unit
    , replaceReducer :: Reducer action state -> Eff (ReduxEffect eff) Unit
    }

type StoreCreator eff state action = Reducer action state -> state -> Eff (ReduxEffect eff) (Store eff state action)

type StoreEnhancer eff state action = StoreCreator eff state action -> StoreCreator eff state action
