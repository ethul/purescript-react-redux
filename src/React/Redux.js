'use strict';

var Redux = require('redux');

var ReactRedux = require('react-redux');

function makeActionForeign(action) {
  var actionForeign = {
    type: '@@purescript',
    action: action
  };

  return actionForeign;
}

exports.createStore_ = function createStore_(reducer, state, enhancer){
  return function(){
    function reducerForeign(stateReducerForeign, actionForeign){
      var action = actionForeign.action;

      var result = action === undefined ? stateReducerForeign : reducer(action)(stateReducerForeign);

      return result;
    }

    function enhancerForeign(createStoreForeign){
      return function(reducerEnhancerForeign, stateEnhancerForeign){
        function createStore(reducerCreateStore){
          return function(stateCreateStore){
            return function(){
              var result = createStoreForeign(reducerCreateStore, stateCreateStore);

              return result;
            };
          };
        }

        return enhancer(createStore)(reducerEnhancerForeign)(stateEnhancerForeign)();
      };
    }

    return Redux.createStore(reducerForeign, state, enhancerForeign);
  };
};

exports.connect = function connect(stateToProps){
  return ReactRedux.connect(stateToProps);
};

exports.dispatch_ = function dispatch_(thisForeign, action){
  return function(){
    var actionForeign = makeActionForeign(action);

    var actionForeignResult = thisForeign.props.dispatch(actionForeign);

    return actionForeignResult.action;
  };
};

exports.applyMiddleware = function applyMiddleware(middlewares){
  var middlewaresForeign = middlewares.map(function(middleware){
    return function(middlewareAPIForeign){
      return function(nextForeign){
        return function(actionForeign){
          function getState(){
            return middlewareAPIForeign.getState();
          }

          function dispatch(action){
            return function(){
              var actionForeignResult = makeActionForeign(action);

              var result = middlewareAPIForeign.dispatch(actionForeignResult);

              return result;
            };
          }

          function next(action){
            return function(){
              var actionForeignResult = makeActionForeign(action);

              var result = nextForeign(actionForeignResult);

              return result;
            };
          }

          var middlewareAPI = {getState: getState, dispatch: dispatch};

          var action = actionForeign.action;

          var result = middleware(middlewareAPI)(next)(action)();

          return result;
        };
      };
    }
  });

  var middlewareEnhancerForeign = Redux.applyMiddleware.apply(Redux, middlewaresForeign);

  var result = exports.fromEnhancerForeign(middlewareEnhancerForeign);

  return result;
};

exports.fromEnhancerForeign = function fromEnhancerForeign(enhancerForeign){
  return function(createStore){
    return function(reducerForeign){
      return function(stateForeign){
        return function(){
          function createStoreForeign(reducer, state){
            var result = createStore(reducer)(state)();

            return result;
          };

          return enhancerForeign(createStoreForeign)(reducerForeign, stateForeign);
        };
      };
    };
  };
}

exports.providerClass = ReactRedux.Provider;
