'use strict';

var Redux = require('redux');

var ReactRedux = require('react-redux');

function startsWithActionType(actionForeign) {
  var index = actionForeign.type.indexOf('@@PURESCRIPT_REACT_REDUX');

  return index === 0;
}

exports.makeActionForeign = function makeActionForeign(action) {
  var constructorName = action.constructor && action.constructor.name ? action.constructor.name : 'UnknownConstructorName';

  var actionForeign = {
    type: '@@PURESCRIPT_REACT_REDUX/' + constructorName,
    action: action
  };

  return actionForeign;
};

exports.createStoreFn = function createStoreFn(reducer, state, enhancer){
  return function(){
    function reducerForeign(stateReducerForeign, actionForeign){
      var result = startsWithActionType(actionForeign) ? reducer(actionForeign.action)(stateReducerForeign) : stateReducerForeign;

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

exports.connectFn = function connectFn(mapStateToProps, mapDispatchToProps, mergeProps, options){
  return ReactRedux.connect(mapStateToProps, mapDispatchToProps, mergeProps, options);
};

exports.connectFn_ = exports.connectFn;

exports.applyMiddleware = function applyMiddleware(middlewares){
  var middlewaresForeign = middlewares.map(function(middleware){
    return function(middlewareAPIForeign){
      function getState(){
        return middlewareAPIForeign.getState();
      }

      function dispatch(action){
        return function(){
          var actionForeignResult = exports.makeActionForeign(action);

          var result = middlewareAPIForeign.dispatch(actionForeignResult);

          return result;
        };
      }

      var cont = middleware({ getState: getState, dispatch: dispatch })

      return function(nextForeign){
        return function(actionForeign){

          function next(action){
            return function(){
              var actionForeignResult = exports.makeActionForeign(action);

              var result = nextForeign(actionForeignResult);

              return result;
            };
          }

          var action = actionForeign.action;

          var result = cont(next)(action)();

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
