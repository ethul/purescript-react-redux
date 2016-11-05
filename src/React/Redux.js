'use strict';

var Redux = require('redux');

var ReactRedux = require('react-redux');

function createStore_(reducer, state) {
  return function(){
    function reducer_(state, action){
      var action_ = action.action;
      return action_ === undefined ? state : reducer(action_)(state);
    };

    return Redux.createStore(reducer_, state);
  };
}
exports.createStore_ = createStore_;

function connect(stateToProps) {
  return ReactRedux.connect(stateToProps);
}
exports.connect = connect;

function dispatch_(this_, action){
  return function(){
    var action_ = {
      type: '@@purescript',
      action: action
    };

    var result = this_.props.dispatch(action_);

    return result.action;
  };
}
exports.dispatch_ = dispatch_;

exports.providerClass = ReactRedux.Provider;
