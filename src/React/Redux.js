'use strict';

var Redux = require('redux');

var ReactRedux = require('react-redux');

exports.reduxCreateStore = function reduxCreateStore(reducer, state, enhancer){
  return Redux.createStore(reducer, state, enhancer);
};

exports.reduxApplyMiddleware = function reduxApplyMiddleware(middleware){
  return Redux.applyMiddleware(middleware);
};

exports.reduxConnect = function reduxConnect(mapStateToProps, mapDispatchToProps, mergeProps, options){
  return ReactRedux.connect(mapStateToProps, mapDispatchToProps, mergeProps, options);
};

exports.reduxConnect_ = exports.reduxConnect;

exports.reduxProviderClass = ReactRedux.Provider;
