/*global require, requirejs */

'use strict';

requirejs.config({
  paths: {
    'angular': ['../lib/angularjs/angular']
  },
  shim: {
    'angular': {
      exports : 'angular'
    }
  }
});

require(['angular', './controllers', './daos'],
  function(angular, controllers) {

    // Declare app level module which depends on filters, and services

    var app = angular.module('myApp', [ 'myApp.controllers']);
    
    var $html = angular.element(document.getElementsByTagName('html')[0]);

	angular.element().ready(function() {
		$html.addClass('ng-app');
		angular.bootstrap($html, [app['name']]);
	});    

});