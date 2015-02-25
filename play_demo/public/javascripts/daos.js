/*global define */

'use strict';

define([ 'angular' ], function(angular) {

	var myModule = angular.module('myApp.daos', []);

	myModule.factory('todoDao', [ '$http', function($http) {

		return {
			getTodos : function() {
				return $http.get("todos");
			},
			addTodo: function(todo){
				return $http.post("todos", todo);
			}

		};
	} ]);

});