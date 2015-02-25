/*global define */

'use strict';

define([ 'angular' ], function() {

	var controllers = angular.module('myApp.controllers', [ 'myApp.daos' ]);

	controllers.controller('todoCtrl', [ '$scope', 'todoDao',
			function($scope, todoDao) {
		

				todoDao.getTodos().success(function(response) {
					$scope.todoList = response;
				});

				$scope.todoAdd = function() {
					var todo = {
						text : $scope.todoInput,
						done : false
					};
					todoDao.addTodo(todo).success(function(response){
						$scope.todoList.push(response);
					});

					$scope.todoInput = "";
				};

				$scope.remove = function() {
					var oldList = $scope.todoList;
					$scope.todoList = [];
					angular.forEach(oldList, function(x) {
						if (!x.done)
							$scope.todoList.push(x);
					});
				};
			} ]);

	return controllers;

});