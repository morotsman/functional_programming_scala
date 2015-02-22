package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import model.Todo
import java.util.concurrent.TimeoutException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import dao.TodoDao

object TodoController extends Controller {



  def listTodos = Action.async {
    TodoDao.getTodos().
      map(todos => Ok(Json.toJson(todos))).
      recover {
        case t: TimeoutException =>
          InternalServerError(t.getMessage)
      }
  }

  def createTodo = Action.async(BodyParsers.parse.json) { request =>
    val todo = request.body.validate[Todo]
    todo.fold(
      errors => {
        Future(BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toFlatJson(errors))))
      },
      todo => {
        TodoDao.insert(todo)
          .map { result =>
            Ok("Todo Created")
          }.recover {
            case t: TimeoutException =>
              InternalServerError(t.getMessage)
          }
      })
  }

  def updateTodo(id: String) = Action.async(BodyParsers.parse.json) { request =>
    val todo = request.body.validate[Todo]
    println(todo + " " + id)
    todo.fold(
      errors => {
        Future(BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toFlatJson(errors))))
      },
      todo => {
        TodoDao.update(id, todo)
          .map { result =>
            Ok("Todo Updated")
          }.recover {
            case t: TimeoutException =>
              InternalServerError(t.getMessage)
          }
      })
  }

}