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
        case t: TimeoutException => InternalServerError(t.getMessage)
        case u                   => InternalServerError("Unknown server exception:" + u.getMessage)
      }
  }

  def createTodo = Action.async(BodyParsers.parse.json) { request =>
    request.body.validate[Todo].map {
      todo =>
        {
          TodoDao.insert(todo)
            .map { result =>
              result match {
                case t: Todo      => Ok(Json.toJson(t))
                case e: Throwable => convertException(e)
              }
            }
        }
    }.recoverTotal {
      errors => Future.successful(BadRequest("Bad request: " + JsError.toFlatJson(errors)))
    }
  }

  def updateTodo(id: String) = Action.async(BodyParsers.parse.json) { request =>
    request.body.validate[Todo].map {
      todo =>
        {
          TodoDao.update(id, todo)
            .map { result =>
              result match {
                case t: Todo      => Ok(Json.toJson(t))
                case e: Throwable => convertException(e)
              }
            }
        }
    }.recoverTotal {
      errors => Future.successful(BadRequest("Bad request: " + JsError.toFlatJson(errors)))
    }
  }

  def convertException(e: Throwable) = e match {
    case t: TimeoutException => InternalServerError(t.getMessage)
    case u                   => InternalServerError("Unknown server exception:" + u.getMessage)
  }

}