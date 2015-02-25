package dao

import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.api._
import reactivemongo.bson._
import scala.concurrent.ExecutionContext.Implicits.global
import model.Todo
import scala.concurrent.Future

object TodoDao {

  val db = ReactiveMongoPlugin.db(play.api.Play.current)
  private val collection = db("todo")

  def getTodos(): Future[List[model.Todo]] = {
    collection.
      find(BSONDocument()).
      cursor[BSONDocument].
      collect[List]().
      map(todos => todos.map(doc => Todo(doc.getAs[BSONObjectID]("_id").map(id => id.stringify), doc.getAs[String]("text").get, doc.getAs[Boolean]("done").get)))
  }

  def insert(todo: Todo) = {
    val id = BSONObjectID.generate
    collection.
      insert(BSONDocument(("_id", id), ("text", todo.text), ("done", todo.done)))
      .map { result =>
        Todo(Some(id.stringify), todo.text, todo.done)
      }.recover {
        case t: Throwable => throw t
      }
  }

  def update(id: String, todo: Todo) = {
    val selector = BSONDocument("_id" -> BSONObjectID(id))
    val modifier = BSONDocument("$set" -> BSONDocument("text" -> todo.text, "done" -> todo.done))
    collection.update(selector, modifier)
      .map { result =>
        Todo(Some(id), todo.text, todo.done)
      }.recover {
        case t: Throwable => throw t
      }
  }

}