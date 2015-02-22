package model

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Todo(id: Option[String], text: String, done: Boolean)

object Todo {
  implicit val todoWrites = new Writes[Todo] {
    def writes(todo: Todo) = Json.obj(
      "text" -> todo.text,
      "done" -> todo.done,
      "id" -> todo.id);

  }

  implicit val todoReads: Reads[Todo] = (
    (JsPath \ "id").readNullable[String] and
    (JsPath \ "text").read[String] and
    (JsPath \ "done").read[Boolean])(Todo.apply _)

}