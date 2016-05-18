package controllers

import play.api.mvc._

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.db.slick._
import slick.driver.JdbcProfile
import models.Tables._
import javax.inject.Inject
import scala.concurrent.Future
import slick.driver.H2Driver.api._

import play.api.libs.json._
import play.api.libs.functional.syntax._

object JsonController {
  // UsersRowをJSONに変換するためのWritesを定義
  // implicit val usersRowWritesWrites = (
  //   (__ \ "id"       ).write[Long]   and
  //   (__ \ "name"     ).write[String] and
  //   (__ \ "companyId").writeNullable[Int]
  // )(unlift(UsersRow.unapply))
  // Play2のJSONサポートが提供するDSLを使用してマッピングを定義していますが、DSLを使わずに以下のように記述することもできます。
  // これでもいける
  implicit val usersRowWritesFormat = new Writes[UsersRow]{
    def writes(user: UsersRow): JsValue = {
      Json.obj(
        "id"        -> user.id,
        "name"      -> user.name,
        "companyId" -> user.companyId
      )
    }
  }

  // ユーザ情報を受け取るためのケースクラス
  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])
  // JSONをUserFormに変換するためのReadsを定義
  implicit val userFormFormat = (
    (__ \ "id"       ).readNullable[Long] and
    (__ \ "name"     ).read[String]       and
    (__ \ "companyId").readNullable[Int]
  )(UserForm)
  // これでもいける系と思ったらいけなかった
  // implicit val userFormFormat = new Reads[UserForm]{
  //   def reads(js: JsValue): UserForm = {
  //     UserForm(
  //       id        = (js \ "id"       ).asOpt[Long],
  //       name      = (js \ "name"     ).as[String],
  //       companyId = (js \ "companyId").asOpt[Int]
  //     )
  //   }
  // }
}

class JsonController @Inject()(val dbConfigProvider: DatabaseConfigProvider) extends Controller
    with HasDatabaseConfigProvider[JdbcProfile] {

  /**
   * 一覧表示
   */
  // コンパニオンオブジェクトに定義したReads、Writesを参照するためにimport文を追加
  import JsonController._

  def list = Action.async { implicit rs =>
    // IDの昇順にすべてのユーザ情報を取得
    db.run(Users.sortBy(t => t.id).result).map { users =>
      // ユーザの一覧をJSONで返す
      Ok(Json.obj("users" -> users))
    }
  }

  /**
   * ユーザ登録
   */
  def create = Action.async(parse.json) { implicit rs =>
    rs.body.validate[UserForm].map { form =>
      // OKの場合はユーザを登録
      val user = UsersRow(0, form.name, form.companyId)
      db.run(Users += user).map { _ =>
        Ok(Json.obj("result" -> "success"))
      }
    }.recoverTotal { e =>
      // NGの場合はバリデーションエラーを返す
      Future {
        BadRequest(Json.obj("result" ->"failure", "error" -> JsError.toJson(e)))
      }
    }
  }

  /**
   * ユーザ更新
   */
    def update = Action.async(parse.json) { implicit rs =>
    rs.body.validate[UserForm].map { form =>
      // OKの場合はユーザ情報を更新
      val user = UsersRow(form.id.get, form.name, form.companyId)
      db.run(Users.filter(t => t.id === user.id.bind).update(user)).map { _ =>
        Ok(Json.obj("result" -> "success"))
      }
    }.recoverTotal { e =>
      // NGの場合はバリデーションエラーを返す
      Future {
        BadRequest(Json.obj("result" ->"failure", "error" -> JsError.toJson(e)))
      }
    }
  }

  /**
   * ユーザ削除
   */
  def remove(id: Long) = Action.async { implicit rs =>
    // ユーザを削除
    db.run(Users.filter(t => t.id === id.bind).delete).map { _ =>
      Ok(Json.obj("result" -> "success"))
    }
  }
}
