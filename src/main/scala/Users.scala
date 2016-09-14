import Store._
import cats.data.Xor
import cats.free.Free
import cats.free.Free.liftF
import cats.{Id, ~>}

import scala.collection.mutable

object Users {

  case class User(name: String)

  sealed trait UserOperationResult extends Product with Serializable

  final case object UserExists extends UserOperationResult

  sealed trait UserServiceA[A]

  case class CreateUser(name: User) extends UserServiceA[UserExists.type Xor Unit]

  case class FindUser(name: String) extends UserServiceA[Option[User]]

  type UserService[A] = Free[UserServiceA, A]

  def create(user: User): UserService[UserExists.type Xor Unit] = {

    def returnFailure = Xor.left[UserExists.type, Unit](UserExists)

    def returnSuccess = Xor.right[UserExists.type, Unit](CreateUser(user))

    find(user.name).map { (u: Option[User]) ⇒
      if (u.isDefined) returnFailure else returnSuccess
    }
  }

  def find(name: String): UserService[Option[User]] =
    liftF[UserServiceA, Option[User]](FindUser(name))

  // test interpreter that records operations and uses a map
  class TestInterpreter extends (UserServiceA ~> Id) {
    var ops: Vector[UserServiceA[_]] = Vector.empty
    val kvs = mutable.Map.empty[String, Any]

    def apply[A](fa: UserServiceA[A]): Id[A] =
      fa match {
        case c@CreateUser(user) =>
          ops = ops :+ c
          if (kvs.contains(user.name))
            Xor.left[UserExists.type, Unit](UserExists)
          else {
            kvs(user.name) = user
            Xor.right[UserExists.type, Unit](())
          }
        case f@FindUser(name) =>
          ops = ops :+ f
          kvs.get(name).asInstanceOf[A]
      }
  }

  // interpreter based on Store
  def storeInterpreter: UserServiceA ~> KVStore =
    new (UserServiceA ~> KVStore) {
      def apply[A](fa: UserServiceA[A]): KVStore[A] =
        fa match {
          case CreateUser(user) =>
            put(user.name, user).map(_ ⇒ Xor.right[UserExists.type, Unit](()))
          case FindUser(name) =>
            get[User](name)
        }
    }
}