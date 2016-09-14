import cats.Id
import cats.free.Free
import Users.UserServiceA
import Store._
import Users._

object Test extends App {

  val program: Free[UserServiceA, List[Option[User]]] =
    for {
      _ ← create(User("Channing"))
      _ ← create(User("Lance"))
      c ← find("Channing")
      l ← find("Channing")
    } yield List(c, l)

  // run test program
  val userTestCompiler = new TestInterpreter
  val testUsersProgram = program.foldMap(userTestCompiler)
  println(userTestCompiler.ops)

  // run real program
  val runUsersProgram: KVStore[List[Option[User]]] = program.foldMap(storeInterpreter)
  val runStoreProgram: Id[List[Option[User]]] = runUsersProgram.foldMap(dbInterpreter)

  println(runStoreProgram)
}
