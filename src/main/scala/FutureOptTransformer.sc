import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * based on talk: https://www.youtube.com/watch?v=jd5e71nFEZM
  *
  * Monads do not compose, as a result you could come up with nested structures like: Future[Option[Future[Option[A]]]
  * Dealing with this nested structures can make your code difficult to read.
  * Monad Transformers enables you to combine two Monads into a super Monad, which reduce boiler plating.
  *
  */
// THE MONAD INTERFACE
trait Monad[F[_]] { // Monad wants a type that is F which is a type constructor of something else
  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def flatMap[A, B](a: F[A])(fn: A => F[B]): F[B]
}

// In order to implement a for comprehension (see below) we need to write a companion object,
// since the map and  flatMap methods expect two argument, but in a for comprehension we give one argument.
object FutOpt extends Monad[FutOpt] {
  override def pure[A](a: A) = FutOpt(Future(Option(a)))

  override def map[A, B](fa: FutOpt[A])(f: A => B) =
    FutOpt(fa.value.map(optA => optA.map(f))) // wrap this 'fa.value.map(optA => optA.map(f))' in a FutOpt since thats the type you return according to the Monad interface


  override def flatMap[A, B](fa: FutOpt[A])(fn: A => FutOpt[B]) = {
    FutOpt(fa.value.flatMap {
      case Some(x) =>  fn(x).value
      case None =>  Future(None: Option[B])
    })
  }
}


case class FutOpt[A](value: Future[Option[A]]) {
   def map[B](f: A => B): FutOpt[B] = {
    FutOpt.map(this)(f)
  }

   def flatMap[B](f: A => FutOpt[B]): FutOpt[B] = {
    FutOpt.flatMap(this)(f)
  }
}

//TO USE
  case class User(name: String, gender: String)
  case class Address(street: String, postalCode: String, city: String, country: String)

  def getUser(name: String): Future[Option[User]] =
    if (name == "Bjorn") Future.successful(Some(User("Bjorn", "Male"))) else Future.successful(None)

  def getAddress(user: User): Future[Option[Address]] =
    if (user.name == "Bjorn") Future.successful(Some(Address("Bjorn", "StreetTest", "Amsterdam", "Netherlands")))
    else Future.successful(None)


 def getCity(username: String): FutOpt[String] = {
   for{
     user <-  FutOpt(getUser(username))
     address <- FutOpt(getAddress(user))
   } yield {
     println("FOUND ==> "+address.city)
     address.city
   }
 }


val resultSome = Await.result(getCity("Bjorn").value, Duration.Inf)
println(resultSome)
val resultNone = Await.result(getCity("Unknown").value, Duration.Inf) // ==> returns no result, since the user is not found
println(resultNone)


