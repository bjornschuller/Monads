import cats.data.{EitherT, OptionT}
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
/**
Monad transformers allow us to squash together Monads, creating one monad where we previously had
two or more. With this transformed monad we can avoid nested calls to flatMap. Cats provides a
library of such transformers: EitherT for composing Either with other monads,
OptionT for composing Option, and so on (Advanced Scala with Cats, page 132).
  * PLEASE NOTE THAT:
  * The combined map and flatMap methods allow us to use both component monads without having to
  * recursively unpack and repack values at each stage in the computation.
  * In other words it cut through two layers of abstrac􏰀tion
  *
*/

case class User(name: String, gender: String)
case class Address(street: String, postalCode: String, city: String, country: String)

def getUser(name: String): Future[Option[User]] =
  if (name == "Bjorn") Future.successful(Some(User("Bjorn", "Male"))) else Future.successful(None)

def getAddress(user: User): Future[Option[Address]] =
  if (user.name == "Bjorn") Future.successful(Some(Address("Bjorn", "StreetTest", "Amsterdam", "Netherlands")))
  else Future.successful(None)

/** OptionT is defined within cats
  * OptionT[F[_], A] --> you can provide it with any Monad (F) that is a type constructor of something else,
  * where A is the type of the inner value in the nested structure.
  * So we can apply this in a nested Future[Option[String]] structure but also a nested Future[List[String]] structure and so on..
  *
  *
  */
def getCity(username: String): OptionT[Future, String] = {
  for{
    user <-  OptionT(getUser(username))
    address <- OptionT(getAddress(user))
  } yield {
    println("FOUND ==> "+address.city)
    address.city
  }
}

val resultSome = Await.result(getCity("Bjorn").value, Duration.Inf)
println(resultSome)
val resultNone = Await.result(getCity("Unknown").value, Duration.Inf) // ==> returns no result, since the user is not found
println(resultNone)


/**
  * remark: in real life scenario it could be that one method returns a Future[Option[String]] and the second method returns a
  * Future[Int]. To fix this: you need to make the types fit into the structure you working with.
  * For instance you can use OptionT.liftF when you are missing the Option type in between or use the
  * OptionT.fromOption when you want to wrap the F type around the Option type.
  */

def getAge(user: User): Future[Int] = Future.successful(101)

def getAgeOfUser(username: String): OptionT[Future, Int] = {
  for{
    user <-  OptionT(getUser(username))
    age <- OptionT.liftF(getAge(user))
  } yield {
    age
  }
}

val ageResult = Await.result(getAgeOfUser("Bjorn").value, Duration.Inf)
println(ageResult)

/**
  * Instead of just return None and/or fail the Either type can give you detailed errors (30:27).
  * So you get more feedback about the thing thats goes 'wrong'.
  *
  * EitherT[F[_], A, B] => left is A, right is B, and F is the wrapping type. In other words,
  * EitherT[F[_], A, B] is a lightweight wrapper for F[Either[A, B]]
  * that makes it easy to compose Eithers and Fs together (see: https://typelevel.org/cats/datatypes/eithert.html).
  * As an example see code below:
  *
  */
case class KnownError(msg: String)

def getUserEither(name: String): EitherT[Future, KnownError, User] =
  if (name == "Bjorn") EitherT.right(Future.successful(User("Bjorn", "Male")))
  else EitherT.left(Future.successful(KnownError("User not found")))

def getAddressEither(user: User): EitherT[Future, KnownError, Address] =
  if (user.name == "Bjorn") EitherT.right(Future.successful(Address("Bjorn", "StreetTest", "Amsterdam", "Netherlands")))
  else EitherT.left(Future.successful(KnownError(s"Address for $user not found")))



def getCityEitherT(username: String): EitherT[Future, KnownError, String] = {
  for{
    user <-  getUserEither(username)
    address <- getAddressEither(user)
  } yield {
    println("FOUND ==> "+address.city)
    address.city
  }
}

val resultRight = Await.result(getCityEitherT("Bjorn").value, Duration.Inf)
println(resultRight)
val resultLeft = Await.result(getCityEitherT("Unknown").value, Duration.Inf) // ==> returns no result, since the user is not found
println(resultLeft)


























