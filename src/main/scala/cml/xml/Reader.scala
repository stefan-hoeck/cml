package cml.xml

import cml.ValRes
import scalaz._, Scalaz._

trait ReaderFunctions {
  val noLogs: Logs = DList()

  def ap[A,B,C](r1: Reader[A,B])(r2: Reader[A,B ⇒ C]): Reader[A,C] = a ⇒ 
    (r1(a), r2(a)) match {
      case ((logs1, vb), (logs2, vbc)) ⇒ (logs1 ++ logs2, vb ap vbc)
    }

  def bulkReader[A,B,F[_]:Traverse](r: Reader[A,B]): Reader[F[A],F[B]] = {
    import reader.PairApplicative

    _ traverse r
  }

  def contramap[A,B,C](r: Reader[A,B])(f: C ⇒ A): Reader[C,B] =
    f andThen r

  def compose[A,B,C](r2: Reader[B,C])(r1: Reader[A,B]): Reader[A,C] =
    r1(_) match {
      case (logs1, vb) ⇒ vb fold (
        nel ⇒ (logs1, nel.failure[C]),
        r2(_) match { case (logs2, vc) ⇒ (logs1 ++ logs2, vc) }
      )
    }

  def composeO[A,B,C](r2: Reader[B,C])(r1: Reader[A,Option[B]])
    : Reader[A,Option[C]] = composeOO(map(r2)(_.some))(r1)

  def composeOO[A,B,C](r2: Reader[B,Option[C]])(r1: Reader[A,Option[B]])
    : Reader[A,Option[C]] = {
      val r2O: Reader[Option[B],Option[C]] = _.cata(r2, (noLogs, none.success))

      compose(r2O)(r1)
    }

  def liftV[A,B](f: A ⇒ ValRes[B]): Reader[A,B] = a ⇒ (noLogs, f(a))

  def log[A](log: Log): Reader[A,A] = logs(DList(log))

  def logs[A](logs: Logs): Reader[A,A] = a ⇒ (logs, a.success)

  def map[A,B,C](r: Reader[A,B])(f: B ⇒ C): Reader[A,C] = r(_) match {
    case (logs, vb) ⇒ (logs, vb map f)
  }

  def point[A,B](b: ⇒ B): Reader[A,B] = _ ⇒ (noLogs,b.success)
}

trait ReaderInstances {
  implicit val PairApplicative: Applicative[ReaderPair] =
    new Applicative[ReaderPair] {
      def point[A](a: ⇒ A) = (reader.noLogs, a.success)

      def ap[A,B](fa: ⇒ ReaderPair[A])(f: ⇒ ReaderPair[A ⇒ B]) = (fa, f) match {
        case ((logs1, va), (logs2, vab)) ⇒ (logs1 ++ logs2, va ap vab)
      }

      override def map[A,B](fa: ReaderPair[A])(f: A ⇒ B) = fa match {
        case (logs,va) ⇒ (logs, va map f)
      }
    }

  implicit def ReaderApplicative[R]: Applicative[({type λ[α]=Reader[R,α]})#λ] =
    new Applicative[({type λ[α]=Reader[R,α]})#λ] {
      def point[A](a: ⇒ A) = reader point a
      def ap[A,B](fa: ⇒ Reader[R,A])(f: ⇒ Reader[R,A ⇒ B]) = reader.ap(fa)(f)
      override def map[A,B](fa: Reader[R,A])(f: A ⇒ B) = reader.map(fa)(f)
    }
}

object reader extends ReaderFunctions with ReaderInstances

// vim: set ts=2 sw=2 et:
