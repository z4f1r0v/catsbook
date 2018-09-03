// type class
import cats.Monoid
import cats.Semigroup
import cats.Show
import cats.Eq
import cats.Functor
import cats.Monad
import cats.MonadError
import cats.Semigroupal
import cats.Foldable
import cats.Traverse
// data
import cats.data.Writer
import cats.data.Reader
import cats.data.State
import cats.data.OptionT
import cats.data.EitherT
import cats.data.ReaderT
import cats.data.WriterT
import cats.data.StateT
import cats.data.IdT

// type class instance
import cats.instances.int._
import cats.instances.boolean._
import cats.instances.string._
import cats.instances.option._
import cats.instances.long._
import cats.instances.function._
import cats.instances.future._
import cats.instances.list._
import cats.instances.try_._

// interface syntax
import cats.syntax.eq._ // for === and =!=
import cats.syntax.show._ // for show
import cats.syntax.semigroup._ // for |+|
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap
import cats.syntax.applicative._ // for pure
import cats.syntax.either._ // for asRight
import cats.syntax.applicativeError._ // for raiseError etc
import cats.syntax.monadError._// for ensure
import cats.syntax.writer._ // for tell
import cats.syntax.apply._ // for tupled and mapN
import cats.syntax.foldable._ // for combineAll and foldMap
import cats.syntax.traverse._ // for sequence and traverse
