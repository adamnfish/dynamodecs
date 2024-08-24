package com.adamnfish.dynamodecs


/**
 * Utilities to help with traversing collections with error handling.
 *
 * These are included in the library to avoid a dependency on cats.
 */
object Utils:
  extension[A](aa: List[A])
    private[dynamodecs] def traverseE[L, B](f: A => Either[L, B]): Either[L, List[B]] =
      aa.foldRight[Either[L, List[B]]](Right(Nil)):
        (a, acc) =>
          for
            bs <- acc
            b <- f(a)
          yield b :: bs

  extension[A](aa: Seq[A])
    private[dynamodecs] def traverseE[L, B](f: A => Either[L, B]): Either[L, Seq[B]] =
      aa.foldLeft[Either[L, Seq[B]]](Right(Seq.empty)):
        (acc, a) =>
          for
            bs <- acc
            b <- f(a)
          yield bs :+ b

  extension[A](aa: Vector[A])
    private[dynamodecs] def traverseE[L, B](f: A => Either[L, B]): Either[L, Vector[B]] =
      aa.foldLeft[Either[L, Vector[B]]](Right(Vector.empty)):
        (acc, a) =>
          for
            bs <- acc
            b <- f(a)
          yield bs :+ b

  extension[A](aa: Set[A])
    private[dynamodecs] def traverseE[L, B](f: A => Either[L, B]): Either[L, Set[B]] =
      aa.foldLeft[Either[L, Set[B]]](Right(Set.empty)):
        (acc, a) =>
          for
            bs <- acc
            b <- f(a)
          yield bs + b

  extension[V](aa: Map[String, V])
    private[dynamodecs] def traverseE[L, B](f: V => Either[L, B]): Either[L, Map[String, B]] =
      aa.foldLeft[Either[L, Map[String, B]]](Right(Map.empty)):
        case (acc, (k, v)) =>
          for
            bs <- acc
            b <- f(v)
          yield bs + (k -> b)

  extension[K, V](aa: Map[K, V])
    private[dynamodecs] def traverseEKV[L, V2, K2](f: (K, V) => Either[L, (K2, V2)]): Either[L, Map[K2, V2]] =
      aa.foldRight[Either[L, Map[K2, V2]]](Right(Map.empty)):
        case ((k, v), acc) =>
          for
            bs <- acc
            k2v2 <- f(k, v)
            (k2, v2) = k2v2
          yield bs + (k2 -> v2)
