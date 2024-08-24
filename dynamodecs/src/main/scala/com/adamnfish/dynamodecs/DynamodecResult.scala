package com.adamnfish.dynamodecs


sealed abstract class DynamodecError extends Exception

sealed abstract class DynamodecDecodeError extends DynamodecError
object DynamodecDecodeError:
  def apply(message: String): DynamodecDecodeError =
    new DynamodecDecodeError:
      override def getMessage: String = message

  def apply(message: String, cause: Throwable): DynamodecDecodeError =
    new DynamodecDecodeError:
      override def getMessage: String = message
      override def getCause: Throwable = cause

type DynamodecResult[A] = Either[DynamodecError, A]
