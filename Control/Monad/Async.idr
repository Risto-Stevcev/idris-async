module Control.Monad.Async

public export
data Async : Type -> Type -> Type where
  MkAsync : ((a -> b) -> b) -> Async b a
