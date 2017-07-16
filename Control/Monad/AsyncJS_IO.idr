module Control.Monad.AsyncJS_IO

import public Control.Monad.Async
import Data.JSError


%access public export

AsyncJS_IO : Type -> Type
AsyncJS_IO = Async (JS_IO ()) JSError


--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

Functor AsyncJS_IO where
  map f (MkAsync cb) = MkAsync (\error => \success => cb error (\x => success (f x)))

mutual
  Applicative AsyncJS_IO where
    pure x = MkAsync (\error => \success => success x)
    x <*> y = x >>= (\f => f <$> y)

  Monad AsyncJS_IO where
    (MkAsync cb) >>= f = MkAsync (\error => \success => cb error (\x => let (MkAsync cb') = (f x) in cb' error success))


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

liftJS_IO : JS_IO a -> AsyncJS_IO a
liftJS_IO x = MkAsync (\err => \cb => x >>= cb)

attempt : AsyncJS_IO a -> AsyncJS_IO (Either JSError a)
attempt (MkAsync cb) = MkAsync $ \_ => \success => cb (\e => success $ Left e) (\s => success $ Right s)

throwAsync : AsyncJS_IO a -> AsyncJS_IO ()
throwAsync (MkAsync cb) = MkAsync (\_ => \_ => cb (\err => throwErr err) (\_ => pure ()))
  where
    throwErr : JSError -> JS_IO ()
    throwErr (MkJSError p) = foreign FFI_JS "(function() { if (%0 instanceof Error) throw %0 })()" (Ptr -> JS_IO ()) p

parallel : AsyncJS_IO a -> AsyncJS_IO a -> AsyncJS_IO a
parallel (MkAsync s1) (MkAsync s2) = MkAsync $ \err => \cb => do
  s1 err cb
  s2 err cb

runAsync : AsyncJS_IO () -> JS_IO ()
runAsync (MkAsync f) = f (\_ => pure ()) (\_ => pure ())
