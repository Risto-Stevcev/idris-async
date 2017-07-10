module Control.Monad.AsyncJS_IO

import public Control.Monad.Async

%access public export

AsyncJS_IO : Type -> Type
AsyncJS_IO = Async (JS_IO ())


--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

Functor AsyncJS_IO where
  map f (MkAsync cb) = MkAsync (\cb' => cb (\x => cb' (f x)))

Applicative AsyncJS_IO where
  pure x = MkAsync $ \f => assert_total $
    foreign FFI_JS "setTimeout(%0, 0)" (JsFn (() -> JS_IO ()) -> JS_IO ()) (MkJsFn $ \_ => f x)
  (MkAsync cb1) <*> (MkAsync cb2) =
    MkAsync (\cb => cb1 (\f => cb2 (\x => cb (f x))))

Monad AsyncJS_IO where
  (MkAsync cb1) >>= f =
    MkAsync $ \cb => cb1 (\x => let MkAsync cb2 = f x in cb2 cb)


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

liftJS_IO : JS_IO a -> AsyncJS_IO a
liftJS_IO x = MkAsync (\cb => x >>= cb)

parallel : AsyncJS_IO a -> AsyncJS_IO a -> AsyncJS_IO a
parallel (MkAsync s1) (MkAsync s2) = MkAsync $ \cb => do
  s1 cb
  s2 cb

runAsync : AsyncJS_IO () -> JS_IO ()
runAsync (MkAsync f) = f (\_ => pure ())
