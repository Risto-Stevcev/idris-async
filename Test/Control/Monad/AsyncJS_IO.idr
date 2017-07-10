module Test.Control.Monad.AsyncJS_IO

import Control.Monad.AsyncJS_IO

%lib Node "fs"

writeFile' : String -> String -> (() -> JS_IO ()) -> JS_IO ()
writeFile' path contents cb =
  foreign FFI_JS "fs.writeFile(%0, %1, function(err, data) { %2(data) })"
    (String -> String -> JsFn (() -> JS_IO ()) -> JS_IO ()) path contents (MkJsFn cb)

writeFile : String -> String -> AsyncJS_IO ()
writeFile path contents = MkAsync $ \f => writeFile' path contents (\x => f x)


readFile' : String -> (String -> JS_IO ()) -> JS_IO ()
readFile' path cb = foreign FFI_JS "fs.readFile(%0, 'utf8', function(err, data) { %1(data) })"
  (String -> JsFn (String -> JS_IO ()) -> JS_IO ()) path (MkJsFn cb)

readFile : String -> AsyncJS_IO String
readFile path = MkAsync $ \f => (readFile' path (\s => f s))


consoleLog : String -> AsyncJS_IO ()
consoleLog s = liftJS_IO (putStrLn' s)

setTimeout' : (() -> JS_IO ()) -> (millis : Int) -> JS_IO ()
setTimeout' f millis = foreign FFI_JS "setTimeout(%0, %1)" (JsFn (() -> JS_IO ()) -> Int -> JS_IO ()) (MkJsFn f) millis

timeout : AsyncJS_IO a -> (millis : Int) -> AsyncJS_IO ()
timeout (MkAsync f) millis = MkAsync $ \g => setTimeout' (\_ => f (\_ => g ())) millis


namespace Main
  export
  main : JS_IO ()
  main = runAsync $ do
    x <- pure "blah"
    consoleLog x
    consoleLog "blaz"
    writeFile "foo.txt" "blar"
    parallel (timeout (consoleLog "foo!") 5000) (timeout (consoleLog "bar!") 5000)
    contents <- readFile "foo.txt"
    consoleLog contents
