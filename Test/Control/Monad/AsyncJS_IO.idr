module Test.Control.Monad.AsyncJS_IO

import Control.Monad.AsyncJS_IO
import Data.JSError

%lib Node "fs"
%hide Prelude.File.readFile

toPtr : (JSError -> JS_IO ()) -> (Ptr -> JS_IO ())
toPtr errorCb = (\e => errorCb $ MkJSError e)

writeFile' : String -> String -> (JSError -> JS_IO ()) -> (() -> JS_IO ()) -> JS_IO ()
writeFile' path contents error success =
  foreign FFI_JS "fs.writeFile(%0, %1, function(err, data) { %3(data) })"
    (String -> String -> JsFn (Ptr -> JS_IO ()) -> JsFn (() -> JS_IO ()) -> JS_IO ()) path contents (MkJsFn $ toPtr error) (MkJsFn success)

writeFile : String -> String -> AsyncJS_IO ()
writeFile path contents = MkAsync $ \e => \f => writeFile' path contents (\x => e x) (\x => f x)


readFile' : String -> (JSError -> JS_IO ()) -> (String -> JS_IO ()) -> JS_IO ()
readFile' path error success = foreign FFI_JS "fs.readFile(%0, 'utf8', function(err, data) { err ? %1(err) : %2(data) })"
  (String -> JsFn (Ptr -> JS_IO ()) -> JsFn (String -> JS_IO ()) -> JS_IO ()) path (MkJsFn $ toPtr error) (MkJsFn success)

readFile : String -> AsyncJS_IO String
readFile path = MkAsync $ \e => \f => readFile' path (\x => e x) (\s => f s)

consoleLog : String -> AsyncJS_IO ()
consoleLog s = liftJS_IO (putStrLn' s)

setTimeout' : (() -> JS_IO ()) -> (millis : Int) -> JS_IO ()
setTimeout' f millis = foreign FFI_JS "setTimeout(%0, %1)" (JsFn (() -> JS_IO ()) -> Int -> JS_IO ()) (MkJsFn f) millis

timeout : AsyncJS_IO a -> (millis : Int) -> AsyncJS_IO ()
timeout (MkAsync f) millis = MkAsync $ \e => \g => setTimeout' (\_ => f e (\_ => g ())) millis

namespace Main
  export
  main : JS_IO ()
  main = runAsync $ do
    x <- pure "blah"
    consoleLog x
    consoleLog "blaz"
    writeFile "foo.txt" "worble"
    parallel (timeout (consoleLog "foo!") 1000) (timeout (consoleLog "bar!") 1000)
    contents <- readFile "foo.txt"
    consoleLog contents
    --throwAsync $ readFile "dontexist!"
    res <- attempt $ readFile "dontexist"
    case res of
      --Left v => liftJS_IO $ throw v
      Left v => consoleLog (show v)
      Right _ => consoleLog "SUCCESS"
    consoleLog "!"
