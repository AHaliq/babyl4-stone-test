{-# LANGUAGE RankNTypes #-}
module InnerMain (main) where

import Data.ByteString (ByteString)
import Data.String (fromString)
import Reflex.Dom hiding (mainWidgetWithCss, run)
import qualified Reflex.Dom.Main as RDMain
import qualified Widgets.Page.TwoWindow as W
import qualified Widgets.Page.TwoWindow.Style as Style (css)

#if defined(ghcjs_HOST_OS)
run :: a -> a
run = id
#elif defined(MIN_VERSION_jsaddle_warp)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Language.Javascript.JSaddle (JSM)
--import qualified Language.Javascript.JSaddle.Warp as JW
import System.Environment (lookupEnv)

import Network.Wai (Application, requestMethod, pathInfo)
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets
import Debug.Trace (trace)


run :: JSM () -> IO()
run jsm = do
  port <- maybe 3001 read <$> lookupEnv "JSADDLE_WARP_PORT"
  putStrLn $ "Running jsaddle-warp server on port " <> show port
  runWarp port jsm

runWarp :: Int -> JSM () -> IO ()
runWarp port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings))
      . staticMiddleware =<<
       jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleApp

waiApp :: Application
waiApp = staticApp (defaultFileServerSettings "../")

staticMiddleware :: Application -> Application
staticMiddleware app = \req resp -> aux (requestMethod req, pathInfo req) req resp
  where
    aux x@("GET", "static":_) = trace ("wai app: " ++ show x) waiApp -- its a list of paths
    aux x = trace ("app: " ++ show x) app

#elif defined(wasm32_HOST_ARCH)
-- import qualified Language.Javascript.JSaddle.Wasm as Wasm (run)
-- import Language.Javascript.JSaddle (JSM)
-- run :: JSM () -> IO ()
-- run = Wasm.run 0
#else
-- import Language.Javascript.JSaddle.WebKitGTK (run)
#endif

mainWidgetWithCss :: ByteString -> (forall x. Widget x ()) -> IO ()
mainWidgetWithCss css w = run $ RDMain.mainWidgetWithCss css w

main :: IO ()
main =
  let cssString = fromString Style.css
   in mainWidgetWithCss cssString W.widget