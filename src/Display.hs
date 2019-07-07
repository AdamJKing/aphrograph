
module Display where

import           App
import           Display.Widgets
import           Display.Graph.Widget
import qualified Brick.Widgets.Core            as Widget

newtype AppErrorWidget = AppErrorWidget AppError deriving Show
newtype AppWidget = AppWidget (Either AppErrorWidget GraphDisplayWidget ) deriving Show

instance CompileWidget AppErrorWidget where
    compile (AppErrorWidget err) = Widget.str (show err)

instance CompileWidget AppWidget where
    compile (AppWidget (Left  errState    )) = compile errState
    compile (AppWidget (Right defaultState)) = compile defaultState

constructDom :: AppState -> AppWidget
constructDom (AppState       ctxt) = AppWidget (Right (graphDisplayWidget ctxt))
constructDom (FailedAppState err ) = AppWidget (Left (AppErrorWidget err))
