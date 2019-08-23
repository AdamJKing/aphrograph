{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Display.Widgets where

import           Brick.Types                   as B
import           Brick.Widgets.Core            as BW
import           Brick.Widgets.List            as WL
import           Graphite.Types
import           Data.Vector
import           Prelude                 hiding ( on )

class CompileWidget n w where
    compile :: w -> B.Widget n

class CompileLayeredWidget n w where
    compileLayered :: w -> [ B.Widget n ]

data AppComponent = GraphView | MetricsBrowserComponent
    deriving ( Eq, Ord, Show )

newtype MetricsBrowserWidget = MetricsBrowser (Vector Metric)

instance CompileWidget AppComponent MetricsBrowserWidget where
    compile (MetricsBrowser metrics) =
        let hasFocus   = True
            listHeight = 1
        in  WL.renderList
                (\active (Metric descriptor) -> withAttr
                    ("metric" <> if active then "selected" else "unselcted")
                    (BW.txt descriptor)
                )
                hasFocus
                (WL.list MetricsBrowserComponent metrics listHeight)
