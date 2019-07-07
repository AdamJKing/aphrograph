{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Display.Widgets where

import           Brick.Types                   as Brick

class CompileWidget w where
    compile :: forall n. w -> Brick.Widget n
