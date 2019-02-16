{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Labels where

import           Normalisation
import           Display.Graph
import           Display.Types
import           Control.Monad.Except

data Label = Label !Text !Int deriving (Eq, Show)

data Projection a b = Projection (a, a) (b, b) deriving (Eq, Show)

projected
    :: (Scaled a b, MonadError NormalisationFailure m, MonadReader (Projection a b) m) => a -> m b
projected n = do
    Projection a b <- ask
    liftEither $ scale n a b

original
    :: (Scaled b a, MonadError NormalisationFailure m, MonadReader (Projection a b) m) => b -> m a
original n = do
    Projection a b <- ask
    liftEither $ scale n b a

availableOnScreen :: (Scaled a b, MonadReader (Projection a b) m) => m b
availableOnScreen = reader (\(Projection _ (lo, hi)) -> hi - lo)

labels
    :: (Scaled a Int, Scaled Int a, Show a, MonadReader (Projection a Int) m)
    => ExceptT DisplayError m [Label]
labels = do
    (available :: Double) <- fromIntegral <$> availableOnScreen
    let ticks = floor (sqrt available)
    withExceptT ErrorDuringRender $ mapM
        (\i -> do
            let pos = i * ticks
            value <- show <$> original pos
            return (Label value pos)
        )
        [0 .. ticks]
