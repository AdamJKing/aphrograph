{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Display.Projection where

import Control.Lens
import Display.Projection.Scalable

data ProjectionContext a b = ProjectionContext
  { origin :: (a, a),
    projection :: (b, b)
  }

data ProjectionContext2D a b c d = ProjectionContext2D
  { originX :: (a, a),
    originY :: (b, b),
    projectionX :: (c, c),
    projectionY :: (d, d)
  }

newtype Projected2D a b c d m t = Projected2D
  { runProjection :: ProjectionContext2D a b c d -> m (t c d)
  }

instance Bifunctor ProjectionContext where
  bimap f g ProjectionContext {..} = ProjectionContext (over each f origin) (over each g projection)

class MonadProjector a b m where
  project :: a -> m b

instance (Real a, Scalable b, Monad m) => MonadProjector a b (ReaderT (ProjectionContext a b) m) where
  project v = asks (\ProjectionContext {..} -> scale v origin projection)
