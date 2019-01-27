module Labels.Orientation where

import qualified Data.Text                     as T
import           Fmt
import           Fmt.Internal.Core             as Fmt

data HorizontalText = HorizontalText !Fmt.Builder !Int deriving (Eq, Show)
data VerticalText = VerticalText !Fmt.Builder !Int deriving (Eq, Show)

instance Fmt.FromBuilder VerticalText where
    fromBuilder = VerticalText <*> (T.length . show)

instance Fmt.FromBuilder HorizontalText where
    fromBuilder = HorizontalText <*> (T.length . show)

instance Fmt.Buildable VerticalText where
    build (VerticalText txt _) = build txt

instance Fmt.Buildable HorizontalText where
    build (HorizontalText txt _) = build txt

instance Semigroup HorizontalText where
    (HorizontalText txt i) <> (HorizontalText _   0) = HorizontalText txt i
    (HorizontalText _   0) <> (HorizontalText txt i) = HorizontalText txt i
    (HorizontalText txt1 i1) <> (HorizontalText txt2 i2) =
        HorizontalText (txt1 |++| txt2) (i1 + i2)

instance Monoid HorizontalText where
    mempty = HorizontalText mempty 0

instance Semigroup VerticalText where
    (VerticalText txt i) <> (VerticalText _   0) = VerticalText txt i
    (VerticalText _   0) <> (VerticalText txt i) = VerticalText txt i
    (VerticalText txt1 i1) <> (VerticalText txt2 i2) =
        VerticalText (txt1 |+ "\n" +| txt2) (i1 + i2)

instance Monoid VerticalText where
    mempty = VerticalText mempty 0

horizontal :: T.Text -> HorizontalText
horizontal = HorizontalText <$> build <*> T.length

vertical :: T.Text -> VerticalText
vertical = VerticalText <$> build <*> ((1 +) . countNewlines)
    where countNewlines = T.length . T.filter (== '\n')

