{-# LANGUAGE TemplateHaskell #-}

module Display.TimeDialogueWidget (QuickOffset (..), TimeDialogue, renderTimeDialogue) where

import Brick
  ( Padding (Pad),
    Widget,
    fill,
    hLimit,
    padBottom,
    str,
    vLimit,
    (<+>),
  )
import Brick.Forms as Brick
  ( Form,
    newForm,
    radioField,
    renderForm,
    (@@=),
  )
import Control.Lens (makeLenses)

data QuickOffset = FifteenMins | OneHour | TwelveHours | TwentyFourHours | SevenDays
  deriving (Eq, Enum)

data TimeFieldName = FifteenMinsField | OneHourField | TwelveHoursField | TwentyFourHoursField | SevenDaysField
  deriving (Eq, Ord, Show)

data Field

data TimeDialogue n e = TimeDialogue (TimeFieldName -> n) (Brick.Form QuickOffset e TimeFieldName)

-- timeDialogue :: QuickOffset -> TimeDialogue
-- timeDialogue default' =
--   let title = "Time Picker"
--       options = do
--         option <- [toEnum 0 ..]
--         return (nameOffset option, option)
--    in OpenDialogue $ Brick.dialog (Just title) (Just (fromEnum default', options)) 25
--   where
--     nameOffset FifteenMins = "-15m"
--     nameOffset OneHour = "-1h"
--     nameOffset TwelveHours = "-12h"
--     nameOffset TwentyFourHours = "-24h"
--     nameOffset SevenDays = "-7d"

-- updateDialogue :: Vty.Event -> TimeDialogue -> Brick.EventM n TimeDialogue
-- updateDialogue ev (OpenDialogue dialogue) = OpenDialogue <$> (CompM $ Brick.handleDialogEvent ev dialogue)

-- getTimeSelection :: Dialogue Open -> Maybe QuickOffset
-- getTimeSelection (OpenOnMetrics _) = _
-- getTimeSelection (OpenOnTime _) = _

data TimeDialogueState = TimeDialogueState
  { _chosenOffset :: QuickOffset
  }

makeLenses ''TimeDialogueState

timeDialogue :: TimeDialogueState -> Form TimeDialogueState e TimeFieldName
timeDialogue =
  let label s w =
        padBottom (Pad 1) $
          (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
   in Brick.newForm
        [ label "Quick Offset" @@= radioField chosenOffset [(FifteenMins, FifteenMinsField, "-15m")]
        ]

renderTimeDialogue :: TimeDialogue n e -> Brick.Widget n
renderTimeDialogue (TimeDialogue _ dialogue) = Brick.renderForm dialogue

