{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Display.TimeDialogueWidget
  ( QuickOffset (..),
    TimeDialogueState (TimeDialogueState),
    TimeDialogue,
    renderTimeDialogue,
    TimeFieldName,
    timeDialogue,
    updateDialogue,
  )
where

import Brick
  ( BrickEvent,
    EventM,
    Padding (Pad),
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
    handleFormEvent,
    newForm,
    radioField,
    renderForm,
    (@@=),
  )
import Control.Lens (makeLenses)
import Data.OpenUnion (Union, liftUnion)
import TypeFun.Data.List (Elem)

data QuickOffset = FifteenMins | OneHour | TwelveHours | TwentyFourHours | SevenDays
  deriving (Eq, Enum, Ord, Show)

newtype TimeFieldName = Field QuickOffset
  deriving (Eq, Ord, Show)

newtype TimeDialogueState = TimeDialogueState
  { _chosenOffset :: QuickOffset
  }

makeLenses ''TimeDialogueState

data TimeDialogue n e where
  TimeDialogue ::
    forall (ns :: [Type]) e.
    Elem TimeFieldName ns =>
    Brick.Form TimeDialogueState e (Union ns) ->
    TimeDialogue (Union ns) e

updateDialogue :: Eq (Union ns) => BrickEvent (Union ns) e -> TimeDialogue (Union ns) e -> Brick.EventM (Union ns) (TimeDialogue (Union ns) e)
updateDialogue keyPress (TimeDialogue dialogue) = TimeDialogue <$> Brick.handleFormEvent keyPress dialogue

timeDialogue :: (Ord (Union ns), Eq (Union ns), Elem TimeFieldName ns, Show (Union ns)) => TimeDialogueState -> TimeDialogue (Union ns) e
timeDialogue = TimeDialogue . form
  where
    form =
      let label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s) <+> fill ' ' <+> w
          radioFields =
            addField
              [ (FifteenMins, "-15m"),
                (OneHour, "-1h"),
                (TwelveHours, "-12h"),
                (TwentyFourHours, "-24h"),
                (SevenDays, "-7d")
              ]
       in Brick.newForm
            [ label "Quick Offset" @@= radioField chosenOffset radioFields
            ]

addField :: Elem TimeFieldName ns => [(QuickOffset, s)] -> [(QuickOffset, Union ns, s)]
addField values = do
  (value, label) <- values
  return (value, liftUnion (Field value), label)

renderTimeDialogue :: Eq (Union ns) => TimeDialogue (Union ns) e -> Brick.Widget (Union ns)
renderTimeDialogue (TimeDialogue dialogue) = Brick.renderForm dialogue
