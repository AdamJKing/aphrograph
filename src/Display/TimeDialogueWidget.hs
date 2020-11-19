{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
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
    yieldTimeValue,
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
    formState,
    handleFormEvent,
    newForm,
    radioField,
    renderForm,
    (@@=),
  )
import Control.Lens (makeLenses, (^.))
import Data.OpenUnion (Union, liftUnion)
import qualified Graphite.Types as Graphite
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
            [ (value, liftUnion (Field value), asStr value)
              | value <-
                  [ FifteenMins,
                    OneHour,
                    TwelveHours,
                    TwentyFourHours,
                    SevenDays
                  ]
            ]
       in Brick.newForm
            [ label "Quick Offset" @@= radioField chosenOffset radioFields
            ]

renderTimeDialogue :: Eq (Union ns) => TimeDialogue (Union ns) e -> Brick.Widget (Union ns)
renderTimeDialogue (TimeDialogue dialogue) = Brick.renderForm dialogue

asStr :: QuickOffset -> Text
asStr FifteenMins = "-15m"
asStr OneHour = "-1h"
asStr TwelveHours = "-12h"
asStr TwentyFourHours = "-24h"
asStr SevenDays = "-7d"

yieldTimeValue :: TimeDialogue n e -> Graphite.From
yieldTimeValue (TimeDialogue dialogue) = Graphite.From $ asStr (formState dialogue ^. chosenOffset)
