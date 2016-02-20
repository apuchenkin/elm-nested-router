module Router.Mailbox where

import Router.Helpers  exposing (singleton)
import Router.Types    exposing (Action)

mailbox : Signal.Mailbox (List (Action state))
mailbox = Signal.mailbox []

address : Signal.Address (Action state)
address = Signal.forwardTo mailbox.address singleton
