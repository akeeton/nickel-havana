module WidgetContainer
    ( WidgetContainer
    , Action
    , init
    , update
    , view
    )
    where

import Html exposing (..)
import Signal exposing (Address)

import Widget exposing (Widget)


type alias WidgetContainer =
    { widgets: List Widget
    }


type Action
    = DoNothing
    | WidgetAction (Widget.Action)


init : List Widget -> WidgetContainer
init widgets =
    { widgets = widgets }


update : Action -> WidgetContainer -> WidgetContainer
update action container =
    case action of
        WidgetAction action ->
            container

        DoNothing ->
            container


view : Address Action -> WidgetContainer -> Html
view address container =
    let
        forwardingAddress = Signal.forwardTo address WidgetAction
        widgetsHtmls = List.map (Widget.view forwardingAddress) container.widgets
    in
        div [] widgetsHtmls
