module Material.TitleBar (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task
import Time exposing (second)
import Html.Animation as UI


-- MODEL


type alias Model =
  String


init : String -> Model
init title =
  title



-- UPDATE


type Action
  = ShowNav
  | UpdateTitle String


update : Action -> Model -> Model
update action model =
  case action of
    ShowNav ->
      model

    UpdateTitle title ->
      title



-- UPDATE


view : Address Action -> Model -> Html
view address model =
  let
    toolbarStyle =
      [ ( "position", "absolute" )
      , ( "left", "0px" )
      , ( "top", "0px" )
      , ( "width", "100%" )
      , ( "height", "100px" )
      , ( "background-color", "#00BCD4" )
      , ( "z-index", "2" )
      ]
  in
    div
      [ onClick address ShowNav
      , id "hovering"
      , style toolbarStyle
      ]
      [ h1
          [ style [ ( "padding", "25px" ) ] ]
          [ text model ]
      ]
