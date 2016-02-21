module Main (..) where

import StartApp exposing (start)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task
import Time exposing (second)
import Html.Animation as UI
import Material.TitleBar as TitleBar
import Material.SideNav as SideNav
import Material.CategoryNav as CategoryNav
import Material.Page exposing (..)


-- MODEL


type alias Model =
  { titleBar : TitleBar.Model
  , sideNav : SideNav.Model
  }



-- UPDATE


type Action
  = TB TitleBar.Action
  | SN SideNav.Action
  | ChangePage Page


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    ChangePage page ->
      let
        model =
          { model
            | titleBar =
                TitleBar.update
                  (TitleBar.UpdateTitle page.title)
                  model.titleBar
          }
      in
        update (SN SideNav.HideNav) model

    -- Currently the only action the toolbar can do is open the side navigation
    TB _ ->
      update (SN SideNav.ShowNav) model

    SN action ->
      case action of
        SideNav.Cat id action ->
          case action of
            CategoryNav.SelectPage page ->
              update (ChangePage page) model

            action ->
              sideNavUpdate (SideNav.Cat id action) model

        action ->
          sideNavUpdate action model


sideNavUpdate : SideNav.Action -> Model -> ( Model, Effects Action )
sideNavUpdate action model =
  let
    ( sideNav, effect ) =
      SideNav.update action model.sideNav
  in
    ( { model | sideNav = sideNav }
    , (Effects.map SN effect)
    )


view : Address Action -> Model -> Html
view address model =
  div
    []
    [ TitleBar.view (Signal.forwardTo address TB) model.titleBar
    , SideNav.view (Signal.forwardTo address SN) model.sideNav
    ]


init : ( Model, Effects Action )
init =
  let
    categories =
      [ { title = "Introduction"
        , pages =
            [ { title = "Hello World" } ]
        }
      , { title = "Category 2"
        , pages =
            [ { title = "Page 2" }
            , { title = "Page 3" }
            ]
        }
      ]
  in
    ( { titleBar = TitleBar.init "Hello World"
      , sideNav =
          SideNav.init
            ({ title = "Hello World" })
            categories
      }
    , Effects.none
    )


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
