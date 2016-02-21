module Material.CategoryNav (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task
import Time exposing (second)
import Html.Animation as UI
import Material.Page exposing (..)


-- MODEL


type alias Model =
  { title : String
  , pages : List Page
  , style : UI.Animation
  , isOpen : Bool
  }


init : String -> List Page -> Model
init title pages =
  { title = title
  , pages = pages
  , style = UI.init [ UI.MarginTop -100 UI.Percent ]
  , isOpen = False
  }



-- ACTION


type Action
  = SelectCategory
  | SelectPage Page
  | Animate UI.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    SelectCategory ->
      let
        ( anim, fx ) =
          if model.isOpen then
            UI.animate
              |> UI.duration (0.4 * second)
              |> UI.props
                  [ UI.MarginTop (UI.to -100) UI.Percent
                  ]
              |> UI.on model.style
          else
            UI.animate
              |> UI.duration (0.4 * second)
              |> UI.props
                  [ UI.MarginTop (UI.to 0) UI.Percent
                  ]
              |> UI.on model.style
      in
        ( { model
            | style = anim
            , isOpen = not model.isOpen
          }
        , Effects.map Animate fx
        )

    Animate action ->
      let
        ( anim, fx ) =
          UI.update action model.style
      in
        ( { model | style = anim }
        , Effects.map Animate fx
        )

    SelectPage _ ->
      ( model, Effects.none )



-- VIEW


view : Signal.Address Action -> Model -> List Html
view address model =
  ((Html.dt
      [ class "categoryTitle"
      , onClick address SelectCategory
      , style
          [ ( "position", "relative" )
          , ( "z-index", "2" )
          , ( "cursor", "pointer" )
          , ( "padding", "15px 0px 15px 22px" )
          , ( "font-weight", "700" )
          ]
      ]
      [ Html.text model.title ]
   )
    :: [ Html.div
          [ class "subpages"
          , style
              ([ ( "position", "relative" )
               , ( "z-index", "1" )
               , ( "overflow", "hidden" )
               ]
              )
          ]
          [ Html.div
              [ class "subpageContainer"
              , style (UI.render model.style)
              ]
              (viewSubpages address model.pages)
          ]
       ]
  )


viewSubpages : Address Action -> List Page -> List Html
viewSubpages address pages =
  (List.map (viewSubpage address) pages)


viewSubpage : Signal.Address Action -> Page -> Html
viewSubpage address page =
  Html.dd
    [ class "drawerOption"
    , onClick address (SelectPage page)
    , style
        [ ( "cursor", "pointer" )
        ]
    ]
    [ text page.title ]
