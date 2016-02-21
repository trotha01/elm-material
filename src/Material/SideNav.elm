module Material.SideNav (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task
import Time exposing (second)
import Html.Animation as UI
import Material.CategoryNav as CategoryNav
import Material.Page exposing (..)


-- MODEL


type alias Model =
  { style : UI.Animation
  , scrimStyle : UI.Animation
  , currentPage : Page
  , categories : List ( Int, CategoryNav.Model )
  }


init : Page -> List Category -> Model
init currentPage categories =
  let
    style =
      (UI.init
        [ UI.Left -350.0 UI.Px
        , UI.Opacity 0.0
        ]
      )

    scrimStyle =
      (UI.init
        [ UI.Width 0 UI.Percent ]
      )
  in
    { style = style
    , scrimStyle = scrimStyle
    , currentPage = currentPage
    , categories =
        List.indexedMap
          (\i c ->
            ( i, CategoryNav.init c.title c.pages )
          )
          categories
    }



-- ACTION


type Action
  = ShowNav
  | HideNav
  | Animate UI.Action
  | Cat Int CategoryNav.Action



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Cat id action ->
      updateCategory id model action

    ShowNav ->
      let
        ( anim, fx ) =
          UI.animate
            |> UI.duration (0.4 * second)
            |> UI.props
                [ UI.Left (UI.to 0) UI.Px
                , UI.Opacity (UI.to 1)
                ]
            |> UI.on model.style

        -- TODO: change duration to 0
        ( scrimAnim, scrimFx ) =
          UI.animate
            |> UI.duration (0.1 * second)
            |> UI.props
                [ UI.Width (UI.to 100) UI.Percent ]
            |> UI.on model.scrimStyle
      in
        ( { model
            | style = anim
            , scrimStyle = scrimAnim
          }
          -- TODO: do I need to batch?
        , Effects.batch
            [ Effects.map Animate fx
            , Effects.map Animate scrimFx
            ]
        )

    HideNav ->
      let
        ( anim, fx ) =
          UI.animate
            |> UI.duration (0.4 * second)
            |> UI.props
                [ UI.Left (UI.to -350) UI.Px
                , UI.Opacity (UI.to 0)
                ]
            |> UI.on model.style

        ( scrimAnim, scrimFx ) =
          UI.animate
            |> UI.duration (0.1 * second)
            |> UI.props
                [ UI.Width (UI.to 0) UI.Percent ]
            |> UI.on model.scrimStyle
      in
        ( { model
            | style = anim
            , scrimStyle = scrimAnim
          }
          -- TODO: do I need to batch?
        , Effects.batch
            [ Effects.map Animate fx
            , Effects.map Animate scrimFx
            ]
        )

    Animate action ->
      let
        ( anim, fx ) =
          UI.update action model.style

        ( scrimAnim, scrimFx ) =
          UI.update action model.scrimStyle
      in
        ( { model
            | style = anim
            , scrimStyle = scrimAnim
          }
        , Effects.batch
            [ Effects.map Animate fx
            , Effects.map Animate scrimFx
            ]
        )


updateCategory : Int -> Model -> CategoryNav.Action -> ( Model, Effects Action )
updateCategory id model action =
  let
    updateCat ( currentId, cat ) ( categories, effects ) =
      if currentId == id then
        let
          ( updatedCat, newEffect ) =
            CategoryNav.update action cat

          newMappedEffect =
            Effects.map (Cat id) newEffect
        in
          ( ( id, updatedCat ) :: categories, newMappedEffect )
      else
        ( ( currentId, cat ) :: categories, effects )

    ( updatedCats, newEffects ) =
      List.foldr updateCat ( [], Effects.none ) model.categories
  in
    ( { model | categories = updatedCats }, newEffects )



-- VIEW


view : Address Action -> Model -> Html
view address model =
  let
    menuStyle =
      [ ( "position", "absolute" )
      , ( "top", "0px" )
      , ( "padding", "25px" )
      , ( "width", "300px" )
      , ( "height", "100%" )
      , ( "background-color", "white" )
      , ( "color", "black" )
      , ( "z-index", "3" )
      ]

    scrimStyle =
      [ ( "position", "absolute" )
      , ( "left", "0px" )
      , ( "top", "0px" )
      , ( "width", "100%" )
      , ( "height", "100%" )
      , ( "background-color", "rgba(0, 0, 0, 0.5)" )
      , ( "z-index", "1" )
      ]
  in
    div
      []
      [ div
          [ style (menuStyle ++ (UI.render model.style)) ]
          [ Html.dl
              [ class "category"
              , style
                  [ ( "margin", "0px" ) ]
              ]
              (viewCategories address model.categories)
          ]
      , div
          [ onClick address (HideNav)
          , id "scrim"
          , style (scrimStyle ++ (UI.render model.scrimStyle))
          ]
          []
      ]


viewCategories : Signal.Address Action -> List ( Int, CategoryNav.Model ) -> List Html
viewCategories address categories =
  List.concat
    (List.map
      (\( i, c ) ->
        (CategoryNav.view
          (Signal.forwardTo address (Cat i))
        )
          c
      )
      categories
    )
