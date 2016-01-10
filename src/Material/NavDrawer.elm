module Material.NavDrawer where

import Color exposing (white)
import Material.Foo exposing (Category, Categories, Page, Pages, scrim, divWidth, divHeight, divWidthPair, divHeightPair )
import Text exposing (fromString)
import Signal exposing (Mailbox)
import Time exposing (Time)
import Easing exposing (easeInBounce, easeInElastic)
import Animation exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (width, height, size, style, id, class)
import Html.Events exposing (onClick)
import Flex
import Debug

-- MODEL

type Action
    = SelectPage Page
    | SelectCategory Category
    | Tick Time
    | Open Time
    | Close Time
    | CloseNow -- uses model.clock time to close
    | WindowResize (Int, Int)

-- drawerWidth is actually used for the offset as the nav drawer moves on and off the screen
-- It's easier to think of as a width
type alias Model =
    { screenWidth: Int
    , screenHeight: Int
    , drawerWidth: Animation
    , categories: NavCategories
    , page: Page
    , pages: Pages
    , mailbox: Mailbox Action
    , start: Time
    , clock: Time
    }

-- The list of categories and whether or not to display the pages
type alias NavCategories = List (Category, Bool)

initialize : Int -> Int -> Categories -> Page -> Pages -> Mailbox Action -> Time -> Time -> Model
initialize w h categories page pages mailbox start clock =
    let widthAnimation = (animation start |> from 0 |> to 0 |> duration 400)
     in Model w h widthAnimation (initCategories categories) page pages mailbox start clock

initCategories : Categories -> NavCategories
initCategories categories =
    List.map (\c -> (c, False)) categories

model0 : Categories -> Page -> Pages -> (Int, Int) -> Mailbox Action -> Model
model0 categories page pages (w, h) mailbox = initialize w h categories page pages mailbox 0 0

navDrawerWidth = 340
-- drawerOptionHeight = 100
drawerOptionHeight = 43

-- UPDATE

step : Action -> Model -> Model
step action model =
    case action of
        SelectPage page ->
            {model | page = page, drawerWidth = drawerCloseAnimation model.clock }
        SelectCategory category ->
            let categories = List.map (\(c, v) -> if c.name == category.name then (c, not v) else (c,v)) model.categories
             in {model | categories = categories }
            -- TODO: toggle page display
        Tick t ->
            {model | clock = model.clock + t}
        Open t ->
            {model | drawerWidth =  drawerOpenAnimation t }
        Close t ->
            {model | drawerWidth = drawerCloseAnimation t }
        CloseNow ->
            {model | drawerWidth = drawerCloseAnimation model.clock }
        WindowResize (w, h) ->
            {model | screenWidth = w, screenHeight = h }

drawerOpenAnimation : Time -> Animation
drawerOpenAnimation t = (animation t |> from 0 |> to navDrawerWidth |> duration 400)

drawerCloseAnimation : Time -> Animation
drawerCloseAnimation t = (animation t |> from navDrawerWidth |> to 0 |> duration 400)

-- VIEW

{-|
  NavigationDrawer comes in from the left
-}
view : Model -> Html
view model =
  let drawerOffset = (round (animate model.clock model.drawerWidth)) - navDrawerWidth
      pageScrim = Html.div
                  [ id "scrim"
                  , divHeight model.screenHeight
                  , divWidth model.screenWidth
                  , onClick model.mailbox.address CloseNow
                  , style
                    [ ("background-color","black")
                    , ("opacity", "0.5")
                    , ("position", "absolute")
                    , ("z-index", "99")
                    , (divWidthPair model.screenWidth)
                    , (divHeightPair model.screenHeight)
                    ]
                  ]
                  []
      drawer = Html.div
                [ id "drawer"
                , style
                    [ (divWidthPair navDrawerWidth)
                    , (divHeightPair model.screenHeight)
                    , ("position", "absolute")
                    , ("background-color", "white")
                    , ("left", toString (drawerOffset) ++ "px")
                    , ("z-index", "100")
                    ]
                ]
               (categoryDrawers model.mailbox.address model.categories)
               -- (drawerOptions model.mailbox.address model.pages)

   in if drawerOffset + navDrawerWidth > 0 then
        Html.div []
        [ drawer
        , pageScrim
        ]
      else
        Html.div [] []

-- Nav Drawer View
categoryDrawers : Signal.Address Action -> NavCategories -> List Html
categoryDrawers address categories =
    List.map (categoryDrawerOptions address) categories

categoryDrawerOptions : Signal.Address Action -> (Category, Bool) -> Html
categoryDrawerOptions address (category, pagesVisible) =
    let pages =
        if pagesVisible then
           category.pages
        else []
     in Html.div [
        class "category"
        , onClick address (SelectCategory category)
        , style
            [ ("cursor","pointer")
            ]
    ]
    ((Html.h3 [] [Html.text category.name]) ::
        (drawerOptions address pages))

drawerOptions : Signal.Address Action -> Pages -> List Html
drawerOptions address pages =
    (List.map (drawerOption address) pages)

drawerOption : Signal.Address Action -> Page -> Html
drawerOption address page =
    Html.div
        [ class "drawerOption"
        , onClick address (SelectPage page)
        , style
            [ ("cursor","pointer")
            , (divWidthPair navDrawerWidth)
            , (divHeightPair drawerOptionHeight)
            ]
        ]
        [ Html.text page.title ]

-- SIGNALS

mailbox : Page -> Mailbox Action
mailbox page =
  Signal.mailbox (SelectPage page)

