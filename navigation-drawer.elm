import Window
import Color exposing (green, white, black)
import Graphics.Element exposing (Element)
import Graphics.Element as G -- exposing (Element, centered, container, color, flow, right, spacer)
import Graphics.Input exposing (clickable)
import Text exposing (fromString)
import Maybe exposing (Maybe)
import List

main : Signal Element
main = action.signal
  |> Signal.foldp update (MainView helloWorld)
  |> Signal.map2 view Window.dimensions

-- MODEL
type State
    = MainView Page 
    | NavBar Page 

type Action
    = OpenNavDrawer
    | CloseNavDrawer (Maybe Page) -- Might change content


type alias Page = { content:Element }

helloWorld : Page
helloWorld= { content = G.centered (fromString "Hello World") }

stuff : Page
stuff = { content = G.centered (fromString "Stuff") }

pages : List Page
pages = [ helloWorld, stuff]


-- INPUTS
action : Signal.Mailbox Action
action =
  Signal.mailbox (OpenNavDrawer)

-- DISPLAY

title : String -> Element
title string = G.centered (fromString string)

welcomePage : Element
welcomePage = G.centered (fromString "Hello World")

hamburger : Element
hamburger = G.image 60 60 "hamburger.svg"

-- clickable : Signal.Message -> Element -> Element
-- button : Signal.Message -> String -> Element

body : (Int, Int) -> Element -> Element
body (w, h) content =
  G.container w h G.middle content

-- toolbar takes in a width (of the screen) and a title string
toolbar : Int -> Int -> String -> Element
toolbar width height string =
  G.container width height G.midLeft
  (G.flow G.right
    [ G.spacer 60 1
    , hamburger
        |> clickable (Signal.message action.address OpenNavDrawer)
    , G.spacer 180 1
    , title string
    ]
  )
  |> G.color green

drawerOption : Page -> Element
drawerOption page =
      G.container 340 100 G.middle page.content
          |> clickable (Signal.message action.address (CloseNavDrawer (Just page)))

-- NavigationDrawer comes in from the left
navigationDrawer width height =
  G.flow G.right
  [ G.flow G.down
    (List.map drawerOption pages)
    |> G.color white
  , G.spacer (width) height
      |> G.color black
      |> G.opacity (0.5)
      |> clickable (Signal.message action.address (CloseNavDrawer Nothing))
  ]

view : (Int, Int) -> State -> Element
view (w, h) state =
    case state of
        MainView page -> G.flow G.down 
          [ toolbar w 180 "Title"
          , body (w, (h-180)) page.content
          ]
        NavBar page -> G.layers
          [ G.flow G.down
              [ toolbar w 180 "Title"
              , body (w, (h-180)) page.content
              ]
          , navigationDrawer w h
          ]

-- UPDATE

update : Action -> State -> State
update action state =
  let page =
    case state of
      MainView p -> p
      NavBar p -> p
  in case action of
    OpenNavDrawer -> NavBar page
    CloseNavDrawer (Just newPage) -> MainView newPage
    CloseNavDrawer (Nothing) -> MainView page
