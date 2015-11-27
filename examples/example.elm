import Window
import Color exposing (green, white, black)
import Graphics.Element exposing (Element)
import Graphics.Element exposing
  (Element, centered, middle, down, layers, container, color, flow, right, spacer)
import Graphics.Input exposing (clickable)
import Text exposing (fromString)
import Maybe exposing (Maybe)
import Html exposing (Html)
import List

import Material
import Material.Foo exposing (Page, Pages)


-- MODEL

helloWorld : Page
helloWorld =
  { title = "Material Design Sample"
  , content = centered (fromString "Hello World")
  }

components : Page
components =
  { title = "Components"
  , content = centered (fromString "Nothin` here yet")
  }

patterns : Page
patterns =
  { title = "Patterns"
  , content = centered (fromString "patterns will go here, patterns will go here, patterns will go here")
  }

pages : Pages
pages = [ helloWorld, components, patterns ]

main : Signal Html
main =
   Material.app pages
