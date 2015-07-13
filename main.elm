import Window
import Color exposing (green, white, black)
import Graphics.Element exposing (Element)
import Graphics.Element exposing
  (Element, centered, middle, down, layers, container, color, flow, right, spacer)
import Graphics.Input exposing (clickable)
import Text exposing (fromString)
import Maybe exposing (Maybe)
import List

import Material exposing (Page, Pages)


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

pages : Pages
pages = [ helloWorld, components ]

main : Signal Element
main = Material.app pages
