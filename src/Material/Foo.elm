module Material.Foo where

import Graphics.Element exposing (Element, spacer, color, opacity)
import Color exposing (black)

type alias Page = { content:Element, title:String }
type alias Pages = List Page

{-|
  The scrim is used to fade the screen
-}
scrim : (Int, Int) -> Element
scrim (width, height) =
    spacer (width) height
      |> color black
      |> opacity (0.5)
