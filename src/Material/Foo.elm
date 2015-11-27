module Material.Foo where

import Graphics.Element exposing (Element, spacer, color, opacity)
import Color exposing (black)
import Html exposing (Html, Attribute)
import Html.Attributes exposing (width, height, size, style, id)

type alias Page = { content:Element, title:String }
type alias Pages = List Page

{-|
  The scrim is used to fade the screen
-}

scrim : (Int, Int) -> Html
scrim (w, h) =
    Html.div
        [ id "scrim"
        , divHeight h
        , divWidth w
        , style
            [ ("background-color","black")
            , ("opacity","0.5")
            ]
        ]
        []

divWidthPair : Int -> (String, String)
divWidthPair w = ("width", (toString w) ++ "px")

divHeightPair: Int -> (String, String)
divHeightPair h = ("height", (toString h) ++ "px")

-- TODO: find out why these don't work with each other
divWidth : Int -> Attribute
divWidth w = style [("width", (toString w) ++ "px")]

divHeight: Int -> Attribute
divHeight h = style [("height", (toString h) ++ "px")]
