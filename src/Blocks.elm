module Blocks exposing (..)

import Html
import Html.Attributes as Attr
import TypedSvg as Svg
import TypedSvg.Attributes exposing (fill)
import TypedSvg.Attributes.InPx exposing (x, y, width, height, strokeWidth)
import TypedSvg.Types exposing (Fill(..))

import Color exposing (..)

main =
  Html.program
    { init = (model, Cmd.none)
    , update = update
    , subscriptions = \model -> Sub.none
    , view = view
    }

{-
  Model
-}

type alias Model =
  { blocks : List Block
  , toolbox : Toolbox
  }

type Toolbox
  = EmptyToolbox
  | Toolbox (List ToolboxContent)

type ToolboxContent
  = Seperator
  | ToolboxBlock Block

type alias Block =
  { name : String
  , category : String
  , outputType : BlockType
  , content : List BlockContent
  }

type BlockType = BlockType String

type BlockContent
  = ValueInput BlockInput
  | StatementInput BlocksInput
  | TextField (BlockField String)
  | BlockText String

type alias BlockField a =
  { id : String
  , default : Maybe a
  , editable : Bool
  }

type alias BlockInput =
  { id : String
  , types : List BlockType
  , default : Maybe Block
  , editable : Bool
  }

type alias BlocksInput =
  { id : String
  , types : List BlockType
  , default : Maybe (List Block)
  , editable : Bool
  }

model =
  { blocks = blocks
  , toolbox = EmptyToolbox
  }

blocks =
  [ ioAsk
  , ioPrint
  ]

text : Block
text =
  { name = "text"
  , category = "text"
  , outputType = BlockType "String"
  , content =
    [ BlockText "\""
    , ValueInput
      { id = "value"
      , types = [BlockType "String"]
      , default = Nothing 
      , editable = True 
      }
    , BlockText "\""
    ]
  }

ioAsk : Block
ioAsk =
  { name = "ask"
  , category = "io"
  , outputType = BlockType "String" 
  , content =
    [ BlockText "ask"
    , ValueInput 
      { id = "question"
      , types = [BlockType "String"]
      , default = Just text 
      , editable = True 
      }
    ]
  }

ioPrint : Block
ioPrint =
  { name = "print"
  , category = "io"
  , outputType = BlockType "Statement"
  , content =
    [ BlockText "print"
    , ValueInput
      { id = "value"
      , types = [BlockType "Statement"]
      , default = Nothing
      , editable = True
      }
    ]
  }

{-
  Update
-}

update msg model =
  (model, Cmd.none)

{-
  View
-}

view model =
  Html.div
    []
    [ workspaceView model.blocks
    ]

workspaceView blocks =
  Svg.svg
    []
    <| List.map blockView blocks

blockView block =
  Svg.g
    []
    [ Svg.rect
      [ width 200
      , height 50
      , fill <| Fill darkCharcoal
      ]
      []
    , Svg.g
      []
      <| List.map blockContentView block.content
    ]

blockContentView content =
  case content of
    BlockText text ->
      Svg.text_ 
        [ x 50
        , y 50
        , fill <| Fill white
        ]
        [ Html.text text 
        ]
    _ ->
      Html.text ""
