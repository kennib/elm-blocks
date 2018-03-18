module Blocks exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style,attribute)
import TypedStyles exposing (..)

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
  | BlockNewline

type alias BlockField a =
  { id : String
  , field : Maybe a
  , editable : Bool
  }

type alias BlockInput =
  { id : String
  , types : List BlockType
  , block : Maybe Block
  , editable : Bool
  }

type alias BlocksInput =
  { id : String
  , types : List BlockType
  , blocks : List Block
  , editable : Bool
  }

model =
  { blocks = blocks
  , toolbox = EmptyToolbox
  }

blocks =
  [ ioAsk
  , ioPrint
  , controlFor
  ]

text : Block
text =
  { name = "text"
  , category = "text"
  , outputType = BlockType "String"
  , content =
    [ BlockText "\""
    , TextField
      { id = "text"
      , field = Nothing
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
      , block = Just text
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
      , types = [BlockType "Value"]
      , block = Nothing
      , editable = True
      }
    ]
  }

controlFor : Block
controlFor =
  { name = "for"
  , category = "control"
  , outputType = BlockType "Statement"
  , content =
    [ BlockText "for"
    , ValueInput
      { id = "variable"
      , types = [BlockType "Variable"]
      , block = Nothing
      , editable = True
      }
    , BlockText "in"
    , ValueInput
      { id = "iterable"
      , types = [BlockType "Iterable"]
      , block = Nothing
      , editable = True
      }
    , BlockNewline
    , StatementInput
      { id = "do"
      , types = [BlockType "Statement"]
      , blocks =
        [ ioPrint
        , ioAsk
        ]
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

view : Model -> Html msg
view model =
  Html.div
    []
    [ workspaceView model.blocks
    ]

workspaceView : List Block -> Html msg
workspaceView blocks =
  Html.ol
    [ style
      [ ("list-style", "none")
      ]
    ]
    <| List.map (\block -> Html.li [] [ blockView block ]) blocks

blockView : Block -> Html msg
blockView block =
  Html.div
    [ style
      [ ("display", "inline-block")
      , border 1 px solid white
      , backgroundColor gray
      ]
    ]
    <| List.map blockContentView block.content

blockContentView : BlockContent -> Html msg
blockContentView content =
  case content of
    ValueInput input ->
      blockInputView input
    StatementInput input ->
      blocksInputView input
    TextField textField ->
      Html.input
        []
        [ Html.text <|
        case textField.field of
          Just text ->
            text
          Nothing ->
            ""
        ]
    BlockText text ->
      Html.span
        []
        [ Html.text text
        ]
    BlockNewline ->
      Html.br [] []

blockInputView : BlockInput -> Html msg
blockInputView input =
  Html.span
    [ style
      [ ("display", "inline-block")
      , ("min-width", "50px")
      , minHeight 15 px
      , border 1 px solid red
      , backgroundColor white
      , margin 5 px
      , padding 5 px
      ]
    ]
    [ case input.block of
      Just block ->
        blockView block
      Nothing ->
        Html.text ""
    ]

blocksInputView : BlocksInput -> Html msg
blocksInputView input =
  Html.ol
    [ style
      [ ("display", "inline-block")
      , ("list-style", "none")
      , ("min-width", "50px")
      , minHeight 15 px
      , border 1 px solid red
      , borderRightWidth 0 px
      , backgroundColor white
      , margin 5 px
      , padding 5 px
      ]
    ]
    <| List.map (\block -> Html.li [] [ blockView block ]) input.blocks

