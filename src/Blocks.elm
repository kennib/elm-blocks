module Blocks exposing (..)

import List.Extra as List

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
  Codegen
-}

codeGen : List Block -> String
codeGen blocks =
  List.map (blockToCode 0) blocks
  |> String.join "\n"

blockToCode : Int -> Block -> String
blockToCode nestingLevel block =
  case block.name of
    _ ->
      List.map (contentToCode nestingLevel) block.content |> String.join " "

contentToCode : Int -> BlockContent -> String
contentToCode nestingLevel content =
  case content of
    TextField field ->
      field.field |> Maybe.withDefault ""
    ValueInput valueInput ->
      "(" ++ (Maybe.map (blockToCode (nestingLevel+1)) valueInput.block |> Maybe.withDefault "None") ++ ")"
    StatementInput statementInput ->
      let
        indent = "\n" ++ String.repeat (nestingLevel+1) "\t"
      in
        ":" ++ indent ++ (List.map (blockToCode (nestingLevel+1)) statementInput.blocks |> String.join indent)
    BlockText text ->
      text
    BlockNewline ->
      ""
    

getTextField : Block -> String -> Maybe String
getTextField block fieldId =
  block.content
  |> List.filterMap (\content ->
    case content of
      TextField field ->
        if field.id == fieldId then
          Just field.field
        else
          Nothing
      _ ->
        Nothing
  )
  |> List.head
  |> Maybe.withDefault Nothing

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
    , codeView model.blocks
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

codeView : List Block -> Html msg
codeView blocks =
  Html.textarea
    [ attribute "rows" "15"
    ]
    [ Html.text
      <| codeGen blocks
    ]
