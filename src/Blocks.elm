module Blocks exposing (..)

import List.Extra as List

import Html exposing (Html)
import Html.Events exposing (onInput)
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
  { blocks : List (Block (WorkspaceBlock {}))
  , toolbox : Toolbox
  }

type Toolbox
  = EmptyToolbox
  | Toolbox (List ToolboxContent)

type ToolboxContent
  = Seperator
  | ToolboxBlock (Block {})

type alias WorkspaceBlock a =
  { a
  | id : WorkspaceId
  }

type alias WorkspaceId = Int

type alias Block a =
  { a
  | name : String
  , category : String
  , outputType : BlockType
  , content : List (BlockContent a)
  }

type BlockType = BlockType String

type BlockContent a
  = ValueInput (BlockInput a)
  | StatementInput (BlocksInput a)
  | TextField (BlockField String)
  | BlockText String
  | BlockNewline

type alias BlockField a =
  { id : String
  , field : Maybe a
  , editable : Bool
  }

type alias BlockInput a =
  { id : String
  , types : List BlockType
  , block : Maybe (Block a)
  , editable : Bool
  }

type alias BlocksInput a =
  { id : String
  , types : List BlockType
  , blocks : List (Block a)
  , editable : Bool
  }

model =
  { blocks = blocks
  , toolbox = EmptyToolbox
  }

blocks =
  workspaceBlocks
    [ ioAsk
    , ioPrint
    , controlFor
    ]

workspaceBlocks : List (Block {}) -> List (Block (WorkspaceBlock {}))
workspaceBlocks blocks =
  let
    (_, workspaceBlocks) = createBlocks 0 blocks 
  in
    workspaceBlocks

createBlocks : WorkspaceId -> List (Block {}) -> (WorkspaceId, List (Block (WorkspaceBlock {})))
createBlocks index blocks =
  case blocks of
    block::blocksTail ->
      let
        (nextIndex, workspaceBlock) = createBlock index block
        (nextNextIndex, workspaceBlocks) = createBlocks nextIndex blocksTail 
      in
        (nextNextIndex, workspaceBlock::workspaceBlocks)
    [] ->
      (index, [])

createBlock : WorkspaceId -> Block {} -> (WorkspaceId, Block (WorkspaceBlock {}))
createBlock index block =
  let
    (nextIndex, workspaceContent) = createBlockContents (index+1) block.content
  in
    ( nextIndex
    , { id = index
      , name = block.name
      , category = block.category
      , outputType = block.outputType
      , content = workspaceContent
      }
    )

createBlockContents : WorkspaceId -> List (BlockContent {}) -> (WorkspaceId, List (BlockContent (WorkspaceBlock {})))
createBlockContents index contents =
  case contents of
    content::contentsTail ->
      let
        (nextIndex, workspaceContent) = createBlockContent index content
        (nextNextIndex, workspaceContents) = createBlockContents nextIndex contentsTail 
      in
        (nextNextIndex, workspaceContent::workspaceContents)
    [] ->
      (index, [])

createBlockContent : WorkspaceId -> BlockContent {} -> (WorkspaceId, BlockContent (WorkspaceBlock {}))
createBlockContent index content =
  case content of
    ValueInput input ->
      let
        (nextIndex, workspaceBlock) =
          case input.block of
            Just block ->
              createBlock index block
              |> Tuple.mapSecond Just
            Nothing ->
              (index, Nothing)
      in
        (nextIndex
        , ValueInput 
          { input
          | block = workspaceBlock
          }
        )
    StatementInput input ->
      let
        (nextIndex, workspaceBlocks) = createBlocks index input.blocks
      in
        (nextIndex
        , StatementInput
          { input
          | blocks = workspaceBlocks
          }
        )
    TextField field ->
      (index, TextField field)
    BlockText text ->
      (index, BlockText text)
    BlockNewline ->
      (index, BlockNewline)

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

controlFor =
  { name = "for"
  , category = "control"
  , outputType = BlockType "Statement"
  , content =
    [ BlockText "for "
    , ValueInput
      { id = "variable"
      , types = [BlockType "Variable"]
      , block = Nothing
      , editable = True
      }
    , BlockText " in "
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

codeGen : List (Block a) -> String
codeGen blocks =
  List.map (blockToCode 0) blocks
  |> String.join "\n"

blockToCode : Int -> Block a -> String
blockToCode nestingLevel block =
  case block.name of
    _ ->
      List.map (contentToCode nestingLevel) block.content |> String.join ""

contentToCode : Int -> BlockContent a -> String
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
    

getTextField : Block a -> String -> Maybe String
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

type Msg
  = TextInput WorkspaceId String

update msg model =
  case msg of
    TextInput workspaceId text ->
      ({ model | blocks = List.map (updateWorkspaceId workspaceId (setTextField "text" text)) model.blocks}, Cmd.none)

updateWorkspaceId
  :  WorkspaceId
  -> (Block (WorkspaceBlock a) -> Block (WorkspaceBlock a))
  -> Block (WorkspaceBlock a)
  -> Block (WorkspaceBlock a)
updateWorkspaceId id update block =
  if block.id == id then
    update block
  else
    let
      content = List.map (updateWorkspaceIdContent id update) block.content
    in
      { block
      | content = content
      }

updateWorkspaceIdContent
  :  WorkspaceId
  -> (Block (WorkspaceBlock a) -> Block (WorkspaceBlock a))
  -> BlockContent (WorkspaceBlock a)
  -> BlockContent (WorkspaceBlock a)
updateWorkspaceIdContent id update content =
  case content of
    ValueInput input ->
      ValueInput
        { input
        | block = input.block |> Maybe.map (updateWorkspaceId id update)
        }
    StatementInput input ->
      StatementInput
        { input
        | blocks = input.blocks |> List.map (updateWorkspaceId id update)
        }
    _ ->
      content

setTextField : String -> String -> Block a -> Block a
setTextField fieldId value block =
  let
    updatedContent = 
      block.content
      |> List.map updateContent

    updateContent content =
      case content of
        TextField field ->
          if field.id == fieldId then
            TextField { field | field = Just value }
          else
            TextField field
        _ ->
          content
  in
    { block
    | content = updatedContent
    }

{-
  View
-}

view : Model -> Html Msg
view model =
  Html.div
    []
    [ workspaceView model.blocks
    , codeView model.blocks
    ]

workspaceView : List (Block (WorkspaceBlock a)) -> Html Msg
workspaceView blocks =
  Html.ol
    [ style
      [ ("list-style", "none")
      ]
    ]
    <| List.map (\block -> Html.li [] [ blockView block ]) blocks

blockView : Block (WorkspaceBlock a) -> Html Msg
blockView block =
  Html.div
    [ style
      [ ("display", "inline-block")
      , border 1 px solid white
      , backgroundColor gray
      ]
    ]
    <| List.map (blockContentView block.id) block.content

blockContentView : WorkspaceId -> BlockContent (WorkspaceBlock a) -> Html Msg
blockContentView id content =
  case content of
    ValueInput input ->
      blockInputView input
    StatementInput input ->
      blocksInputView input
    TextField textField ->
      Html.input
        [ onInput (TextInput id)
        ]
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

blockInputView : BlockInput (WorkspaceBlock a) -> Html Msg
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

blocksInputView : BlocksInput (WorkspaceBlock a) -> Html Msg
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

codeView : List (Block a) -> Html Msg
codeView blocks =
  Html.textarea
    [ attribute "rows" "15"
    ]
    [ Html.text
      <| codeGen blocks
    ]
