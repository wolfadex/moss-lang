module Update exposing (update)

import Action
import Array
import ArrayUtil
import Browser.Dom as Dom
import Cmd.Extra
import Common
    exposing
        ( hoversToPositions
        , lastColumn
        , lastLine
        , recordHistory
        , recordHistoryWithCmd
        , recordHistory_
        , removeCharAfter
        , sanitizeHover
        , stateToSnapshot
        )
import ContextMenu
import Debounce
import EditorModel exposing (AutoLineBreak(..), EditMode(..), EditorModel, VimMode(..))
import EditorMsg exposing (EMsg(..), Hover(..), Selection(..))
import History
import Search
import Task
import Update.File
import Update.Function as Function
import Update.Group
import Update.Scroll
import Update.Wrap
import Vim
import Vim.Update
import Window


update : EMsg -> EditorModel -> ( EditorModel, Cmd EMsg )
update msg model =
    case msg of
        EditorNoOp ->
            ( model, Cmd.none )

        ExitVimInsertMode ->
            ( { model | editMode = VimEditor VimNormal }, Cmd.none )

        Test ->
            Action.goToLine 30 model

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        EditorModel.debounceConfig
                        (Debounce.takeLast Function.unload)
                        msg_
                        model.debounce
            in
            ( { model | debounce = debounce }, cmd )

        Unload _ ->
            ( { model | debounce = model.debounce }, Cmd.none )

        MoveUp ->
            ( Action.cursorUp model
                |> recordHistory_ model
            , Cmd.none
            )

        MoveDown ->
            ( Action.cursorDown model
                |> recordHistory_ model
            , Cmd.none
            )

        MoveLeft ->
            ( Action.cursorLeft model
                |> recordHistory_ model
            , Cmd.none
            )

        MoveRight ->
            ( Action.cursorRight model
                |> recordHistory_ model
            , Cmd.none
            )

        NewLine ->
            (Function.newLine model |> Common.sanitizeHover)
                |> (\m -> ( m, Update.Scroll.jumpToBottom m ))

        InsertChar char ->
            let
                ( debounce, debounceCmd ) =
                    Debounce.push EditorModel.debounceConfig char model.debounce
            in
            ( Function.insertChar model.editMode char { model | debounce = debounce }
            , debounceCmd
            )
                |> recordHistoryWithCmd model

        Indent ->
            ( model
                |> Action.indent
                |> recordHistory_ model
            , Cmd.none
            )

        Deindent ->
            ( model
                |> Action.deIndent
                |> recordHistory_ model
            , Cmd.none
            )

        KillLine ->
            Function.killLine model |> recordHistory

        KillLineAlt ->
            -- TODO: what did I hace in mind here?
            Function.killLine model |> recordHistory

        DeleteLine ->
            Function.deleteLine model |> recordHistory

        Cut ->
            Function.deleteSelection model
                |> recordHistoryWithCmd model

        Copy ->
            Function.copySelection model

        Paste ->
            Function.pasteSelection model
                |> recordHistory

        RemoveCharBefore ->
            Function.deleteSelection model
                |> recordHistoryWithCmd model

        FirstLine ->
            Action.firstLine model

        -- Continuous scroll works, click does not
        Hover hover ->
            Common.sanitizeHover_ hover { model | hover = hover }

        -- Click works, continuous scroll does not
        -- Hover hover -> ( { model | hover = hover } |> Common.sanitizeHover , Cmd.none)
        ViewportMotion res ->
            let
                _ =
                    Debug.log "scrollToTopForElement" res
            in
            ( model, Cmd.none )

        GotViewportInfo r ->
            case r of
                Err e ->
                    let
                        _ =
                            Debug.log "GotViewportInfo" e
                    in
                    ( model, Cmd.none )

                Ok vp ->
                    ( model, Cmd.none )

        GoToHoveredPosition ->
            -- TODO : Current work
            let
                cursor =
                    -- global coordinates
                    case model.hover of
                        NoHover ->
                            model.cursor

                        HoverLine line ->
                            { line = line + model.window.offset, column = lastColumn model.lines (model.window.offset + line) }

                        HoverChar localPosition ->
                            Window.shiftPosition model.window.offset localPosition

                window =
                    Window.shift cursor.line model.window

                deltaOffset =
                    window.offset - model.window.offset |> toFloat

                scrollCmd =
                    if deltaOffset == 0 then
                        Cmd.none

                    else
                        scrollEditor

                scrollEditor =
                    Task.attempt ViewportMotion updateScrollPosition

                newViewportY yvp =
                    yvp - deltaOffset * model.lineHeight

                updateScrollPosition =
                    Dom.getViewportOf "__editor__"
                        |> Task.andThen (\vp -> Dom.setViewportOf "__editor__" 0 (newViewportY vp.viewport.y))
            in
            ( { model
                | cursor = cursor
                , window = window
              }
            , scrollCmd
            )

        LastLine ->
            Action.lastLine model

        AcceptLineToGoTo str ->
            ( { model | lineNumberToGoTo = str }, Cmd.none )

        GoToLine ->
            case String.toInt model.lineNumberToGoTo of
                Nothing ->
                    ( model, Cmd.none )

                Just n ->
                    Action.goToLine n model

        RemoveCharAfter ->
            ( removeCharAfter model
                |> Common.sanitizeHover
            , Cmd.none
            )
                |> recordHistoryWithCmd model

        StartSelecting ->
            ( { model | selection = SelectingFrom model.hover }
            , Cmd.none
            )

        StopSelecting ->
            -- Selection for all other
            let
                endHover =
                    model.hover

                newSelection =
                    case model.selection of
                        NoSelection ->
                            NoSelection

                        SelectingFrom startHover ->
                            if startHover == endHover then
                                case startHover of
                                    NoHover ->
                                        NoSelection

                                    HoverLine _ ->
                                        NoSelection

                                    HoverChar position ->
                                        SelectedChar position

                            else
                                hoversToPositions model.lines startHover endHover
                                    |> Maybe.map (\( from, to ) -> Selection from to)
                                    |> Maybe.withDefault NoSelection

                        SelectedChar _ ->
                            NoSelection

                        Selection _ _ ->
                            NoSelection
            in
            ( { model | selection = newSelection }
            , Cmd.none
            )

        SelectLine ->
            Action.selectLine model

        SelectUp ->
            Action.selectUp model
                |> recordHistory

        SelectDown ->
            Action.selectDown model
                |> recordHistory

        SelectLeft ->
            Action.selectLeft model
                |> recordHistory

        SelectRight ->
            Action.selectRight model
                |> recordHistory

        MoveToLineStart ->
            Action.moveToLineStart model

        MoveToLineEnd ->
            Action.moveToLineEnd model

        PageDown ->
            Action.pageDown model

        PageUp ->
            Action.pageUp model

        Clear ->
            ( { model | lines = Array.fromList [ "" ] }, Cmd.none )

        Undo ->
            case History.undo (Common.stateToSnapshot model) model.history of
                Just ( history, snapshot ) ->
                    ( { model
                        | cursor = snapshot.cursor
                        , selection = snapshot.selection
                        , lines = snapshot.lines
                        , history = history
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Redo ->
            case History.redo (Common.stateToSnapshot model) model.history of
                Just ( history, snapshot ) ->
                    ( { model
                        | cursor = snapshot.cursor
                        , selection = snapshot.selection
                        , lines = snapshot.lines
                        , history = history
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ContextMenuMsg msg_ ->
            let
                ( contextMenu, cmd ) =
                    ContextMenu.update msg_ model.contextMenu
            in
            ( { model | contextMenu = contextMenu }
            , Cmd.map ContextMenuMsg cmd
            )

        Item _ ->
            ( model, Cmd.none )

        WrapSelection ->
            ( Update.Wrap.selection model |> recordHistory_ model, Cmd.none )

        WrapAll ->
            ( Update.Wrap.all model |> recordHistory_ model, Cmd.none )

        ToggleAutoLineBreak ->
            case model.autoLineBreak of
                AutoLineBreakOFF ->
                    ( { model | autoLineBreak = AutoLineBreakON }, Cmd.none )

                AutoLineBreakON ->
                    ( { model | autoLineBreak = AutoLineBreakOFF }, Cmd.none )

        EditorRequestFile ->
            ( model, Update.File.requestMarkdownFile )

        EditorRequestedFile file ->
            ( model, Update.File.read file )

        MarkdownLoaded str ->
            ( { model | lines = str |> String.lines |> Array.fromList }, Cmd.none )

        EditorSaveFile ->
            let
                markdown =
                    model.lines
                        |> Array.toList
                        |> String.join "\n"
            in
            ( model, Update.File.save markdown )

        SendLineForLRSync ->
            {- DOC scroll LR entry point (1) -}
            Update.Scroll.sendLine model

        GotViewportForSync _ selection result ->
            case result of
                Ok vp ->
                    let
                        y =
                            vp.viewport.y

                        lineNumber =
                            round (y / model.lineHeight)
                    in
                    ( { model | topLine = lineNumber, selection = selection }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CopyPasteClipboard ->
            {- The msg CopyPasteClipboard is detected and acted upon by the
               host app's update function.
            -}
            ( model, Cmd.none )

        WriteToSystemClipBoard ->
            {- The msg WriteToSystemClipBoard is detected and acted upon by the
               host app's update function.
            -}
            case model.selection of
                Selection p1 p2 ->
                    let
                        selectedString =
                            ArrayUtil.between p1 p2 model.lines

                        newModel =
                            { model | selectedString = Just selectedString }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DoSearch key ->
            ( Search.do key model, Cmd.none )

        ToggleSearchPanel ->
            -- TODO: FOCUS ON "editor-search-box"
            let
                focusSearchBox : Cmd EMsg
                focusSearchBox =
                    Task.attempt (\_ -> EditorNoOp) (Dom.focus "editor-search-box")
            in
            ( { model | showSearchPanel = not model.showSearchPanel }, focusSearchBox )

        ToggleReplacePanel ->
            ( { model | canReplace = not model.canReplace }, Cmd.none )

        OpenReplaceField ->
            ( { model | canReplace = True }, Cmd.none )

        RollSearchSelectionForward ->
            Update.Scroll.rollSearchSelectionForward model

        RollSearchSelectionBackward ->
            Update.Scroll.rollSearchSelectionBackward model

        AcceptReplacementText str ->
            ( { model | replacementText = str }, Cmd.none )

        ReplaceCurrentSelection ->
            case model.selection of
                Selection _ to ->
                    let
                        newLines =
                            ArrayUtil.replace model.cursor to model.replacementText model.lines
                    in
                    Update.Scroll.rollSearchSelectionForward { model | lines = newLines }

                -- TODO: FIX THIS
                -- |> recordHistory
                _ ->
                    ( model, Cmd.none )

        AcceptLineNumber _ ->
            ( model, Cmd.none )

        AcceptSearchText str ->
            Update.Scroll.toString str model

        GotViewport result ->
            -- TODO
            case result of
                Ok vp ->
                    let
                        y =
                            vp.viewport.y

                        lineNumber =
                            round (y / model.lineHeight)
                    in
                    ( { model | topLine = lineNumber }, Cmd.none )

                Err e ->
                    let
                        _ =
                            Debug.log "GotViewportForSync" e
                    in
                    ( model, Cmd.none )

        ToggleDarkMode ->
            ( Function.toggleViewMode model, Cmd.none )

        ToggleHelp ->
            ( Function.toggleHelpState model, Cmd.none )

        ToggleEditMode ->
            ( Function.toggleEditMode model, Cmd.none )

        ToggleShortCutExecution ->
            Vim.Update.toggleShortCutExecution model

        MarkdownMsg _ ->
            ( model, Cmd.none )

        SelectGroup ->
            let
                range =
                    Update.Group.groupRange model.cursor model.lines

                line =
                    model.cursor.line
            in
            case range of
                Just ( start, end ) ->
                    ( { model
                        | cursor = { line = line, column = end }
                        , selection = Selection { line = line, column = start } { line = line, column = end }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Vim vimMsg ->
            ( { model | vimModel = Vim.Update.update vimMsg model.vimModel }, Cmd.none )
