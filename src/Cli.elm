module Cli exposing (run)

import AssocList
import BackendTask exposing (BackendTask)
import BackendTask.File
import Canonical
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Pages.Script exposing (Script)
import Set exposing (Set)
import Source


run : Script
run =
    Pages.Script.withCliOptions options
        (\{ inputFile } ->
            -- gatherFiles
            --     (AssocList.singleton inputFile ())
            --     AssocList.empty
            parseFile inputFile
                |> BackendTask.andThen Pages.Script.log
        )


type alias CliOptions =
    { inputFile : String
    }


options : Program.Config CliOptions
options =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.requiredPositionalArg "inputFile")
            )



-- gatherFies : AssocList.Dict String () -> AssocList.Dict String Canonical.File -> BackendTask FatalError String


parseFile : String -> BackendTask FatalError String
parseFile filePath =
    BackendTask.File.rawFile filePath
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (Source.parse
                >> Result.mapError Debug.toString
                >> Result.mapError FatalError.fromString
                >> BackendTask.fromResult
            )
        |> BackendTask.andThen
            (Canonical.fromSource
                >> Result.map Debug.toString
                >> Result.mapError Debug.toString
                >> Result.mapError FatalError.fromString
                >> BackendTask.fromResult
            )
