module Cli exposing (run)

import BackendTask
import BackendTask.File
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError
import Pages.Script exposing (Script)
import Set exposing (Set)
import Source


run : Script
run =
    Pages.Script.withCliOptions options
        (\{ inputFile } ->
            BackendTask.File.rawFile inputFile
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (Source.parse
                        >> Result.map Debug.toString
                        >> Result.mapError Debug.toString
                        >> Result.mapError FatalError.fromString
                        >> BackendTask.fromResult
                    )
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
