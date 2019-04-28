module Program exposing
    ( Instruction(..)
    , ParseError(..)
    , Program
    , ProgramIndex
    , RegisterIndex
    , parse
    )

import Array exposing (Array)
import List.Extra exposing (find, indexedFoldr)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , int
        , keyword
        , oneOf
        , spaces
        , succeed
        )


type alias Program =
    List Instruction


type alias RegisterIndex =
    Int


type alias ProgramIndex =
    Int


type Instruction
    = Inc RegisterIndex ProgramIndex
    | Deb RegisterIndex ProgramIndex ProgramIndex
    | Halt


type alias LineError =
    ( ProgramIndex, String )


type ParseError
    = SyntaxError (List LineError)
    | InvalidGoto ( ProgramIndex, Instruction )


urmParser : Parser Instruction
urmParser =
    oneOf
        [ succeed Halt
            |. keyword "HALT"
        , succeed Inc
            |. keyword "INC"
            |. spaces
            |= int
            |. spaces
            |= int
        , succeed Deb
            |. keyword "DEB"
            |. spaces
            |= int
            |. spaces
            |= int
            |. spaces
            |= int
        ]


parse : String -> Result ParseError Program
parse rawProgram =
    let
        lines =
            String.split "\n" rawProgram

        ( errors, program ) =
            lines
                |> indexedFoldr parseLine ( [], [] )
    in
    if List.length errors > 0 then
        Err (SyntaxError errors)

    else
        validateProgram program


validateProgram : Program -> Result ParseError Program
validateProgram program =
    let
        maxInstruction =
            numberOfInstructions program

        referencesInstructionOver maxIndex ( _, instruction ) =
            case instruction of
                Inc _ a ->
                    a > maxIndex

                Deb _ a b ->
                    a > maxIndex && b > maxIndex

                Halt ->
                    False

        invalidInstruction =
            program
                |> List.indexedMap (\i instruction -> ( i + 1, instruction ))
                |> find (referencesInstructionOver maxInstruction)
    in
    case invalidInstruction of
        Just invalid ->
            InvalidGoto invalid |> Err

        Nothing ->
            Ok program


parseLine :
    ProgramIndex
    -> String
    -> ( List LineError, List Instruction )
    -> ( List LineError, List Instruction )
parseLine index rawLine ( errors, instructions ) =
    case Parser.run urmParser rawLine of
        Ok instruction ->
            ( errors, instruction :: instructions )

        Err _ ->
            ( ( index + 1, rawLine ) :: errors, instructions )


numberOfInstructions : Program -> Int
numberOfInstructions =
    List.length
