module ProgramTest exposing (parseTest)

import Expect
import Program
    exposing
        ( Instruction(..)
        , ParseError(..)
        )
import Test exposing (..)


lines : List String -> String
lines =
    String.join "\n"


parseTest : Test
parseTest =
    describe "parse"
        [ test "single invalid instruction" <|
            \_ ->
                "FOO 1 2 3"
                    |> Program.parse
                    |> Expect.equal (Err (SyntaxError [ ( 1, "FOO 1 2 3" ) ]))
        , test "two invalid instructions" <|
            \_ ->
                lines [ "FOO 1 2 3", "BAR" ]
                    |> Program.parse
                    |> Expect.equal
                        (Err
                            (SyntaxError
                                [ ( 1, "FOO 1 2 3" )
                                , ( 2, "BAR" )
                                ]
                            )
                        )
        , test "mixture of valid and invalid instructions" <|
            \_ ->
                lines
                    [ "INC 1 2"
                    , "BAR"
                    , "HALT"
                    ]
                    |> Program.parse
                    |> Expect.equal (Err (SyntaxError [ ( 2, "BAR" ) ]))
        , test "single HALT instruction" <|
            \_ ->
                "HALT"
                    |> Program.parse
                    |> Expect.equal (Ok [ Halt ])
        , test "INC without register" <|
            \_ ->
                "INC"
                    |> Program.parse
                    |> Expect.equal (Err (SyntaxError [ ( 1, "INC" ) ]))
        , test "INC without next instruction" <|
            \_ ->
                "INC 1"
                    |> Program.parse
                    |> Expect.equal (Err (SyntaxError [ ( 1, "INC 1" ) ]))
        , test "INC instruction" <|
            \_ ->
                lines
                    [ "INC 1 2"
                    , "HALT"
                    ]
                    |> Program.parse
                    |> Expect.equal (Ok [ Inc 1 2, Halt ])
        , test "DEB without register" <|
            \_ ->
                "DEB"
                    |> Program.parse
                    |> Expect.equal (Err (SyntaxError [ ( 1, "DEB" ) ]))
        , test "DEB without next instruction" <|
            \_ ->
                "DEB 1"
                    |> Program.parse
                    |> Expect.equal (Err (SyntaxError [ ( 1, "DEB 1" ) ]))
        , test "DEB without branch instruction" <|
            \_ ->
                "DEB 1 2"
                    |> Program.parse
                    |> Expect.equal (Err (SyntaxError [ ( 1, "DEB 1 2" ) ]))
        , test "DEB instruction" <|
            \_ ->
                lines
                    [ "DEB 1 2 3"
                    , "HALT"
                    ]
                    |> Program.parse
                    |> Expect.equal (Ok [ Deb 1 2 3, Halt ])
        , test "valid program with each instruction" <|
            \_ ->
                lines
                    [ "INC 1 2"
                    , "DEB 1 2 3"
                    , "HALT"
                    ]
                    |> Program.parse
                    |> Expect.equal
                        (Ok
                            [ Inc 1 2
                            , Deb 1 2 3
                            , Halt
                            ]
                        )
        , test "INC instruction referring to non existent instruction" <|
            \_ ->
                lines
                    [ "INC 1 4"
                    , "HALT"
                    ]
                    |> Program.parse
                    |> Expect.equal (Err (InvalidGoto ( 1, Inc 1 4 )))
        ]
