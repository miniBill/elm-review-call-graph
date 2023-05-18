module ExtractCallGraphTest exposing (all)

import ExtractCallGraph exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ExtractCallGraph"
        [ test "Should extract both exposed and unexposed values" <|
            \() ->
                """module A exposing (a)
a = 1

b = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract "{}"
        , test "Should work for loops" <|
            \() ->
                """module A exposing (a)
a = b

b = a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract
                        """
                        {
                            "A.a": [
                                "A.b"
                            ],
                            "A.b": [
                                "A.a"
                            ]
                        }
                        """
        , test "Should work for external functions" <|
            \() ->
                """module A exposing (a)
a = List.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract
                        """
                        {
                            "A.a": [
                                "List.empty"
                            ]
                        }
                        """
        , test "Should work for aliased imports" <|
            \() ->
                """module A exposing (a)

import Element.Border as Border

a = Border.width
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract
                        """
                        {
                            "A.a": [
                                "Element.Border.width"
                            ]
                        }
                        """
        , test "Should not report duplicates" <|
            \() ->
                """module A exposing (a)

import Element.Border as Border

a = [ Border.width, Border.width ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract
                        """
                        {
                            "A.a": [
                                "Element.Border.width"
                            ]
                        }
                        """
        ]
