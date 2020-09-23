module Tests exposing (..)

import EnumReview exposing (rule)
import Review.Test
import Test exposing (..)


all : Test
all =
    describe "Enum"
        [ test "valid" <|
            \_ ->
                """
module A exposing (..)
type Fruit = Apple | Banana | Mango
enum : Enum Fruit
enum = Enum.create [ ("Apple", Apple), ("Banana", Banana), ("Mango", Mango) ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors

        --
        , test "empty list" <|
            \_ ->
                """
module A exposing (..)
type Fruit = Apple | Banana | Mango
enum : Enum Fruit
enum = Enum.create [ ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The list passed to Enum.create does not contain all the type constructors for `Fruit`"
                            , details = [ "It is missing the following constructors:", "Apple", "Banana", "Mango" ]
                            , under = """enum = Enum.create [ ]"""
                            }
                        ]

        --
        , test "missing type signature" <|
            \_ ->
                """
module A exposing (..)
type Fruit = Apple | Banana | Mango
enum = Enum.create [ ("Apple", Apple), ("Banana", Banana), ("Mango", Mango) ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Enum definition is missing a type annotation"
                            , details = [ "Without a type annotation, I don't know which type to check against for exhaustiveness" ]
                            , under = """enum = Enum.create [ ("Apple", Apple), ("Banana", Banana), ("Mango", Mango) ]"""
                            }
                        ]

        --
        , test "missing Banana and Mango" <|
            \_ ->
                """
module A exposing (..)
type Fruit = Apple | Banana | Mango
enum : Enum Fruit
enum = Enum.create [ ("Apple", Apple) ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The list passed to Enum.create does not contain all the type constructors for `Fruit`"
                            , details = [ "It is missing the following constructors:", "Banana", "Mango" ]
                            , under = """enum = Enum.create [ ("Apple", Apple) ]"""
                            }
                        ]
        , test "duplicate constructor" <|
            \_ ->
                """
module A exposing (..)
type Fruit = Apple | Banana | Mango
enum : Enum Fruit
enum = Enum.create [ ("Apple", Apple), ("Banana", Banana), ("Mango", Mango), ("Mango2", Mango) ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The list passed to Enum.create contains duplicate constructors:"
                            , details = [ "Mango" ]
                            , under = """enum = Enum.create [ ("Apple", Apple), ("Banana", Banana), ("Mango", Mango), ("Mango2", Mango) ]"""
                            }
                        ]

        --
        , test "duplicate string" <|
            \_ ->
                """
module A exposing (..)
type Fruit = Apple | Banana | Mango
enum : Enum Fruit
enum = Enum.create [ ("Apple", Apple), ("Banana", Banana), ("Banana", Mango) ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The list passed to Enum.create contains duplicate Strings:"
                            , details = [ "\"Banana\"" ]
                            , under = """enum = Enum.create [ ("Apple", Apple), ("Banana", Banana), ("Banana", Mango) ]"""
                            }
                        ]

        --
        , test "pipe, missing Mango" <|
            \_ ->
                """
module A exposing (..)
type Fruit = Apple | Banana | Mango
enum : Enum Fruit
enum = [ ("Apple", Apple), ("Banana", Banana) ] |> Enum.create
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The list passed to Enum.create does not contain all the type constructors for `Fruit`"
                            , details = [ "It is missing the following constructors:", "Mango" ]
                            , under = """enum = [ ("Apple", Apple), ("Banana", Banana) ] |> Enum.create"""
                            }
                        ]
        ]
