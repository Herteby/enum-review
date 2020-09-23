module EnumReview exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression(..), Function)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newModuleRuleSchema "CorrectEnumDefinitions" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { customTypes : Dict String (Set String)
    }


initialContext : Context
initialContext =
    { customTypes = Dict.empty
    }



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor declarations context =
    -- Here we wish to find the custom types that were defined in the module, and store them in the context.
    ( []
    , { context
        | customTypes =
            declarations
                |> List.filterMap getCustomType
                |> List.map (\type_ -> ( Node.value type_.name, typeConstructors type_ ))
                |> Dict.fromList
      }
    )


typeConstructors : Type -> Set String
typeConstructors type_ =
    type_.constructors
        |> List.map (Node.value >> .name >> Node.value)
        |> Set.fromList


getCustomType : Node Declaration -> Maybe Type
getCustomType node =
    case Node.value node of
        Declaration.CustomTypeDeclaration type_ ->
            Just type_

        _ ->
            Nothing



-- DECLARATION VISITOR


getTuples : Node Expression -> Maybe (List ( String, String ))
getTuples expr =
    case Node.value expr of
        Application [ _, Node _ (ListExpr list) ] ->
            List.map getTuple list |> combine

        _ ->
            Nothing


getTuple : Node Expression -> Maybe ( String, String )
getTuple (Node _ value) =
    case value of
        TupledExpression [ Node _ (Literal stringLiteral), Node _ (FunctionOrValue [] variantName) ] ->
            Just ( stringLiteral, variantName )

        _ ->
            Nothing


combine : List (Maybe a) -> Maybe (List a)
combine =
    List.foldr (Maybe.map2 (::)) (Just [])


o =
    OperatorApplication "|>"
        Left
        (Node { end = { column = 48, row = 5 }, start = { column = 8, row = 5 } } (ListExpr [ Node { end = { column = 26, row = 5 }, start = { column = 10, row = 5 } } (TupledExpression [ Node { end = { column = 18, row = 5 }, start = { column = 11, row = 5 } } (Literal "Apple"), Node { end = { column = 25, row = 5 }, start = { column = 20, row = 5 } } (FunctionOrValue [] "Apple") ]), Node { end = { column = 46, row = 5 }, start = { column = 28, row = 5 } } (TupledExpression [ Node { end = { column = 37, row = 5 }, start = { column = 29, row = 5 } } (Literal "Banana"), Node { end = { column = 45, row = 5 }, start = { column = 39, row = 5 } } (FunctionOrValue [] "Banana") ]) ]))
        (Node { end = { column = 63, row = 5 }, start = { column = 52, row = 5 } } (FunctionOrValue [ "Enum" ] "create"))


findEnumCreate : Node Declaration -> Maybe Function
findEnumCreate declaration =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            case function.declaration |> Node.value |> .expression |> Node.value of
                Application ((Node _ (FunctionOrValue [ "Enum" ] "create")) :: _) ->
                    Just function

                OperatorApplication "|>" Left (Node _ list) (Node _ (FunctionOrValue [ "Enum" ] "create")) ->
                    Just function

                other ->
                    let
                        _ =
                            Debug.log "other" other
                    in
                    Nothing

        _ ->
            Nothing


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor declaration context =
    case findEnumCreate declaration of
        Just function ->
            case getTypeAnnotation function |> Maybe.andThen getListOfTypeAnnotation of
                Nothing ->
                    ( [ Rule.error
                            { message = "Enum definition is missing a type annotation"
                            , details = [ "Without a type annotation, I don't know which type to check against for exhaustiveness" ]
                            }
                            (Node.range function.declaration)
                      ]
                    , context
                    )

                Just typeName ->
                    case Dict.get typeName context.customTypes of
                        Just constructors ->
                            case
                                function.declaration
                                    |> Node.value
                                    |> .expression
                                    |> getTuples
                            of
                                Just tuples ->
                                    let
                                        missingConstructors =
                                            tuples
                                                |> List.map Tuple.second
                                                |> Set.fromList
                                                |> Set.diff constructors
                                                |> Set.toList

                                        duplicateConstructors =
                                            tuples |> List.map Tuple.second |> duplicates

                                        duplicateStrings =
                                            tuples |> List.map Tuple.first |> duplicates
                                    in
                                    if not <| List.isEmpty missingConstructors then
                                        ( [ Rule.error
                                                { message = "The list passed to Enum.create does not contain all the type constructors for `" ++ typeName ++ "`"
                                                , details = "It is missing the following constructors:" :: missingConstructors
                                                }
                                                (Node.range function.declaration)
                                          ]
                                        , context
                                        )

                                    else if not <| List.isEmpty duplicateStrings then
                                        ( [ Rule.error
                                                { message = "The list passed to Enum.create contains duplicate Strings:"
                                                , details = List.map (\str -> "\"" ++ str ++ "\"") duplicateStrings
                                                }
                                                (Node.range function.declaration)
                                          ]
                                        , context
                                        )

                                    else if not <| List.isEmpty duplicateConstructors then
                                        ( [ Rule.error
                                                { message = "The list passed to Enum.create contains duplicate constructors:"
                                                , details = duplicateConstructors
                                                }
                                                (Node.range function.declaration)
                                          ]
                                        , context
                                        )

                                    else
                                        --valid
                                        ( [], context )

                                Nothing ->
                                    --unexpected structure, couldn't find tuple list
                                    ( [], context )

                        Nothing ->
                            -- couldn't find type definition
                            ( [], context )

        Nothing ->
            -- ignore, not an Enum definition
            ( [], context )


getTypeAnnotation : Expression.Function -> Maybe TypeAnnotation
getTypeAnnotation function =
    function.signature
        |> Maybe.map (Node.value >> .typeAnnotation >> Node.value)


getListOfTypeAnnotation : TypeAnnotation -> Maybe String
getListOfTypeAnnotation typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.Typed typeNode (parameterNode :: []) ->
            case ( Node.value typeNode, Node.value parameterNode ) of
                ( ( [], "Enum" ), TypeAnnotation.Typed parameter _ ) ->
                    case Node.value parameter of
                        ( [], typeName ) ->
                            Just typeName

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


duplicates : List String -> List String
duplicates list =
    let
        recurse dupes l =
            case l of
                a :: b :: rest ->
                    recurse
                        (if a == b then
                            Set.insert a dupes

                         else
                            dupes
                        )
                        (b :: rest)

                _ ->
                    dupes
    in
    recurse Set.empty (List.sort list) |> Set.toList
