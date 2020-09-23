module EnumReview exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression(..))
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


startsWithEnumCreate : Expression -> Bool
startsWithEnumCreate expression =
    case expression of
        Application (firstNode :: _) ->
            Node.value firstNode == FunctionOrValue [ "Enum" ] "create"

        _ ->
            False


getTuples : Node Expression -> List ( String, String )
getTuples expr =
    case Node.value expr of
        Application [ _, Node _ (ListExpr list) ] ->
            List.filterMap getTuple list

        _ ->
            []


getTuple : Node Expression -> Maybe ( String, String )
getTuple (Node _ value) =
    case value of
        TupledExpression [ Node _ (Literal stringLiteral), Node _ (FunctionOrValue [] variantName) ] ->
            Just ( stringLiteral, variantName )

        _ ->
            Nothing


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor declaration context =
    {- Here, we are interested in the declarations of the form
       enum : Enum Xyz
       enum = Enum.create [ ... ]
    -}
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            let
                body : Expression
                body =
                    function.declaration |> Node.value |> .expression |> Node.value
            in
            if startsWithEnumCreate body then
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
                                let
                                    tuples : List ( String, String )
                                    tuples =
                                        function.declaration
                                            |> Node.value
                                            |> .expression
                                            |> getTuples

                                    usedConstructors : Set String
                                    usedConstructors =
                                        tuples |> List.map Tuple.second |> Set.fromList

                                    missingConstructors : Set String
                                    missingConstructors =
                                        Set.diff constructors usedConstructors

                                    duplicateConstructors =
                                        tuples |> List.map Tuple.second |> duplicates

                                    duplicateStrings =
                                        tuples |> List.map Tuple.first |> duplicates
                                in
                                if not <| Set.isEmpty missingConstructors then
                                    ( [ Rule.error
                                            { message = "The list passed to Enum.create does not contain all the type constructors for `" ++ typeName ++ "`"
                                            , details =
                                                [ "It is missing the following constructors:"
                                                    ++ (missingConstructors |> Set.toList |> joinLines)
                                                ]
                                            }
                                            (Node.range function.declaration)
                                      ]
                                    , context
                                    )

                                else if not <| List.isEmpty duplicateStrings then
                                    ( [ Rule.error
                                            { message = "The list passed to Enum.create contains duplicate Strings:"
                                            , details = [ duplicateStrings |> List.map (\str -> "\"" ++ str ++ "\"") |> joinLines ]
                                            }
                                            (Node.range function.declaration)
                                      ]
                                    , context
                                    )

                                else if not <| List.isEmpty duplicateConstructors then
                                    ( [ Rule.error
                                            { message = "The list passed to Enum.create contains duplicate constructors:"
                                            , details = [ joinLines duplicateConstructors ]
                                            }
                                            (Node.range function.declaration)
                                      ]
                                    , context
                                    )

                                else
                                    ( []
                                    , context
                                    )

                            Nothing ->
                                ( []
                                , context
                                )

            else
                ( [], context )

        _ ->
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


joinLines : List String -> String
joinLines lines =
    "\n - " ++ String.join "\n - " lines
