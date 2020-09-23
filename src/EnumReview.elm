module EnumReview exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression(..), Function)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newProjectRuleSchema "CorrectEnumDefinitions" initialContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            , customTypes = projectContext.customTypes
            , localTypes = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { customTypes =
                Dict.singleton
                    (Rule.moduleNameFromMetadata metadata)
                    moduleContext.localTypes
            }
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { customTypes = Dict.union newContext.customTypes previousContext.customTypes }


type Constructors
    = ConstructorsWithoutArguments (Set String)
    | ConstructorsWithArguments


type alias ProjectContext =
    { customTypes : Dict ModuleName (Dict String Constructors)
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , customTypes : Dict ModuleName (Dict String Constructors)
    , localTypes : Dict String Constructors
    }


initialContext : ProjectContext
initialContext =
    { customTypes = Dict.empty
    }



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor declarations context =
    -- Here we wish to find the custom types that were defined in the module, and store them in the context.
    ( []
    , { context
        | localTypes =
            declarations
                |> List.filterMap getCustomType
                |> List.map typeConstructors
                |> Dict.fromList
      }
    )


typeConstructors : Type -> ( String, Constructors )
typeConstructors type_ =
    ( Node.value type_.name
    , if List.all (Node.value >> .arguments >> List.isEmpty) type_.constructors then
        type_.constructors
            |> List.map (Node.value >> .name >> Node.value)
            |> Set.fromList
            |> ConstructorsWithoutArguments

      else
        ConstructorsWithArguments
    )


getCustomType : Node Declaration -> Maybe Type
getCustomType node =
    case Node.value node of
        Declaration.CustomTypeDeclaration type_ ->
            Just type_

        _ ->
            Nothing


type StringOrInt
    = String String
    | Int Int


getTuples : Expression -> Maybe (List ( StringOrInt, String ))
getTuples expr =
    case expr of
        ListExpr list ->
            List.map getTuple list |> maybeCombine

        _ ->
            Nothing


getTuple : Node Expression -> Maybe ( StringOrInt, String )
getTuple (Node _ value) =
    case value of
        TupledExpression [ Node _ key, Node _ (FunctionOrValue [] variantName) ] ->
            case key of
                Literal string ->
                    Just ( String string, variantName )

                Integer int ->
                    Just ( Int int, variantName )

                _ ->
                    Nothing

        _ ->
            Nothing


findEnumCreate : ModuleNameLookupTable -> Node Declaration -> Maybe ( Function, Expression )
findEnumCreate lookupTable declaration =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            case function.declaration |> Node.value |> .expression |> Node.value of
                Application (calledFunction :: [ Node _ list ]) ->
                    -- TODO Need to report an error if there are no arguments to the call
                    if isCreateFunction lookupTable calledFunction then
                        Just ( function, list )

                    else
                        Nothing

                OperatorApplication "|>" Left (Node _ list) rightExpression ->
                    if isCreateFunction lookupTable rightExpression then
                        Just ( function, list )

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


isCreateFunction : ModuleNameLookupTable -> Node Expression -> Bool
isCreateFunction lookupTable node =
    case Node.value node of
        Expression.FunctionOrValue _ name ->
            if name == "create" || name == "createInt" then
                case ModuleNameLookupTable.moduleNameFor lookupTable node of
                    Just [ "Enum" ] ->
                        True

                    _ ->
                        False

            else
                False

        _ ->
            False


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationVisitor declaration context =
    case findEnumCreate context.lookupTable declaration of
        Just ( function, list ) ->
            case getTypeName function of
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
                    case Dict.get typeName context.localTypes of
                        Just (ConstructorsWithoutArguments constructors) ->
                            case getTuples list of
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
                                            tuples
                                                |> List.filterMap
                                                    (\( key, _ ) ->
                                                        case key of
                                                            String str ->
                                                                Just str

                                                            Int _ ->
                                                                Nothing
                                                    )
                                                |> duplicates

                                        duplicateInts =
                                            tuples
                                                |> List.filterMap
                                                    (\( key, _ ) ->
                                                        case key of
                                                            Int int ->
                                                                Just int

                                                            String _ ->
                                                                Nothing
                                                    )
                                                |> duplicates
                                    in
                                    if missingConstructors /= [] then
                                        ( [ Rule.error
                                                { message = "The list passed to Enum.create does not contain all the type constructors for `" ++ typeName ++ "`"
                                                , details = "It is missing the following constructors:" :: missingConstructors
                                                }
                                                (Node.range function.declaration)
                                          ]
                                        , context
                                        )

                                    else if duplicateStrings /= [] then
                                        ( [ Rule.error
                                                { message = "The list passed to Enum.create contains duplicate Strings:"
                                                , details = List.map (\str -> "\"" ++ str ++ "\"") duplicateStrings
                                                }
                                                (Node.range function.declaration)
                                          ]
                                        , context
                                        )

                                    else if duplicateInts /= [] then
                                        ( [ Rule.error
                                                { message = "The list passed to Enum.createInt contains duplicate Ints:"
                                                , details = List.map String.fromInt duplicateInts
                                                }
                                                (Node.range function.declaration)
                                          ]
                                        , context
                                        )

                                    else if duplicateConstructors /= [] then
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
                                    ( [ Rule.error
                                            { message = "Unexpected structure"
                                            , details = [ ":(" ]
                                            }
                                            (Node.range function.declaration)
                                      ]
                                    , context
                                    )

                        Just ConstructorsWithArguments ->
                            ( [ Rule.error
                                    { message = "One of the variants of `" ++ typeName ++ "` has an argument"
                                    , details = [ "Enum.create is intended to only be used with simple enum-like types" ]
                                    }
                                    (Node.range function.declaration)
                              ]
                            , context
                            )

                        Nothing ->
                            -- couldn't find type definition
                            ( [ Rule.error
                                    { message = "Couldn't find the type definition for `" ++ typeName ++ "`"
                                    , details = [ "Where did you hide it?" ]
                                    }
                                    (Node.range function.declaration)
                              ]
                            , context
                            )

        Nothing ->
            -- ignore, not an Enum definition
            ( [], context )


getTypeName : Expression.Function -> Maybe String
getTypeName function =
    function.signature
        |> Maybe.map (Node.value >> .typeAnnotation >> Node.value)
        |> Maybe.andThen
            (\annotation ->
                case annotation of
                    TypeAnnotation.Typed typeNode (parameterNode :: []) ->
                        case ( Node.value typeNode, Node.value parameterNode ) of
                            ( ( [], enumType ), TypeAnnotation.Typed parameter _ ) ->
                                -- TODO Handle multiple ways of importing Enum/EnumInt
                                if enumType == "Enum" || enumType == "EnumInt" then
                                    case Node.value parameter of
                                        ( [], typeName ) ->
                                            Just typeName

                                        _ ->
                                            Nothing

                                else
                                    Nothing

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )


maybeCombine : List (Maybe a) -> Maybe (List a)
maybeCombine =
    List.foldr (Maybe.map2 (::)) (Just [])


duplicates : List comparable -> List comparable
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
