module ExtractCallGraph exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import FastDict as Dict exposing (Dict)
import Json.Encode exposing (Value)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Extracts the call graph for an application

    config =
        [ ExtractCallGraph.rule
        ]


## Fail/Success

This rule does not report errors.


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template miniBill/elm-review-call-graph/example --extract --report=json | jq ".extracts.ExtractCallGraph" > call-graph.json
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "ExtractCallGraph" Dict.empty
        |> Rule.withModuleVisitor (Rule.withDeclarationEnterVisitor declarationVisitor)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDataExtractor extractor
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    Dict String (Set String)


type alias ModuleContext =
    { moduleNameLookupTable : ModuleNameLookupTable
    , moduleName : String
    , callGraph : Dict String (Set String)
    }



-- Project/Module Context conversion


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\moduleNameLookupTable moduleName _ ->
            { moduleNameLookupTable = moduleNameLookupTable
            , moduleName = String.join "." moduleName
            , callGraph = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withModuleName


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator .callGraph


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts left right =
    Dict.union left right



-- Declaration visitor


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor (Node _ declaration) context =
    case declaration of
        FunctionDeclaration function ->
            ( [], visitFunction context.moduleName function context )

        _ ->
            ( [], context )


visitFunction : String -> Function -> ModuleContext -> ModuleContext
visitFunction namespace function context =
    let
        declaration : FunctionImplementation
        declaration =
            Node.value function.declaration

        newNamespace : String
        newNamespace =
            namespace ++ "." ++ Node.value declaration.name
    in
    visitExpression newNamespace declaration.expression context


visitExpression : String -> Node Expression -> ModuleContext -> ModuleContext
visitExpression namespace ((Node _ expression) as expressionNode) context =
    case expression of
        FunctionOrValue _ name ->
            case ModuleNameLookupTable.fullModuleNameFor context.moduleNameLookupTable expressionNode of
                Nothing ->
                    context

                Just fullModuleName ->
                    { context
                        | callGraph =
                            upsert namespace
                                (String.join "." fullModuleName
                                    ++ "."
                                    ++ name
                                )
                                context.callGraph
                    }

        IfBlock c t f ->
            visitExpressions namespace [ c, t, f ] context

        OperatorApplication _ _ l r ->
            visitExpressions namespace [ l, r ] context

        Application children ->
            visitExpressions namespace children context

        TupledExpression children ->
            visitExpressions namespace children context

        ListExpr children ->
            visitExpressions namespace children context

        Negation child ->
            visitExpression namespace child context

        ParenthesizedExpression child ->
            visitExpression namespace child context

        RecordAccess child _ ->
            visitExpression namespace child context

        LetExpression letBlock ->
            visitLetBlock namespace letBlock context

        CaseExpression caseBlock ->
            visitCaseBlock namespace caseBlock context

        LambdaExpression lambda ->
            visitLambda namespace lambda context

        RecordExpr recordSetters ->
            visitRecordSetters namespace recordSetters context

        RecordUpdateExpression _ recordSetters ->
            visitRecordSetters namespace recordSetters context

        _ ->
            context


visitRecordSetters : String -> List (Node Elm.Syntax.Expression.RecordSetter) -> ModuleContext -> ModuleContext
visitRecordSetters namespace recordSetters context =
    List.foldl
        (\(Node _ ( _, expression )) -> visitExpression namespace expression)
        context
        recordSetters


visitLambda : String -> Lambda -> ModuleContext -> ModuleContext
visitLambda namespace { expression } context =
    visitExpression namespace expression context


visitCaseBlock : String -> CaseBlock -> ModuleContext -> ModuleContext
visitCaseBlock namespace caseBlock context =
    List.foldl
        (visitCase namespace)
        (visitExpression namespace caseBlock.expression context)
        caseBlock.cases


visitCase : String -> Case -> ModuleContext -> ModuleContext
visitCase namespace ( _, expression ) context =
    visitExpression namespace expression context


visitLetBlock : String -> LetBlock -> ModuleContext -> ModuleContext
visitLetBlock namespace { declarations, expression } context =
    List.foldl (visitLetDeclaration namespace)
        (visitExpression namespace expression context)
        declarations


visitLetDeclaration : String -> Node LetDeclaration -> ModuleContext -> ModuleContext
visitLetDeclaration namespace (Node _ letDeclaration) context =
    case letDeclaration of
        LetDestructuring _ expression ->
            visitExpression namespace expression context

        LetFunction function ->
            -- TODO: use visitFunction and keep track of locally defined functions
            visitExpression namespace (Node.value function.declaration).expression context


visitExpressions : String -> List (Node Expression) -> ModuleContext -> ModuleContext
visitExpressions namespace expressions context =
    List.foldl (visitExpression namespace) context expressions


upsert : comparable1 -> comparable2 -> Dict comparable1 (Set comparable2) -> Dict comparable1 (Set comparable2)
upsert key value dict =
    Dict.insert key (Set.insert value <| Maybe.withDefault Set.empty (Dict.get key dict)) dict



-- Extractor


extractor : ProjectContext -> Value
extractor context =
    context
        |> Dict.toCoreDict
        |> Json.Encode.dict identity
            (Set.toList >> Json.Encode.list Json.Encode.string)
