module ExtractCallGraph exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
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


type alias Ignored =
    Set String



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
            let
                name : String
                name =
                    Node.value (Node.value function.declaration).name

                namespace : String
                namespace =
                    context.moduleName ++ "." ++ name
            in
            ( [], visitFunction namespace Set.empty function context )

        _ ->
            ( [], context )


visitFunction : String -> Ignored -> Function -> ModuleContext -> ModuleContext
visitFunction namespace ignored function context =
    let
        declaration : FunctionImplementation
        declaration =
            Node.value function.declaration

        newIgnored : Ignored
        newIgnored =
            extractNamesFromPatterns declaration.arguments ignored
                |> Set.insert (Node.value declaration.name)
    in
    visitExpression namespace newIgnored declaration.expression context


extractNamesFromPatterns : List (Node Pattern) -> Set String -> Set String
extractNamesFromPatterns patterns set =
    List.foldl extractNamesFromPattern set patterns


extractNamesFromPattern : Node Pattern -> Set String -> Set String
extractNamesFromPattern (Node _ pattern) set =
    case pattern of
        VarPattern v ->
            Set.insert v set

        RecordPattern fields ->
            List.foldl (\(Node _ field) -> Set.insert field) set fields

        UnConsPattern head tail ->
            extractNamesFromPatterns [ head, tail ] set

        ListPattern children ->
            extractNamesFromPatterns children set

        TuplePattern children ->
            extractNamesFromPatterns children set

        NamedPattern _ children ->
            extractNamesFromPatterns children set

        AsPattern child (Node _ var) ->
            extractNamesFromPattern child (Set.insert var set)

        ParenthesizedPattern child ->
            extractNamesFromPattern child set

        _ ->
            set


visitExpression : String -> Ignored -> Node Expression -> ModuleContext -> ModuleContext
visitExpression namespace ignored ((Node _ expression) as expressionNode) context =
    case expression of
        FunctionOrValue moduleName name ->
            case ModuleNameLookupTable.fullModuleNameFor context.moduleNameLookupTable expressionNode of
                Nothing ->
                    context

                Just fullModuleName ->
                    if List.isEmpty moduleName && Set.member name ignored then
                        context

                    else
                        let
                            fullName : String
                            fullName =
                                String.join "." fullModuleName
                                    ++ "."
                                    ++ name
                        in
                        { context
                            | callGraph =
                                upsert namespace
                                    fullName
                                    context.callGraph
                        }

        IfBlock c t f ->
            visitExpressions namespace ignored [ c, t, f ] context

        OperatorApplication _ _ l r ->
            visitExpressions namespace ignored [ l, r ] context

        Application children ->
            visitExpressions namespace ignored children context

        TupledExpression children ->
            visitExpressions namespace ignored children context

        ListExpr children ->
            visitExpressions namespace ignored children context

        Negation child ->
            visitExpression namespace ignored child context

        ParenthesizedExpression child ->
            visitExpression namespace ignored child context

        RecordAccess child _ ->
            visitExpression namespace ignored child context

        LetExpression letBlock ->
            visitLetBlock namespace ignored letBlock context

        CaseExpression caseBlock ->
            visitCaseBlock namespace ignored caseBlock context

        LambdaExpression lambda ->
            visitLambda namespace ignored lambda context

        RecordExpr recordSetters ->
            visitRecordSetters namespace ignored recordSetters context

        RecordUpdateExpression _ recordSetters ->
            visitRecordSetters namespace ignored recordSetters context

        _ ->
            context


visitRecordSetters : String -> Ignored -> List (Node Elm.Syntax.Expression.RecordSetter) -> ModuleContext -> ModuleContext
visitRecordSetters namespace ignored recordSetters context =
    List.foldl
        (\(Node _ ( _, expression )) -> visitExpression namespace ignored expression)
        context
        recordSetters


visitLambda : String -> Ignored -> Lambda -> ModuleContext -> ModuleContext
visitLambda namespace ignored { expression } context =
    visitExpression namespace ignored expression context


visitCaseBlock : String -> Ignored -> CaseBlock -> ModuleContext -> ModuleContext
visitCaseBlock namespace ignored caseBlock context =
    List.foldl
        (visitCase namespace ignored)
        (visitExpression namespace ignored caseBlock.expression context)
        caseBlock.cases


visitCase : String -> Ignored -> Case -> ModuleContext -> ModuleContext
visitCase namespace ignored ( pattern, expression ) context =
    visitExpression namespace
        (extractNamesFromPattern pattern ignored)
        expression
        context


visitLetBlock : String -> Ignored -> LetBlock -> ModuleContext -> ModuleContext
visitLetBlock namespace ignored { declarations, expression } context =
    let
        newIgnored : Ignored
        newIgnored =
            List.foldl extractNamesFromLetDeclaration ignored declarations

        contextAfterDeclarations : ModuleContext
        contextAfterDeclarations =
            List.foldl
                (visitLetDeclaration namespace newIgnored)
                context
                declarations
    in
    visitExpression namespace newIgnored expression contextAfterDeclarations


extractNamesFromLetDeclaration : Node LetDeclaration -> Ignored -> Ignored
extractNamesFromLetDeclaration (Node _ letDeclaration) ignored =
    case letDeclaration of
        LetDestructuring pattern _ ->
            extractNamesFromPattern pattern ignored

        LetFunction function ->
            let
                name : String
                name =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            Set.insert name ignored


visitLetDeclaration : String -> Ignored -> Node LetDeclaration -> ModuleContext -> ModuleContext
visitLetDeclaration namespace ignored (Node _ letDeclaration) context =
    case letDeclaration of
        LetDestructuring pattern expression ->
            let
                newIgnored : Set String
                newIgnored =
                    extractNamesFromPattern pattern ignored
            in
            visitExpression namespace
                newIgnored
                expression
                context

        LetFunction function ->
            visitFunction namespace ignored function context


visitExpressions : String -> Ignored -> List (Node Expression) -> ModuleContext -> ModuleContext
visitExpressions namespace ignored expressions context =
    List.foldl (visitExpression namespace ignored) context expressions


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
