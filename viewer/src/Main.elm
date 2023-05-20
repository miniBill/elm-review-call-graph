module Main exposing (main)

import Browser
import Dagre.Attributes
import Dict exposing (Dict)
import Element exposing (Element, alignTop, column, el, fill, padding, paddingEach, row, spacing, text, width, wrappedRow)
import Element.Border as Border
import Element.Input as Input
import Graph
import Graph.DOT
import Html
import Json.Decode
import List.Extra
import Render
import Render.StandardDrawers
import Render.StandardDrawers.Attributes
import Render.Types exposing (NodeDrawer)
import Set exposing (Set)


type alias Graph =
    Graph.Graph String ()


type alias Model =
    { input : String
    , filterRoot : String
    , view : View
    , cached : Result String Cached
    }


type View
    = Dot
    | Mermaid
    | Graph


type alias Cached =
    { callGraph : CallGraph
    , modules : List String
    , selectedModules : Set String
    }


type Msg
    = Input String
    | View View
    | SelectModule String Bool
    | Filter String


type alias Flags =
    {}


rythm : number
rythm =
    10


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            Element.layout [ padding rythm ] << view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { input = "{}"
      , filterRoot = ""
      , view = Dot
      , cached = toCached "{}"
      }
    , Cmd.none
    )


toCached : String -> Result String Cached
toCached input =
    toCallGraph input
        |> Result.map
            (\callGraph ->
                let
                    nodes : List String
                    nodes =
                        Dict.toList callGraph
                            |> List.concatMap (\( from, tos ) -> from :: tos)
                            |> Set.fromList
                            |> Set.toList

                    modules =
                        nodes
                            |> List.map getModule
                            |> Set.fromList
                in
                { callGraph = callGraph
                , modules = Set.toList modules
                , selectedModules = modules
                }
            )


getModule : String -> String
getModule name =
    name
        |> String.split "."
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> String.join "."


type alias CallGraph =
    Dict String (List String)


toCallGraph : String -> Result String CallGraph
toCallGraph input =
    case Json.Decode.decodeString callGraphDecoder input of
        Ok callGraph ->
            Ok callGraph

        Err _ ->
            Err "Error parsing the input"


callGraphToGraph : Model -> Cached -> Graph
callGraphToGraph model cached =
    let
        reachableNodes : List String
        reachableNodes =
            calculateReachableNodes model cached

        indices : Dict String Int
        indices =
            reachableNodes
                |> List.indexedMap (\i name -> ( name, i ))
                |> Dict.fromList

        edges : List ( Int, Int )
        edges =
            Dict.toList cached.callGraph
                |> List.concatMap
                    (\( from, tos ) ->
                        tos
                            |> List.filterMap
                                (\to ->
                                    Maybe.map2 Tuple.pair
                                        (Dict.get from indices)
                                        (Dict.get to indices)
                                )
                    )
    in
    Graph.fromNodeLabelsAndEdgePairs reachableNodes edges


calculateReachableNodes : Model -> Cached -> List String
calculateReachableNodes model { selectedModules, modules, callGraph } =
    let
        noModuleFilter =
            Set.size selectedModules == List.length modules

        filterByModule : String -> Bool
        filterByModule node =
            noModuleFilter || Set.member (getModule node) selectedModules
    in
    if String.isEmpty model.filterRoot then
        Dict.toList callGraph
            |> List.concatMap (\( from, tos ) -> from :: tos)
            |> Set.fromList
            |> Set.toList
            |> List.filter filterByModule

    else
        let
            filteredCallGraph : CallGraph
            filteredCallGraph =
                callGraph
                    |> Dict.filter (\key _ -> filterByModule key)
                    |> Dict.map (\_ value -> List.filter filterByModule value)

            roots : List String
            roots =
                Dict.keys filteredCallGraph
                    |> List.filter (\node -> String.contains model.filterRoot node)

            visit : List String -> Set String -> Set String
            visit queue acc =
                case queue of
                    [] ->
                        acc

                    node :: tail ->
                        if Set.member node acc then
                            visit tail acc

                        else
                            visit
                                (Maybe.withDefault [] (Dict.get node filteredCallGraph) ++ tail)
                                (Set.insert node acc)
        in
        visit roots Set.empty
            |> Set.toList


callGraphDecoder : Json.Decode.Decoder CallGraph
callGraphDecoder =
    Json.Decode.dict (Json.Decode.list Json.Decode.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Input i ->
                    { model
                        | input = i
                    }

                SelectModule name select ->
                    onCached
                        (\cached ->
                            { cached
                                | selectedModules =
                                    if select then
                                        Set.insert name cached.selectedModules

                                    else
                                        Set.remove name cached.selectedModules
                            }
                        )
                        model

                View view_ ->
                    { model | view = view_ }

                Filter f ->
                    { model | filterRoot = f }
    in
    ( if newModel.input == model.input then
        newModel

      else
        { newModel | cached = toCached newModel.input }
    , Cmd.none
    )


onCached : (Cached -> Cached) -> Model -> Model
onCached f model =
    { model
        | cached =
            Result.map f model.cached
    }


view : Model -> Element Msg
view model =
    column
        [ spacing rythm
        , width fill
        ]
        [ Input.text [ width fill ]
            { text = model.input
            , onChange = Input
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Call graph JSON"
            }
        , Input.text [ width fill ]
            { text = model.filterRoot
            , onChange = Filter
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Filter to roots:"
            }
        , case model.cached of
            Err e ->
                text <| "Error: " ++ e

            Ok cached ->
                let
                    graph : Graph
                    graph =
                        callGraphToGraph model cached
                in
                row [ width fill, spacing rythm ]
                    [ viewModulePicker model cached
                    , column
                        [ spacing rythm
                        , width fill
                        , alignTop
                        ]
                        [ viewViewPicker model.view
                        , case model.view of
                            Dot ->
                                viewDot graph

                            Mermaid ->
                                viewMermaid graph

                            Graph ->
                                viewGraph graph
                        ]
                    ]
        ]


viewViewPicker : View -> Element Msg
viewViewPicker view_ =
    Input.radioRow [ spacing rythm ]
        { options =
            [ Dot, Mermaid, Graph ]
                |> List.map (\option -> Input.option option (text <| viewToString option))
        , onChange = View
        , selected = Just view_
        , label =
            Input.labelLeft
                [ paddingEach
                    { left = 0
                    , right = rythm
                    , top = 0
                    , bottom = 0
                    }
                ]
                (text "View")
        }


viewToString : View -> String
viewToString view_ =
    case view_ of
        Dot ->
            "Dot"

        Mermaid ->
            "Mermaid"

        Graph ->
            "Graph"


viewDot : Graph -> Element msg
viewDot graph =
    graph
        |> Graph.DOT.output (\label -> Just label) (\_ -> Nothing)
        |> viewPre


viewPre : String -> Element msg
viewPre content =
    content
        |> Html.text
        |> List.singleton
        |> Html.pre []
        |> Element.html
        |> el []


viewMermaid : Graph -> Element msg
viewMermaid graph =
    let
        nodesDict : Dict Graph.NodeId String
        nodesDict =
            graph
                |> Graph.nodes
                |> List.map (\{ id, label } -> ( id, label ))
                |> Dict.fromList

        lines =
            graph
                |> Graph.edges
                |> List.filterMap
                    (\edge ->
                        Maybe.map2
                            (\fromLabel toLabel -> "  " ++ fromLabel ++ " --> " ++ toLabel)
                            (Dict.get edge.from nodesDict)
                            (Dict.get edge.to nodesDict)
                    )
    in
    viewPre <| String.join "\n" ("stateDiagram-v2" :: lines)


viewModulePicker : Model -> Cached -> Element Msg
viewModulePicker model cached =
    calculateReachableNodes model { cached | selectedModules = Set.fromList cached.modules }
        |> List.map getModule
        |> Set.fromList
        |> Set.toList
        |> toTrees
        |> List.map (viewTree cached)
        |> column [ spacing rythm, alignTop ]


type Tree a
    = Node a (List (Tree a))


toTrees : List String -> List (Tree { label : String, module_ : Maybe String })
toTrees modules =
    let
        toTreesHelper : List ( String, List String ) -> List (Tree { label : String, module_ : Maybe String })
        toTreesHelper branch =
            branch
                |> List.Extra.gatherEqualsBy (\( _, split ) -> List.take 1 split)
                |> List.filterMap
                    (\( head, tail ) ->
                        let
                            ( emptyChildren, fullChildren ) =
                                (head :: tail)
                                    |> List.map (Tuple.mapSecond <| List.drop 1)
                                    |> List.partition (Tuple.second >> List.isEmpty)
                        in
                        List.head (Tuple.second head)
                            |> Maybe.map
                                (\top ->
                                    Node
                                        { label = top
                                        , module_ =
                                            List.head emptyChildren
                                                |> Maybe.map Tuple.first
                                        }
                                        (toTreesHelper fullChildren)
                                )
                    )
    in
    toTreesHelper
        (List.map
            (\module_ ->
                ( module_, String.split "." module_ )
            )
            modules
        )


viewTree :
    Cached
    -> Tree { label : String, module_ : Maybe String }
    -> Element Msg
viewTree cached (Node { label, module_ } children) =
    let
        labelView : Element Msg
        labelView =
            case module_ of
                Just moduleName ->
                    Input.checkbox []
                        { checked = Set.member moduleName cached.selectedModules
                        , label = Input.labelRight [] <| text label
                        , icon = Input.defaultCheckbox
                        , onChange = SelectModule moduleName
                        }

                Nothing ->
                    text label
    in
    column
        [ spacing rythm
        , paddingEach
            { left = rythm
            , top = 0
            , bottom = 0
            , right = 0
            }
        , Border.widthEach
            { left = 1
            , top = 0
            , bottom = 0
            , right = 0
            }
        ]
        (labelView :: List.map (viewTree cached) children)


viewGraph : Graph -> Element Msg
viewGraph graph =
    let
        widthDict : Dict Graph.NodeId Float
        widthDict =
            Graph.fold
                (\{ node } ->
                    Dict.insert node.id (toFloat <| 10 * String.length node.label)
                )
                Dict.empty
                graph
    in
    Element.html <|
        Render.draw
            [ Dagre.Attributes.widthDict widthDict
            ]
            [ Render.nodeDrawer nodeDrawer ]
            graph


nodeDrawer : NodeDrawer String Msg
nodeDrawer =
    Render.StandardDrawers.svgDrawNode
        [ Render.StandardDrawers.Attributes.label .label
        , Render.StandardDrawers.Attributes.title .label
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
