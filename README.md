# elm-review-call-graph

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to draw and/or explore the call graph in an application.

The call graph is a list of which functions calls which other function. It is useful to visualize the dependencies between the various parts of your codebase, at a function granularity.

## Provided rules

- [`ExtractCallGraph`](https://package.elm-lang.org/packages/miniBill/elm-review-call-graph/1.0.0/ExtractCallGraph) - Extracts the call graph.

## Configuration

```elm
module ReviewConfig exposing (config)

import ExtractCallGraph
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ ExtractCallGraph.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template miniBill/elm-review-call-graph/example --extract --report=json | jq ".extracts.ExtractCallGraph" > call-graph.json
```

You can then open the call graph with **TODO**
