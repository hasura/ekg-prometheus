# Tutorial

This document introduces Hasura's fork of the `ekg-core` metrics
library, and illustrates how to use the library to instrument your
programs with metrics. If you are new to the library, read this document
first. If you have used the original `ekg-core` library, you should
still read this document first. For a more complete API reference, see
the Haddocks of the `System.Metrics` module.

This document is a literate Haskell program:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (assert)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.Stats (RTSStats (..), getRTSStats)
import GHC.TypeLits (Symbol)

-- This pacakge's modules
import System.Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
```

Although you will need to use some type-level features of Haskell when
using the `ekg-core` API, you will not need a solid understanding of
type-level programming. You can use `ekg-core` proficiently just by
copying the examples presented in this tutorial.

For those who have used the original `ekg-core` library, Hasura's fork
adds the following features:

* dimensional/tagged metrics (in the style of Prometheus), and
* dynamic metrics (the ability to deregister and reregister metrics).

## Overview

Metrics are used to monitor program behavior and performance. All
metrics have:

- a name,
- a set of tags (possibly empty), and
- a way to get the metric's current value.

`ekg-core` provides a way to register metrics in a global "metric
store". The store can then be used to get a snapshot of all metrics. The
store also serves as a central place to keep track of all the program's
metrics, both user and library defined.

This tutorial will show you how to:

- specify metrics,
- register and sample metrics,
- add tags to metrics,
- deregister metrics,
- use pre-defined metrics, and
- sample a subset of metrics atomically.

## Specifying metrics

Before you can register metrics to a metric store, you must first
specify _which_ metrics may be registered to that store. `ekg-core` will
statically ensure that your specifications are respected.

Your **metrics specification** must be given as a generalized algebraic
data type (GADT) with a specific kind signature. Here is an example GADT
that specifies two metrics:

```haskell
data AppMetrics1
  :: Symbol -- ^ Metric name
  -> MetricType -- ^ e.g. Counter, Gauge
  -> Type -- ^ Tag set structure
  -> Type
  where
  Requests :: AppMetrics1 "my_app.requests" 'CounterType ()
  Connections :: AppMetrics1 "my_app.connections" 'GaugeType ()
```

The `AppMetrics1` GADT has two constructors, `Requests` and
`Connections`, each of which correspond to a metric. The type parameters
of each constructor determine the name, type, and "tag structure" of
their corresponding metric. For example, the `Requests` constructor
specifies a metric with:

* name "my_app.requests", and
* type counter, and
* tags disabled.

Tutorial note: We have glossed over tags for now, but will introduce
them properly later.

## Registering and sampling metrics

Now that you have created a metrics specification, you can use it to
annotate a metrics store and start registering and collecting metrics.

Here is an example program that uses the above specification:

```haskell
app1 :: IO ()
app1 = do
  -- Create a mutable reference to a metric store.
  store <- newStore @AppMetrics1 -- (1)

  -- Initialize mutable references to metrics.
  requestsCounter <- Counter.new
  connectionsGauge <- Gauge.new

  -- Register the metrics to the metric store.
  _ <- register store $ -- (2)
    registerCounter Requests () (Counter.read requestsCounter) <>
    registerGauge Connections () (Gauge.read connectionsGauge)

  -- Update the values of the metrics.
  Counter.inc requestsCounter
  Gauge.set connectionsGauge 99

  -- Get the current values of all the metrics in the store.
  sample <- sampleAll store -- (3)

  -- Verify the sample, just for this tutorial.
  let expectedSample = M.fromList
        [ (Identifier "my_app.requests" M.empty, Counter 1)
        , (Identifier "my_app.connections" M.empty, Gauge 99)
        ]
  assert (sample == expectedSample) $ pure ()
```

1. Metric store references are parameterized by a metrics specification.
   In this case, we have used `-XTypeApplications` to explicitly name
   the intended metrics specification, even though GHC could infer the
   metrics specification itself.

1. The `register` IO action atomically applies a sequence of
   "registrations" to a metric store. Individual registrations are
   created by functions like `registerCounter` and `registerGauge`, and
   can be combined into a sequence of registrations by their `Semigroup`
   operation `<>`.

   The `registerCounter` function takes as its first argument a
   constructor of a metrics specification GADT. This constructor must
   have metric type `'CounterType`. Its second parameter specifies the
   set of "tags" to attach to the metric -- for now, tags have been
   disallowed. Its third parameter specifies the IO action that the
   store should use to sample the current value of the metric.

   The `registerGauge` function is the analogue of `registerCounter` for
   the gauge metric type.

1. The `sampleAll` function iterates through all of the metrics
   registered to the store, runs their sampling actions, and collects
   the results. Note that sampling is _not_ atomic: While each metric
   will be retrieved atomically, the sample is not an atomic snapshot of
   the system as a whole.

## Adding tags to metrics

`ekg-core` has a multi-dimensional data model, like
[Prometheus](https://prometheus.io). In this data model, metrics may be
annotated by sets of key-value pairs called **tags** or **tag sets**.
Tags are useful for convenient filtering and aggregation of metric data.
In `ekg-core`, metrics are identified by both their name _and_ their tag
set, so metrics with the same name but different tag sets are distinct
and independent metrics. When working with tagged metrics, the
constructors of a metrics specification GADT corrrespond to **classes**
of metrics that share the same name.

`ekg-core` also has support for _structuring_ the representation of your
tags. A tag set can be represented by a value of any type, as long as
the type is associated with a function that "renders" the value into a
tag set. More specifically, a tag set can be represented by a value of
any type that is an instance of the `ToTags` typeclass, which has a
single function `toTags :: ToTags a => a -> HashMap Text Text`.

Here is an example metrics specification that defines some tagged metrics:

```haskell
data AppMetrics2
  :: Symbol
  -> MetricType
  -> Type -- ^ Tag set structure
  -> Type
  where
  -- (1)
  HTTPRequests :: AppMetrics2 "requests" 'CounterType EndpointTags
  DBConnections :: AppMetrics2 "total_connections" 'GaugeType DataSourceTags

-- (2)
newtype EndpointTags = EndpointTags { endpoint :: T.Text }

instance ToTags EndpointTags where
  toTags (EndpointTags endpoint') = M.singleton "endpoint" endpoint'

-- 3
data DataSourceTags = DataSourceTags
  { source_name :: T.Text
  , conn_info :: T.Text
  } deriving (Generic)
instance ToTags DataSourceTags
```

1. The third type parameter of the constructors is used to specify
   tag set structure.

   In this example, the types provided for the tag set structure
   parameter are two user-defined types, `EndpointTags` and
   `DataSourceTags`.

1. Here, the `ToTags` instance of `EndpointTags` has been specified by
   hand.

1. Here, the `ToTags` instance of `DataSourceTags` has been specified
   using GHC.Generics.

    A `ToTags` instance may be derived via GHC.Generics for any record
    that exclusively has fields of type `Text`. The record field names
    are used as the tag keys.

Here is an example program using this metrics specification:

```haskell
app2 :: IO ()
app2 = do
  store <- newStore @AppMetrics2

  harpsichordRequests <- Counter.new
  tablaRequests <- Counter.new
  dbConnections <- Gauge.new

  _ <- register store $ mconcat
    [ registerCounter HTTPRequests (EndpointTags "dev/harpsichord") (Counter.read harpsichordRequests)
    , registerCounter HTTPRequests (EndpointTags "dev/tabla") (Counter.read tablaRequests)
    , let tags = DataSourceTags
            { source_name = "myDB"
            , conn_info = "localhost:5432" }
      in  registerGauge DBConnections tags (Gauge.read dbConnections)
    ]

  Counter.inc tablaRequests
  Gauge.set dbConnections 99

  sample <- sampleAll store

  let expectedSample = M.fromList
        [ ( Identifier
              { idName = "requests"
              , idTags = M.singleton "endpoint" "dev/harpsichord" }
          , Counter 0
          )
        , ( Identifier
              { idName = "requests"
              , idTags = M.singleton "endpoint" "dev/tabla" }
          , Counter 1
          )
        , ( Identifier
              { idName = "total_connections"
              , idTags = M.fromList
                  [ ("source_name", "myDB")
                  , ("conn_info", "localhost:5432") ] }
          , Gauge 99
          )
        ]
  assert (sample == expectedSample) $ pure ()
```

## Reregistering and deregistering metrics

Metrics you register to a metric store need not be permanent; metrics
can be replaced (reregistered) or removed (deregistered).

Reregistering metrics in `ekg-core` is implicit. If you try to register
a metric at a (name, tag set) pair that is already in use by an existing
metric, the existing metric will be deregistered and replaced with the
new metric.

Deregistering metrics in `ekg-core` is explicit, and is done using
**deregistration handles**. When you register a set of metrics with
`register`, `register` will return an IO action (the deregistration
handle) that can be used to _specifically_ deregister the newly
registered metrics. This action is specific in the following sense: if a
deregistration handle targets a metric, and that metric is replaced by a
new metric, the new metric will not be deregistered if the handle is
used.

Here is an example program that illustrates the reregistration and
deregistrtation of metrics:

```haskell
app3 :: IO ()
app3 = do
  store <- newStore @AppMetrics1 -- reusing a previous specification

  requestsCounter <- Counter.new
  connectionsGauge <- Gauge.new

  -- Register the metrics, retaining the deregistration handle. -- (1)
  deregistrationHandle <- register store $
    registerCounter Requests () (Counter.read requestsCounter) <>
    registerGauge Connections () (Gauge.read connectionsGauge)

  Counter.inc requestsCounter
  Gauge.set connectionsGauge 99

  sample1 <- sampleAll store
  let expectedSample1 = M.fromList
        [ (Identifier "my_app.requests" M.empty, Counter 1)
        , (Identifier "my_app.connections" M.empty, Gauge 99)
        ]
  assert (sample1 == expectedSample1) $ pure ()

  -- Replace (reregister) the connections gauge metric with a new one.
  replacementConnectionsGauge <- Gauge.new
  Gauge.set replacementConnectionsGauge 5
  _ <- register store $
    registerGauge Connections () (Gauge.read replacementConnectionsGauge)

  sample2 <- sampleAll store
  let expectedSample2 = M.fromList
        [ (Identifier "my_app.requests" M.empty, Counter 1)
        , (Identifier "my_app.connections" M.empty, Gauge 5)
        ]
  assert (sample2 == expectedSample2) $ pure ()

  -- Use the dereistration handle to deregister the original metrics.
  deregistrationHandle -- (2)

  sample3 <- sampleAll store
  let expectedSample3 = M.fromList
        [ (Identifier "my_app.connections" M.empty, Gauge 5)
        ]
  assert (sample3 == expectedSample3) $ pure ()
```

1. Deregistration handles were present in in all previous examples,
   but we ignored them for simplicity.

1. The deregistration handle removes all metrics registered by the
   initial call to `register`. In particular, this does not include the
   reregistered gauge.

## Using pre-defined sets of metrics

Other libraries can define sets of metrics that you can register to your
metric store. For example, the `ekg-core` library defines metrics for
the runtime system metrics exposed by `GHC.Stats` -- see
`registerGcMetrics`. Libraries that define metrics must also define
their own metrics specifications, which you will need to include in your
own metrics specification in order to use their metrics.

Here is an example program which includes the `GcMetrics` metrics
specification (used by `registerGcMetrics`) as a part of another metrics
specification:

```haskell
data AppMetrics4 :: Symbol -> MetricType -> Type -> Type where
  -- (1)
  GcSubset ::
    GcMetrics name metricType tags -> AppMetrics4 name metricType tags

app4 :: IO ()
app4 = do
  store <- newStore @AppMetrics4
  -- (2)
  _ <- register (subset GcSubset store) registerGcMetrics
  pure ()
```

1. We define a constructor, `GcSubset`, that takes any metric class from
   `GcMetrics` and makes it a metric class of `AppMetrics4`.

    Metric classes with the same type parameters (name, metric type, and
    tag structure) are treated in the same way by all functions of
    `ekg-core`, so it is enough for our constructor to "forward" the
    type parameters.

1. In order use `registerGcMetrics` with our metric store, we must use
   the `subset` function to create a new reference to our metric store
   annotated with the `GcMetrics` metrics specification that
   `registerGcMetrics` expects.

## Sampling subsets of metrics atomically

Sampling metrics is _not_ atomic; however, subsets of metrics _can_ be
sampled atomically. This can be useful if

- you need a consistent view of several metric or
- sampling the metrics together is more efficient.

For example, sampling GC statistics needs to be done atomically or a GC
might strike in the middle of sampling, rendering the values incoherent.
Sampling GC statistics is also more efficient if done in "bulk", as the
run-time system provides a function to sample all GC statistics at once.

A set of metrics can be sampled atomically if

- their values are all derived from the same, shared value via pure
  functions, and
- the IO action that computes the shared value does so atomically (e.g.
  if the shared value is a record, the action needs to compute its
  fields atomically).

To register an atomically-sampled set of metrics, use the
`registerGroup` function and the `SamplingGroup` type. Here is an
example program that does this:

```haskell
-- (1)
data GcMetrics' :: Symbol -> MetricType -> Type -> Type where
  Gcs' :: GcMetrics' "rts.gcs" 'CounterType ()
  MaxLiveBytes' :: GcMetrics' "rts.max_live_bytes" 'GaugeType ()

app5 :: IO ()
app5 = do
  store <- newStore @GcMetrics'

  -- (2)
  let samplingGroup =
        SamplingGroup
          :> (Gcs', (), fromIntegral . gcs)
          :> (MaxLiveBytes', (), fromIntegral . max_live_bytes)

  _ <- register store $
        registerGroup samplingGroup getRTSStats -- (3)
  pure ()
```

1. We replicate part of the `GcMetrics` metrics specification from
   `ekg-core`.

1. We create a sampling group with two of the runtime system metrics.

    Each metric is represented by:
    - a metric class,
    - a tag set, and
    - a pure function that computes the metric's value from a single
      value that is shared with all metrics of the sampling group.

1. We use the `registerGroup` function to pair our sampling group with
   an IO action, `getRTSStats`, that produces the shared value.

## Conclusion

This tutorial introduced and demonstrated the core features of the
`ekg-core` library:

- specifying metrics,
- registering and sampling metrics,
- tagging metrics,
- deregistering metrics,
- using pre-defined metrics, and
- sampling a subset of metrics atomically.

Additional features and details can be found in the Haddocks for the
`System.Metrics` module.

## Tutorial verification

This tutorial is compiled and run as a test using the `markdown-unlit`
package.

```haskell
main :: IO ()
main = do
  app1
  app2
  app3
  app4
  app5
```