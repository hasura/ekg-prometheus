{-
This module is based on code from the prometheus-2.2.3 package:
https://hackage.haskell.org/package/prometheus-2.2.3

The license for prometheus-2.2.3 follows:

BSD 3-Clause License

Copyright (c) 2016-present, Bitnomial, Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Export
  ( sampleToPrometheus
  , escapeTagValue
  ) where

import Data.Bifunctor (first, second)
import qualified Data.ByteString.Builder as B
import Data.Char (isDigit)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Metrics
  ( Identifier (Identifier, idName),
    Sample,
    Value (Counter, Gauge, Histogram),
  )
import System.Metrics.Histogram
  ( HistogramSample (histBuckets, histCount, histSum)
  )

------------------------------------------------------------------------------

-- | Encode a metrics 'Sample' into the Prometheus 2 exposition format,
-- adjusting the sample as follows:
--
-- * The names of metrics and tags are adjusted to be valid Prometheus
-- names (see
-- <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>):
--
--     * characters not matched by the regex @[a-zA-Z0-9_]@ are replaced
--     with an underscore (@_@), and
--
--     * if a name is empty or if its first character is not matched by
--     the regex @[a-zA-Z_]@, it is prefixed with an underscore (@_@).
--
-- Note that, for the output of this function to be a valid Prometheus
-- metric exposition, the 'Sample' you provide must meet the following
-- conditions:
--
--
-- * Within the values of tags, the backslash (@\\@), double-quote
-- (@\"@), and line feed (@\\n@) characters must be escaped as @\\\\@,
-- @\\\"@, and @\\n@, respectively. See 'escapeTagValue'.
--
-- * If two metrics have the same name, they must also have the same
-- metric type.
--
--     * For each name, only metrics of one type (chosen arbitrarily)
--     will be retained while those of other types will be discarded.
--
--
-- For example, a metrics sample consisting of
--
-- (1) a metric named @100gauge@, with no tags, of type @Gauge@, with
-- value @100@;
-- (2) a metric named @my.counter@, with tags @{tag.name.1="tag value 1", tag.name.2="tag value 1"}@, of type
-- @Counter@, with value @10@;
-- (3) a metric named @my.counter@, with tags @{tag.name.1="tag value 2", tag.name.2="tag value 2"}@, of type
-- @Counter@, with value @11@;
-- (4) a metric named @my.histogram@, with tags @{tag="value"}@, of type
-- @Histogram@, with bucket upper bounds of @[1, 2, 3]@, and
-- observations @[1, 2, 3, 4]@;
--
-- is encoded as follows:
--
-- > # TYPE _100gauge gauge
-- > _100gauge 100.0
-- >
-- > # TYPE my_counter counter
-- > my_counter{tag_name_1="tag value 1",tag_name_2="tag value 1"} 10.0
-- > my_counter{tag_name_1="tag value 2",tag_name_2="tag value 2"} 11.0
-- >
-- > # TYPE my_histogram histogram
-- > my_histogram_bucket{tag="value",le="1.0"} 1.0
-- > my_histogram_bucket{tag="value",le="2.0"} 2.0
-- > my_histogram_bucket{tag="value",le="3.0"} 3.0
-- > my_histogram_bucket{tag="value",le="+Inf"} 4.0
-- > my_histogram_sum{tag="value"} 10.0
-- > my_histogram_count{tag="value"} 4.0
--
sampleToPrometheus :: Sample -> B.Builder
sampleToPrometheus =
  mconcat
    . intersperse newline
    . map
        ( exportGroupedMetric
        . makeGroupedMetric
        . NonEmpty.map (first sanitizeIdentifier)
        )
    -- Note: This use of 'groupBy' relies on the lexicographic ordering
    -- defined on the 'Identifier' type, which considers the metric name
    -- first.
    . NonEmpty.groupBy ((==) `on` (idName . fst))
    . M.toAscList

type Tags = HM.HashMap T.Text T.Text

------------------------------------------------------------------------------

data GroupedMetric
  = GroupedCounter
      T.Text -- Metric name
      [(Tags, Int64)]
  | GroupedGauge
      T.Text -- Metric name
      [(Tags, Int64)]
  | GroupedHistogram
      T.Text -- Metric name
      [(Tags, HistogramSample)]

-- Within a sample, metrics with the name should have the same metric
-- type, but this is not guaranteed. To handle the case where two
-- metrics have the same name but different metric types, we arbitrarily
-- choose, for each metric name, one of its metric types, and discard
-- all the metrics of that name that do not have that type.
--
makeGroupedMetric :: NonEmpty (Identifier, Value) -> GroupedMetric
makeGroupedMetric xs@((Identifier metricName _, headVal) :| _) =
  case headVal of
    Counter _ ->
      GroupedCounter metricName $
        flip mapMaybe xs_list $ \(Identifier _ tags, val) ->
          sequence (tags, getCounterValue val)
    Gauge _ ->
      GroupedGauge metricName $
        flip mapMaybe xs_list $ \(Identifier _ tags, val) ->
          sequence (tags, getGaugeValue val)
    Histogram _ ->
      GroupedHistogram metricName $
        flip mapMaybe xs_list $ \(Identifier _ tags, val) ->
          sequence (tags, getHistogramValue val)
  where
    xs_list = NonEmpty.toList xs

getCounterValue :: Value -> Maybe Int64
getCounterValue = \case
  Counter i -> Just i
  _ -> Nothing

getGaugeValue :: Value -> Maybe Int64
getGaugeValue = \case
  Gauge i -> Just i
  _ -> Nothing

getHistogramValue :: Value -> Maybe HistogramSample
getHistogramValue = \case
  Histogram hs -> Just hs
  _ -> Nothing

------------------------------------------------------------------------------

exportGroupedMetric :: GroupedMetric -> B.Builder
exportGroupedMetric = \case
  GroupedCounter metricName tagsAndValues ->
    exportCounter metricName $ map (second fromIntegral) tagsAndValues
  GroupedGauge metricName tagsAndValues ->
    exportGauge metricName $ map (second fromIntegral) tagsAndValues
  GroupedHistogram metricName tagsAndValues ->
    exportHistogram metricName tagsAndValues

-- Prometheus counter samples
exportCounter :: T.Text -> [(Tags, Double)] -> B.Builder
exportCounter metricName tagsAndValues =
  mappend (metricTypeLine "counter" metricName) $
    foldMap
      (\(tags, value) ->
        metricSampleLine metricName tags (double value))
      tagsAndValues

-- Prometheus gauge samples
exportGauge :: T.Text -> [(Tags, Double)] -> B.Builder
exportGauge metricName tagsAndValues =
  mappend (metricTypeLine "gauge" metricName) $
    foldMap
      (\(tags, value) ->
        metricSampleLine metricName tags (double value))
      tagsAndValues

-- Prometheus histogram samples
exportHistogram :: T.Text -> [(Tags, HistogramSample)] -> B.Builder
exportHistogram metricName tagsAndValues =
  mappend (metricTypeLine "histogram" metricName) $
    flip foldMap tagsAndValues $ \(tags, histSample) ->
      mconcat
        [ flip foldMap (M.toList (histBuckets histSample)) $
            \(upperBound, count) ->
              metricSampleLine
                metricName_bucket
                (HM.insert "le" (T.pack (show upperBound)) tags)
                (double count)
        , metricSampleLine
            metricName_bucket
            (HM.insert "le" "+Inf" tags)
            (double (fromIntegral (histCount histSample)))
        , metricSampleLine
            (metricName <> "_sum")
            tags
            (double (histSum histSample))
        , metricSampleLine
            (metricName <> "_count")
            tags
            (double (fromIntegral (histCount histSample)))
        ]
  where
    metricName_bucket = metricName <> "_bucket"

------------------------------------------------------------------------------

-- Prometheus metric type line
metricTypeLine :: B.Builder -> T.Text -> B.Builder
metricTypeLine metricType metricName =
  "# TYPE "
    <> text metricName
    <> B.charUtf8 ' '
    <> metricType
    <> newline

-- Prometheus metric sample line
metricSampleLine ::
  T.Text -> HM.HashMap T.Text T.Text -> B.Builder -> B.Builder
metricSampleLine metricName labels value =
  text metricName
    <> labelSet labels
    <> B.charUtf8 ' '
    <> value
    <> newline

-- Prometheus label set
labelSet :: HM.HashMap T.Text T.Text -> B.Builder
labelSet labels
  | HM.null labels = mempty
  | otherwise =
      let tagList =
            mconcat $
              intersperse (B.charUtf8 ',') $
                map labelPair $
                  HM.toList labels
       in B.charUtf8 '{'
            <> tagList
            <> B.charUtf8 '}'

-- Prometheus name-value label pair
labelPair :: (T.Text, T.Text) -> B.Builder
labelPair (labelName, labelValue) =
  text labelName
    <> B.charUtf8 '='
    <> B.charUtf8 '"'
    <> text labelValue
    <> B.charUtf8 '"'

------------------------------------------------------------------------------
-- Input sanitization

-- | Adjust the metric and tag names contained in an EKG metric
-- identifier so that they are valid Prometheus names.
sanitizeIdentifier :: Identifier -> Identifier
sanitizeIdentifier (Identifier metricName tags) =
  let name' = sanitizeName metricName
      tags' = HM.mapKeys sanitizeName tags
  in  Identifier name' tags'

-- | Adjust a string so that it is a valid Prometheus name:
--
-- * Characters not matched by the regex @[a-zA-Z0-9_]@ are replaced
-- with an underscore (@_@); and
--
-- * If a name is empty or if its first character is not matched by the
-- regex @[a-zA-Z_]@, it is prefixed with an underscore (@_@).
--
-- This function is idempotent.
--
-- See
-- <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>
-- for more details.
--
sanitizeName :: T.Text -> T.Text
sanitizeName rawName =
  case T.uncons rawName of
    Nothing -> "_"
    Just (headChar, _tail) -> prefix <> sanitizedName
      where
        prefix = if isInitialNameChar headChar then "" else "_"
        sanitizedName = T.map (\c -> if isNameChar c then c else '_') rawName

isNameChar :: Char -> Bool
isNameChar c = isInitialNameChar c || isDigit c

isInitialNameChar :: Char -> Bool
isInitialNameChar c =
  'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'

-- | A convenience function for escaping the backslash (@\\@),
-- double-quote (@\"@), and line feed (@\\n@) characters of a string as
-- @\\\\@, @\\\"@, and @\\n@, respectively, so that it is a valid
-- Prometheus label value. This function is not idempotent.
--
-- See
-- <https://prometheus.io/docs/instrumenting/exposition_formats/#comments-help-text-and-type-information>.
--
-- > >>> putStrLn $ escapeTagValue "\n \" \\"
-- > \n \" \\

-- Implementation note: We do not apply this function on behalf of the
-- user because it is not idempotent.
escapeTagValue :: T.Text -> T.Text
escapeTagValue = T.concatMap escapeTagValueChar

escapeTagValueChar :: Char -> T.Text
escapeTagValueChar c
  | c == '\n' = "\\n"
  | c == '"' = "\\\""
  | c == '\\' = "\\\\"
  | otherwise = T.singleton c

------------------------------------------------------------------------------
-- Builder helpers

text :: T.Text -> B.Builder
text = B.byteString . T.encodeUtf8

double :: Double -> B.Builder
double = B.stringUtf8 . show

newline :: B.Builder
newline = B.charUtf8 '\n'
