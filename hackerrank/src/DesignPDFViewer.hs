module DesignPDFViewer where

import qualified Data.Map                           as Map

-- Complete the designerPdfViewer function below.
designerPdfViewer :: [Int] -> String -> Int
designerPdfViewer h word = tallest * length word
  where
    abcMap  = Map.fromList $ zip ['a' .. 'z'] h
    tallest = foldr (\c m -> max m (Map.findWithDefault 0 c abcMap)) 0 word
