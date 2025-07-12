{-# LANGUAGE OverloadedStrings #-}

module Shaders where

import Prelude hiding (unlines)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

vertexShaderSource :: ByteString
vertexShaderSource = BS.unlines
  [ "#version 330 core"
  , "layout (location = 0) in vec3 aPos;"
  , "out vec3 vPos;"
  , "void main() {"
  , "    gl_Position = vec4(aPos, 1.0);"
  , "    vPos = aPos;"
  , "}"
  ]


fragmentShaderSource :: ByteString
fragmentShaderSource = BS.unlines
  [ "#version 330 core"
  , "in vec3 vPos;"
  , "out vec4 FragColor;"
  , "void main() {"
  , "    vec3 normalColor = normalize(vPos);"
  , "    FragColor = vec4(abs(normalColor), 1.0);"
  , "}"
  ]
