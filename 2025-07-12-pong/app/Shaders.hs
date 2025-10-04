{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shaders where

import SDL
import Prelude hiding (unlines)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Graphics.Rendering.OpenGL as GL
import Control.Exception (SomeException, try, throwIO)


vertexShaderSource :: ByteString
vertexShaderSource = BS.unlines
  [ "#version 330 core"
  , "layout (location = 0) in vec3 aPos;"
  , "out vec3 vPos;"
  , "uniform mat4 uModel;"
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
  , "    FragColor = vec4(1.0, 1.0, 0.0, 1.0);"
  , "}"
  ]

createShaderProgram :: IO GL.Program
createShaderProgram = do
  programResult <- try $ createShaderProgramFromSource vertexShaderSource fragmentShaderSource
  program <- case programResult of
    Left (err :: SomeException) -> throwIO $ userError $ "Shader program creation failed: " ++ show err
    Right prog -> return prog
  return program

createShaderProgramFromSource :: ByteString -> ByteString -> IO GL.Program
createShaderProgramFromSource vsrc fsrc = do
  vertexShader <- compileShaderFromSource GL.VertexShader vsrc
  fragmentShader <- compileShaderFromSource GL.FragmentShader fsrc
  program <- GL.createProgram
  GL.attachShader program vertexShader
  GL.attachShader program fragmentShader
  GL.linkProgram program
  return program

compileShaderFromSource :: GL.ShaderType -> ByteString -> IO GL.Shader
compileShaderFromSource shaderType source = do
  shader <- GL.createShader shaderType
  GL.shaderSourceBS shader $= source
  _ <- GL.compileShader shader
  ok <- GL.get (GL.compileStatus shader)
  unless ok $ do
    log' <- GL.get (GL.shaderInfoLog shader)
    putStrLn $ "Shader compile log: " ++ log'
  return shader
