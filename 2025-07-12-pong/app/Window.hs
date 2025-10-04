{-# LANGUAGE ScopedTypeVariables #-}

module Window where

import SDL
import Data.Text
import Control.Exception (SomeException, try, throwIO)

createWindow :: WindowConfig -> Text -> IO Window
createWindow wconf wname = do
  windowResult <- try $ SDL.createWindow wname wconf
  window <- case windowResult of
    Left (err :: SomeException) -> throwIO $ userError $ "Window creation failed: " ++ show err
    Right w -> do
      putStrLn "Window created successfully"
      return w
  return window
