module Core.Term where

import System.Console.Pretty

-- Output info message in blue
info :: String -> IO ()
info msg = do
  inColor <- supportsPretty
  if inColor
     then putStrLn (color Blue msg)
     else putStrLn $ "INFO: " ++ msg

-- Output warning message in yellow
warn :: String -> IO ()
warn msg = do
  inColor <- supportsPretty 
  if inColor
     then putStrLn (color Yellow msg)
     else putStrLn $ "WARN: " ++ msg

-- Output error message in red
err :: String -> IO ()
err msg = do
  inColor <- supportsPretty
  if inColor
     then putStrLn (color Red msg)
     else putStrLn $ "ERROR: " ++ msg
