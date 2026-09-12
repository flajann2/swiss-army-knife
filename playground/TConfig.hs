{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Config ( getConfig )

main :: IO ()
main = do
  cfg <- getConfig
  putStrLn $ show cfg
  pure ()
  
