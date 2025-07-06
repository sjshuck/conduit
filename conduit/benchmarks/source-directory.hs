import Conduit
import System.Directory (getHomeDirectory)
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ env getHomeDirectory $ \home ->
    let benchSourceDir followSymlinks = bench ("..." ++ show followSymlinks) $
            nfIO $ runConduitRes $
                sourceDirectoryDeep followSymlinks home
                .| sinkNull
    in bgroup "sourceDirectoryDeep..."
      [ benchSourceDir False
      , benchSourceDir True
      ]
  ]
