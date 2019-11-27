import Criterion.Main
import Seascape.Data.Sparse

main :: IO ()
main = defaultMain
  [ bgroup "aggByTerm" [ bench "data" $ whnfIO ((show . aggByTerm) <$> loadFrame "data/data.csv")
                       , bench "data-smol" $ whnfIO ((show . aggByTerm) <$> loadFrame "data/data-smol.csv")
                       ]
  ]
