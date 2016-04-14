import WhileInterp

test f = do
  putStrLn $ "***Testing " ++ f
  putStrLn "\nParsed File : "
  showParsedExp f
  putStrLn "\n Output Map : "
  runFile f
  putStrLn "\n\n"

main :: IO ()
main = do

  test "Testfiles/abs.imp"
  test "Testfiles/extra.imp"
  test "Testfiles/fact.imp"
  test "Testfiles/times.imp"
  test "Testfiles/test.imp"
  test "Testfiles/error.imp"
