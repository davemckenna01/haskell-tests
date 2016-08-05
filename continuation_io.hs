type Dialogue    = [Response] -> [Request]
type SuccCont    = Dialogue
type StrCont     = String     -> Dialogue
type FailCont    = IOError    -> Dialogue


data Response = Success
              | Str String
              | Failure IOError

data Request = ReadFile String
             | ReadChan String
             | AppendChan String Stringmain :: Dialogue

appendChan :: String -> String -> FailCont -> SuccCont -> Dialogue
appendChan name contents fail succ resps = 
     (AppendChan name contents) : succDispatch fail succ resps

readChan :: String -> FailCont -> StrCont -> Dialogue
readChan name fail succ resps =
     (ReadChan name) : strDispatch fail succ resps

readFile :: String -> FailCont -> StrCont -> Dialogue
readFile name fail succ resps =
     (ReadFile name) : strDispatch fail succ resps

done :: Dialogue
done resps = []



strDispatch fail succ (resp:resps) =
     case resp of Str val     -> succ val resps
                  Failure msg -> fail msg resps



succDispatch fail succ (resp:resps) =
     case resp of Success     -> succ resps
                  Failure msg -> fail msg resps


main :: Dialogue
main = appendChan stdout "please type a filename\n" exit (
          readChan stdin exit (\ userInput ->
               let (name : _) = lines userInput in
               appendChan stdout name exit (
                    readFile name (\ ioerror -> appendChan stdout
                                     "can't open file" exit done)
                                   (\ contents ->
                                      appendChan stdout contents exit done))))


-- a simpler version:
main :: Dialogue
main = appendChan stdout "please type a filename\n" exit (
          readChan stdin exit (\ userInput ->
               let (name : _) = lines userInput in
               appendChan stdout name exit done))
