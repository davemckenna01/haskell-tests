-- Taken from chapter 7 in http://haskell.cs.yale.edu/wp-content/uploads/2011/01/haskell-report-1.2.pdf

type Dialogue    = [Response] -> [Request]
type SuccCont    = Dialogue
type StrCont     = String     -> Dialogue
type FailCont    = IOError    -> Dialogue


data Response = Success
              | Str String
              | Failure IOError

data Request = ReadFile String
             | ReadChan String
             | AppendChan String String

-- note: AppendChan and ReadFile etc are functions (magic IO functions that
-- halt and execute "in-place", my terminology might be wrong)
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
main :: [Response] -> [Request]
main = appendChan stdout "please type a filename\n" exit (
          readChan stdin exit (\ userInput ->
               let (name : _) = lines userInput in
               appendChan stdout name exit (
                    readFile name (\ ioerror -> appendChan stdout
                                     "can't open file" exit done)
                                   (\ contents ->
                                      appendChan stdout contents exit done))))


-- a simpler version for learning, all it does is echo the input
main :: Dialogue
main :: [Response] -> [Request]
main = appendChan stdout "please type a filename\n" exit
            (readChan stdin exit
                (\ userInput ->
                let (name : _) = lines userInput in
                appendChan stdout name exit
                    done
                )
            )

-- expand appendChan
= (AppendChan stdout "please type a filename\n") : succDispatch exit
        (readChan stdin exit
            (\ userInput ->
            let (name : _) = lines userInput in
            appendChan stdout name exit
                done
            )
        )

-- expand readChan
= (AppendChan stdout "please type a filename\n") : succDispatch exit
        ((ReadChan stdin) : strDispatch exit
            (\ userInput ->
            let (name : _) = lines userInput in
            appendChan stdout name exit
                done
            )
        )

-- simplify ReadChan success continuation/callback
= (AppendChan stdout "please type a filename\n") : succDispatch exit
        ((ReadChan stdin) : strDispatch exit
            (\ userInput ->
            appendChan stdout userInput exit
                done
            )
        )

-- expand second appendChan
= (AppendChan stdout "please type a filename\n") : succDispatch exit
        ((ReadChan stdin) : strDispatch exit
            (\ userInput ->
            (AppendChan stdout userInput) : succDispatch exit done
            )
        )

{-
My guess is what happens is this: now that the main function is fully expanded
and appears to evaluate to a list (at that, a list of requests. Notice how
AppendChan, which is an element of the algebraic data type Request, is being
consed to a bunch of stuff to its right)
it can be applied to a magical response list that keeps getting filled up
like a bottomless wine glass* with the response to the head element of the
request list.

*This is the "clever footwork" mentioned on P. 4 of Tackling Awkward Squad:
    "There has to be some clever footwork to deal with the
    fact that the function has to be applied to a list of
    responses before there are any responses in the list,
    but that isn’t a problem in a lazy setting."
-}

-- here's how I think it goes down:

-- apply main to empty magic list, which kicks off running first AppendChan
= (AppendChan stdout "please type a filename\n") : succDispatch exit
        ((ReadChan stdin) : strDispatch exit
            (\ userInput -> (AppendChan stdout userInput) : succDispatch exit done )
        )
        [] -- empty magic wine glass kick-off

-- AppendChan does its thing, now there's a response in the response list
= succDispatch exit
        ((ReadChan stdin) : strDispatch exit
            (\ userInput -> (AppendChan stdout userInput) : succDispatch exit done )
        )
        [Success]

-- expand succDispatch and you've got a similar situation where there's an
-- "empty" list that's about to get a response from ReadChan
= (ReadChan stdin) : strDispatch exit
        (\ userInput -> (AppendChan stdout userInput) : succDispatch exit done )
        []

-- ReadChan does its thing, now there's a response in the response list
= strDispatch exit
        (\ userInput -> (AppendChan stdout userInput) : succDispatch exit done )
        [Str "coolfile.hs"]

-- expand strDispatch and you've got a similar situation where there's an
-- "empty" list that's about to get a response from AppendChan
= ((\ userInput -> (AppendChan stdout userInput) : succDispatch exit done ) "coolfile.hs")
  []
= (AppendChan stdout "coolfile.hs") : succDispatch exit done []

-- AppendChan does its thing, now there's a response in the response list
= succDispatch exit done [Success]
= done []
= []





