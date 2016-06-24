{-
shared memory is what causes problems in distributing concurrent processing, because you have to coordinate the sharing: locks, one writer many readers, etc.


Shared memory works well as long as you can control the performance of it. With big distributed systems, if you take a lock on something that's shared across the network, you don't know how long you're going to be locking it and locking up the whole system.

Beckman's enumeration of sharing schemes:

The logical techniques of controlling shared memory through the "zoo" of various things like:
interlock compare exchange, mutexes, semaphores, mailboxes, name pipes... are well understood, and have been around for as long as functional ideas, but if you don't have to use it, why would you use it? It adds complexity, and in distributed setting it adds uncontrollable performance leans.

He's not saying functional is better than shared memory, he's saying there's an alternative, and that is programming using pure mathematical functions.

It's possible to program absolutely anything using pure mathematical functions.

The implementation (programming lang, OS) has to worry about locks and semaphores, but the program author doesn't.

----------

Stateful computation: labelling a tree. Put a value (0, 1, 2, etc) on a leaf node.

SOMEBODY has to remember what the current value of the label is so that when you get to another place in the tree you can update that value. We'll call that value the state.

You can do this statelessly by passing the state around in a fn argument. Every function grows another argument that is the current value of this counter. You bump it and pass it down to the next guy. The state is being "threaded" around by function arguments.

The monad is just a way of abstracting that pattern and making it compositional.

((((The monadic bind is just function composition in disguise. ))))

The whole point of passing state around in function arguments is to avoid an "implicit" state.
(What does he mean by implicit state?)

23:30 (starts talking about splitting the l and r recurse of label on different cores... I DON'T GET THIS!)

------> An instance of the monad is a function from a state to a state and contents pair.

Sounds weird to do it this way -> where are the contents coming from??

You could say fn from contents to contents state pair, but it's not the most convenient for composition.

----

GENERAL RULE (applies to all programming):
There are only 2 kinds of variables, bound variables and free variables.
A bound variable is a function argument. A free variable has to be looked up somewhere else (it's in the ambient monad, or it's in a closure).

Brian Beckman wants programmers to become more like mathemeticians.

Mathematics is the art of abstraction, and the art of precision.

Functional programming forces you to be precise.

I feel like he's using "ambient monad" to mean the "context" that a function executes in. I.e. in a JS program with just one function that mutates a global variable, the global context is the "ambient monad."

Side-effects are "updates in the ambient monad."

There's nothing wrong with the use of side-effects, but it's a question of how precise you're going to be in your use of side-effects. It forbids implicit side effects, but allows explicit side effects (through the state monad).

All haskell does is discipline the side effects. So you have to have them in an explicitly written monad. It requires the programmer to think more precisely about programs.

Beckman said he doesn't recommend writing big programs/project in Haskell (yet). And I get the impression these functional principles are not necessarily meant to be used by joe programmer, but instead used under the hood in runtimes or whatever. Maybe?
-}

-----------------------
-- Beckman's below:
-----------------------

--Non-monadic tree labeling:

data Tr a = Lf a | Br (Tr a) (Tr a)
  deriving Show

tr1 = Br (Lf 'a') (Br (Br (Lf 'b') (Lf 'a')) (Lf 'd'))

type Lt a = (Tr (S, a))
type S = Int

label :: Tr a -> Lt a
label tr = snd (lab tr 0)
  where
    lab :: Tr a -> S -> (S, Lt a)
    lab (Lf contents) n = ((n+1), (Lf (n, contents))) 
    lab (Br l r) n0     = let (n1, l') = lab l n0
                              (n2, r') = lab r n1
                          in  (n2, Br l' r')

--Monadic tree labeling:

newtype Labeled anytype = Labeled (S -> (S, anytype))

instance Monad Labeled where

  return contents = Labeled (\st -> (st, contents))

  Labeled fst0 >>= fany1 = 
    Labeled $ \st0 -> 
      let (st1, any1) = fst0 st0 
          Labeled fst1 = fany1 any1
      in fst1 st1 

mlabel :: Tr anytype -> Lt anytype
mlabel tr = let Labeled mt = mkm tr
            in snd (mt 0) 

mkm :: Tr anytype -> Labeled (Lt anytype)

mkm (Lf x)
  = updateState >>= \n -> return $ Lf (n,x)

Alternative: do n <- updateState
                return $ Lf (n,x)

mkm (Br l r)
  = mkm l >>= \l' ->
    mkm r >>= \r' ->
    return $ (Br l' r')

Alternative: do l' <- mkm l
                r' <- mkm r
                return $ (Br l' r')          

updateState :: Labeled S
updateState =  Labeled (\n -> ((n+1),n))

main = print $ mlabel tr1

