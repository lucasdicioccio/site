
-------

Haskell in fewer than 10e6 ms.
=============================

- *Goal today*: entice you into learning some Haskell/OCaml/Typed-FP-au-choix
- "hash-tag": **@while42London**
- _Caution_ _Caution_ _Caution_ slides will show some code. Some syntax I use does not exist but make things "pretty".

-------

# My viewpoint on engineering

- The Maslow pyramid of needs

` food > safety > love > esteem > self-actualization `

- The lucasdicioccio pyramid of solutions

` timeliness > correctness > maintainability > prettiness > ligth-shedding `

- I can write "quick-and-dirty" code just fine now, but I know that making leaps upward is pretty cheap with Haskell.
- Correctness for most stupid yet time-consuming errors is basically free.
- The light cost of types allows to consicely and precisely model importants features.
- The syntax is a bit foreign at first but "side effects as values" and the "do-notation" allow for cute embedded-DSLs.
- In a number of instances, you realize you gained a lot of hindsight about the problem you were solving and are able to connect it to other solutions.
- Please, do challenge people who tell Haskell is not "practical". Ask them to give you solid arguments and match these arguments against your own "pyramid of needs". Maybe they have a good but out-of-date picture of things, especially from people isolated from typed-FP for years.



-------

# Types provide a logical framework to define and combine data structures
- data types are **cheap** and **expressive**
- if you are used to BNF grammar, data types definitions will look familiar

>           data Bool           = True | False
>
>           data Int16          = 0 | 1 | 2 | 3 | ... | 65535
>           data Int            = 0 | 1 | 2 | 3 | ...
>
>           data Pair a b       = (a, b)
>           data Triple a b c   = (a, b, c)
>
>           data Function a b   = Func (a -> b)
>           data Function a b c = Func (a -> b -> c)
>
>           data List a         = Empty | (a, List a)
>
>           data Maybe a        = Nothing | Just a
>
>           data Tree a         = Leaf a | Bin (Tree a) (Tree a)

- data types help you make invalid program states un-expressible

>           data Four a = (a, a, a, a)
>
>           carWheelsA :: [Wheels]     -- list of wheels for a car
>           carWheelsB :: Four Wheels  -- exactly Four wheels for a car
>
>           -- With the B definition, no need to litter code with length
>           -- checks or asserts, you cannot build a car with two many or two
>           -- few wheels. Basically turning the compiler into an automated
>           -- and meticulous test suites.
>           -- You could do the same in Java but the syntax overhead is
>           -- significant.

>           data Maybe x = Nothing | Just x
>                      --          ^ this 'or' is the bit of information
>                      -- telling whether a value exists or not
>
>           baldwin :: Maybe User  -- Nothing | Just User
>                              --                    ^ user baldwin
>                              --               ^ proof carrying user baldwin
>                              -- ^ baldwin doesn't exist
>
>           -- say we have a function returning the name for a User
>           userName :: User -> Name
>
>           -- We need to prove that (baldwin :: Maybe User) is not Nothing before
>           -- username. If we want to get a Name value out of baldwin we
>           -- provide a value for the Nothing case.
>           newPersonName = case baldwin of
>                           (Just user) = userName user
>                           Nothing     = "bruce" -- let's call him Bruce, that
>                                                 -- will avoid confusion.

- *key takeaway*: algebraic data types as a support for reasoning

-------

# Purity revovlves around a simple principle: referential transparency

- definition: you can swap the result of a computation for the computation
- in layman terms: cut-n-paste is correct

>         -- partition a list in two sublists using a boolean function
>         -- naive algorithm, unreadable version
>         partition :: (a -> Bool) -> [a] -> ([a], [a])
>         partition f xs = ([x | x <- xs, f x], [x | x <- xs, not f x])
>                        -- ^ comprehension lists 
>                        --       `for x in xs if f(x) is True` => to the left sublist
>                        --       `for x in xs if f(x) is False` => to the right sublist
>
>         -- naive algorithm, refactored by copy-pasting and names to the left
>         -- and right sublists
>         partition f xs = (leftList, rightList) where leftList  = [x | x <- xs, f x]
>                                                      rightList = [x | x <- xs, not (f x)]
>                                                   -- ^ call these "names" rather than "variables"

- Referential transparency blurs the concept of "variable": we only have names for "values we carry around" (e.g., `left`, `right`)
- Referential transparency blurs the concept of "result": we only have names for "values we want to compute" (e.g., `partition f [1,2,3]`)
- Referential transparency necessitates a trick to represent side-effects because side-effect cannot be swaped for their result (e.g., result of an HTTP get to the weatherforecast website will depends on the day you apply the side-effect).
- Haskell uses one level of indirection: side-effects are normal values. And one combines side-effects together to piece a single `main` side-effect.
- In short, `main` actually is a **single** IO () action composed from many small actions. The compiler effectively is a function from a value of type `IO ()` returning an executable binary program that your OS understands.

>        actions :: [IO ()] -- a list of side effects
>        actions = [putStrLn "hello", putStrLn "world"]
>
>        -- the only thing capable of observing the content of IO () is the runtime
>        main :: IO ()
>        main = sequence actions >> putStrLn "done"
>            -- ^ turns a list of side-effects into a single side-effect by
>            --   sequencing actions starting from the left of the list
>                             -- ^ the (>>) operator sequences exactly two side effect

- *key takeaway*: purity makes it easier to cut-n-paste code (including when self-talking).
- *key takeaway*: purity elevates side-effects to the rank of normal values, no need to jail side-effects in a function/lambda/proc/block to pass them around.

-------

# Functional problem solving is exciting and offers hindsight
- **reasoning on types**: is there a huge difference between a read-only key/value dictionary and a function?

>     someFunction :: Arg1          -> Arg2  -> Value
>           lookup :: Map Key Value -> Key   -> Maybe Value
>     -- A function "maps" a return value to some argument.
>     -- A dictionary "maps" a return value to some key, but the key might be missing.
>     -- This is why a dictionary is a nice way to "cache results".

- **partial application**: capture only the parameters you know or need, you will complete this later.

>           -- I need three things to make some crepes.
>           makeCrepes :: Milk -> Flour -> [Egg] -> IO (Crepes)
>
>           -- If I have Milk in the fridge I could partially apply the milk.
>           makeCrepes milkFromFridge :: Flour -> [Egg] -> IO (Crepes)
>
>           -- Then maybe my neighbor has some Flour
>           makeCrepes milkFromFridge flourFromNeighbor :: [Egg] -> IO (Crepes)
>
>           -- We just need to buy eggs.
>           makeCrepes milkFromFridge flourFromNeighbor eggsFromFarmer :: IO (Crepes)
>
>           -- In another scenario, we may have had Flour and Eggs at home, we
>           -- can still use the same recipe (real code would need you to add
>           -- some boilerplate to move arguments around).
>           makeCrepes flourFromHome eggsFromHome :: Milk -> IO (Crepes)
>
>           -- Note: in real life, you shouldn't put the milk first because
>           -- mixing ingredients has side-effects. The type system let you models
>           -- requirements well because you cannot observe any
>           -- crepes-making side-effect until you have all ingredients to
>           -- start. That is, you cannot start cooking and figure out later that you
>           -- lack an ingredient.  Also, the makeCrepes function does not
>           -- expose intermediary steps, which prevents your secret
>           -- trick to make the tastiest crepes.

- **parametricity** prevents observation (a.k.a univeral quantification)

>           length :: [a] -> Int -- promises to work for "any" `a` you can
>                                -- imagine (i.e., for all `a`). Thus, length
>                                -- cannot "observe" the values in the list,
>                                -- the result can only depends on the shape of
>                                -- the list, not its content.
>
>           length [Nothing]                                        = 1
>           length [User "Grace Hoper"]                             = 1
>           length [error "if-you-evaluate-me-the-program-crashes"] = 1
>
>           -- Another way to put it is that you could drop the information `a`
>           -- in the list and obtain the same result.
>           -- That is, if you promise to never observe the content of a list, the only
>           -- thing you can do with the list is count how many elements there
>           -- are, but nothing more fancy. In practice, if you do such a
>           -- promise you need not pay the cost of carrying-around the `a` items
>           -- of the list. Suddenly, your list looks like just a natural
>           -- number corresponding to the lenght of the list. Reasoning on
>           -- types shows this interesting fact pretty nicely:
>           data List a         = Empty | (      a,  List a)
>           data Natural        = Zero  | (PlusOne, Natural)
>
>           ["hello", "world"]  = ("hello", ("world", Empty))
>           [     42,      51]  = (     42, (     51, Empty))
>           -- If we focus only on the shape of the list:
>           [______ , _______]  = (_______, (_______, Empty))
>           TwoThings           = (PlusOne, (PlusOne, Zero ))

- **laziness**: expresses the mathematical statement defining a value, computation will occur as needed

>           fibs :: [Int]
>           fibs = 0 : 1 : zipWith (+) fibs (tail fibs) 
>           -- read this as: fibs is a list of Int starting with 0, followed by 1
>           -- followed by the list of sums of the fibs list and the fibs list shifted by one item
>    
>                    fibs      =  [0, 1, 1, 2, 3, 5, ..]
>                                     -- ^ depends on `zipWith...` starting from here
>                    tail fibs =  [1, 1, 2, 3, 5, 8, ..]
>                               -- ^ the 1st value depends only on fibs 2nd
>                               --   item, not the full list
>
> zipWith (+) fibs (tail fibs) =  [1, 2, 3, 5, 8, 13, ..]
>                               -- ^ the 1st value depends only on fibs and
>                               --   (tail fibs) 1st items, not the full lists

- *key takeaway*: functional thinking let you state a large problem and deliver its solution focusing on small bits

-------

# Practical life of a Haskeller
- great feeling of having done the things right
- steep but fullfilling learning curve
- fearless parallelism and concurrency thanks to purity-by-default (enforced by the compiler) rather than purity-by-scrutiny (enforced by your colleagues)
- good memory footprint, decent speed, and pretty extensive profiling support/GC-knobs
- lots of amazing libraries (for parsing, for testing, to interface with C or R, to write web microservices, connect to databases etc.) on Hackage
- long compile-time and some amount of dependency hell, being fixed
- some impedance mismatch between strong types and things where the "shape" of a same data varies widely with the input (e.g., `SELECT colnames` in SQL). One can give-away the type-safety and treat all columns as equal but feels less idiomatic.
- *key takeaway*: learn it now or re-invent a half-baked version using "best-practices" later

-------

# Good to know
- Two Meetup-groups in London
- Growing number of companies ready to pay for functional programmers, including Haskell
- Curated list of videos on GitHub >>= search-for: haskell-must-watch
- Real-World-Haskell is very practical
- Write Yourself a Scheme in 48h, the best way I found to cement what I learned
- Jon Carmack QuakeCon's talk, he rocks

-------

# Algebraic jargon is precise

- The goal of jargon is to capture a particular behavior in terms of constraints.
- **monoids**: very generic way of combining two values

> -- Always remember that one monoid is three things:
> --   i. a set of values
> --  ii. an associative, binary operator on the set of values
> -- iii. a specific element in the set which is "neutral" for the operator
>
> -- Strings give a monoid with `++` (concatenation) and `""` (empty string).
>  ("he" ++ "") ++ ((("" ++ "llo") ++ "") ++ "world") = "he" ++ "llo" ++ "world"

> -- Natural numbers give a monoid with `+` (addition) and `0` (zero).
>  (2     +  0) +  (((0  +      3) +   0) +        5) = 2     + 3      +       5

> -- Natural numbers also give a monoid with `*` (multiplication) and `1` (one).
>  (2     *  1) *  (((1  *      3) *   1) *        5) = 2     * 3      *       5

- **functor**: what does it mean to have a context?

>   -- only function for a Functor is the equivalent to map on lists:
>    map :: (a -> b) -> [ a ] -> [ b ]
>   fmap :: (a -> b) -> t a   -> t b   -- here `t` is the functor and could be `List`
>
>   -- Thus, since the only function to operate on a functor is `fmap`.
>   -- partially-applying fmap to `t a` gives. `(a -> b) -> t b`.
>   -- That is, a functor object is a _consumer_ of functions which modify
>   -- _produced_ values (nit: the functor may never _produce_ any value but
>   -- the type changes anyway).
>
>   -- For instance IO is a functor. Say we can read some Text file given a file path.
>   readFile :: FilePath -> IO Text
>   -- And we know how to counts lines of text.
>   countLines :: Text -> Int
>   -- We can ask readFile to injest countLines with fmap.
>   countFileLines :: FilePath -> IO Int 
>   countFileLines path = fmap countLines (readFile path)
> 
>   -- In this example we didn't have to run side effects. We just adapted the value
>   -- produced by the readFile result. Functor requires some theoretical guarantees
>   -- so that you can count on `fmap` to do-the-right-thing(tm).

- **applicative functors**: what does it mean to merge functors "side-by-side" when applying some function?

> --  +--------+     +---+      +---+
> --  | a -> b | <*> | a |   =  | b |
> --  +--------+     +---+      +---+
> --     Foo          Bar        FooBar
> --
> -- One theoretical guarantee for Applicative is that both contexts are
> -- independent from each other (i.e., the value Bar cannot depend on the
> -- value of `a->b`, `a`, or `b`).

- **monads**: what does it mean to merge the nesting of a same functors?

> --  +-------+   
> --  | +---+ |             +---+
> --  | | c | |  #join  =   | c |
> --  | +---+ |             +---+
> --  |  Foo  |              FooBar
> --  +-------+   
> --     Bar
> --
> -- Monads drop the guarantee that contexts are independent from each other.
> -- (i.e., the value Bar may depend on the value `c`, hence changing the
> -- course of actions as we merge levels of nesting (e.g., being Baz)).

- *key takeaway*: a pretty small set of alien abstractions allow you to express a broad range of problems.
