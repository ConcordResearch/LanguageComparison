# src-hs


First Pass Timings - Lazy
```
Read Accounts Complete 0.000072s
Read Transactions Complete 0.000088s
Parse Accounts complete 0.000007s
Accounts Right Complete 0.000009s
Create Account Lookup complete 0.000005s
Parse Transaction Complete 0.000008s
Process Transactions Complete 0.000007s
Values ToArray Complete 0.000006s
Complete 44.243489s
Total 44.243691s
```




can we work with a CSV (List(List String)) with Lenses

## Lenses



Work the examples:
https://github.com/ekmett/lens#examples

Only use `set`, `view`, `over`, `under`:

```
Getter
^. = view
(^.) :: s -> Getting a s a -> a

Setter
.~ = set
(.~) :: ASetter s t a b -> b -> s -> t

Set with function - analogous to fmap
%~ = over
(%~) :: ASetter s t a b -> (a -> b) -> s -> t

Compose (Prelude)
. = compose 
allows the composition of lenses

```

Ok, let's take a `s t a b` at this:
* `s` is the the input record
* `t` is the output record
* `a` is the type of the field I want to change
* `b`is the output of it? 


```haskell

-- ^. = view
("hello", ("Yo", "mamma")) ^. _2._1
view (_2._1) ("hello", ("Yo", "mamma"))
-- "Yo"

-- .~ = set
(_2._1) .~ 5 $ ("hello", ("Yo", "mamma"))
set (_2._1) 5 ("hello", ("Yo", "mamma"))
--("hello",(5,"mamma"))

"hello"^.to length
--5
[1,2,3] ^. to length
--3

-- Using %~ `set with function`
(_2._1) %~ (<> " mamma") $ ("hello", ("Yo", "mamma"))
--("hello",(" mammaYo","mamma"))
(_2._1) %~ (\a -> a <> " mamma") $ ("hello", ("Yo", "mamma"))
-- ("hello",("Yo mamma","mamma"))

```

```haskell
data FooBar =
    Foo { _x :: Int}
  | Bar { _x :: Int}
  deriving (Show)

class HasX t where
  x :: Lens' t Int
instance HasX FooBar where
  x f (Foo x) = (\x' -> Foo x') <$> f x
  x f (Bar x) = (\x' -> Bar x') <$> f x

-- | Example Code
-- Try:

-- > (Foo 5) ^. x
-- 5
-- > x .~ 10 $ Foo 6
-- Foo {_x = 10}
```

### Questions

Why does the getter (^.) take the item to modify at the beginning?
Why does the setter (.~) take the item at the end?
If I've learned anything about FP very little is done arbitrarily. Why is this valuable?


Cargo Culting Lenses - Talk
https://www.youtube.com/watch?v=qte7U3Cexhc
Slides: https://qfpl.io/share/talks/cargo-culting-lenses/talk.html
Good talk about the power of lenses

https://www.youtube.com/watch?v=QZy4Yml3LTY


Lets.Lens
Start with OpticPolyLens
https://github.com/data61/lets-lens/blob/master/src/Lets/OpticPolyLens.hs
Lens A B: we are targetting a B inside A; A is made of B and some other stuff
Prism A B: we are targetting at most 1 B inside A; A is made of B or some other stuff
Traversal A B: we are targetting 0 or many B inside A; A is made of 0 or many B and some other stuff



("abc", 123, "def") & view _2
123
("abc", 123, "def") & _2 %~ (+10)
("abc",133,"def")

```
_Payment :: Prism Transaction (AccountNumber, Money, String) 
amountL :: Lens Money Number
_Payment . _2 :: Traversal Transaction Money 
_Payment . _2 . amountL :: Traversal Transaction Number
```

dibblego I think I get a part of it. whats `_Payment . _2 ` mean?
```
_2 is a lens that I assume already exists, get's the second component out of a tuple
_2 :: Lens (a, b, c) b
```

From: http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-TH.html#v:makeLenses

```haskell
data FooBar = 
    Foo { _x, _y :: Int } 
  | Bar { _x :: Int }
makeLenses ''FooBar 
```
will create 

```haskell
x :: Lens' FooBar Int
x f (Foo a b) = (\a' -> Foo a' b) <$> f a
x f (Bar a)   = Bar <$> f a

y :: Traversal' FooBar Int
y f (Foo a b) = (\b' -> Foo a  b') <$> f b
y _ c@(Bar _) = pure c
```


YOu can use the 'accessor' function to modify

```
x f foo = (\a -> foo { _x = a }) <$> f (_x foo)
```

ah I see how 'x f foo' gives me 'foo' which gives me access to '_x' in both cases, then 'foo {_x=a}' modifies foo's _x, thats helpful

```
Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
-- you can also use that function to derive the lens
x = lens _x (\s b -> s { _x = b })
```
