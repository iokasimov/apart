# Get all your structure and rip it apart &#128298;

The main idea: if you can describe your data structure via Cofree, you got:
* Comonadic methods, you always can `extract` a focus value or `extend` with some function
* With `apart` you can serialize, persistent or hash a segment of your structure!

# Example usage

Let's define a simple non-empty list type:
```haskell
type Stack a = Cofree Maybe a
```
Then, build a value of this type, stack of integers, the whole structure in memory (ignore lazy evaluation aspects now):
```haskell
inmemory :: Stack Int
inmemory = 1 :< Just (2 :< Just (3 :< Just (4 :< Just (5 :< Nothing))))
```

## Set a limit for structure
Sometimes, we don’t need to hold a whole structure in memory, can it be good just cut a part of it and save to file?
```haskell
save_to_file :: FilePath -> Segment Stack Int -> IO FilePath
save_to_file fp structure = writeFile fp (show structure) *> pure fp

scattered = IO (Scattered (Stack Int) FilePath)
scattered = limit 2 (save_to_file "part.txt") inmemory
```
And our structure transformed into:
```haskell
scattered :: Scattered Stack Int FilePath
scattered = Apart $ 1 :< Ready (Just $ 2 :< Ready (Just $ 3 :< Converted "part.txt"))
```

## Traverse over scattered structure
We also can fluently traverse over scattered structure with action and function for recover a segment:
```haskell
fluently :: IO (Stack ())
fluently = fluent print read_from_file scattered
```

## Recover scattered structure
Return back to memory our stack of integers:
```haskell
read_from_file :: FilePath -> IO (Segment Stack Int)
read_from_file fp = read @(Segment (Stack Int)) <$> readFile fp

inmemory :: IO (Stack Int)
inmemory = recover read_from_file scattered
```
