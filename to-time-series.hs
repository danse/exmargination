
merge :: Merger -> Folded a -> a -> Folded a

fill :: date -> Folded a -> a -> Folded a

increment :: date -> Folded a -> a -> Folded a

data Folded a = Folded {
  thisLimit :: date,
  nextLimit :: date,
  processed :: [a]
}

type DateAccessor a = DateAccessor (a -> Date)

type Merger a = Merger (a -> a -> a)

-- needs a sorted list
-- grab the first date
-- add the interval
-- if the next date is smaller than the result, aggregate
-- if the next date is bigger than the result, increment
-- if the next date is bigger than double the result, fill and increment
step :: Merger -> Int -> DateAccessor a -> a -> [a] -> [a]
step merger interval getDate elements
  | nextDate < thisLimit = merge merger folded element
  | nextDate > nextLimit = fill thisLimit folded element
  | otherwise = increment nextLimit folded element
  where nextDate = getDate element
        thisLimit = thisLimit folded
        nextLimit = nextLimit folded

-- at every call, the recursive function returns a processed list, and it gets a non processed list and a reference date about the last emitted element. if the next element would have a date greater than the reference + the interval, a filling element is created. otherwise, the function will look ahead and merge all elements within the same interval, pick a representative date for the merged elements and use it as the new reference

type SeriesConverter = SeriesConverter [a] -> [a]

createConverter merger interval accessor =
    processed . p . sorter
  where p = step merger interval accessor,
        sorter = sortby accessor
