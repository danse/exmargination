
type DateAccessor a = DateAccessor (a -> Date)

-- the merger gets a list and a date to use for the resulting element
type Merger a = Merger ([a] -> Date -> a)

type Splitter a = Splitter (Date -> [a] -> (Date,[a],[a]))

type Filler a = Filler (Date -> [a] -> [a])

-- return a list of empty values separated by the given interval, up to the date
fillToDate getDate createEmpty interval ref [] = []
fillToDate getDate createEmpty interval ref elements =
  | firstDate > nextRef = (createEmpty ref):(fillToDate getDate createEmpty interval elements)
  | otherwise = []
  where firstDate = getDate $ head elements
        nextRef = ref + interval

-- take elements from the beginning of a list while they fit in an interval
splitUpTo getDate merge startDate stopDate elements =
  ((foldl1 merge inside startDate),outside)
  where (inside,outside) = span (\x -> (getDate x) > startDate && (getDate x) <= stopDate) elements

-- at every call, the recursive function returns a processed list, and
-- it gets a non processed list and a reference date about the last
-- emitted element. if the next element would have a date greater than
-- the reference + the interval, a filling element is
-- created. otherwise, the function will look ahead and merge all
-- elements within the same interval, pick a representative date for
-- the merged elements and use it as the new reference
step :: Splitter -> Filler -> Date -> [a] -> [a]
step splitter filler ref [] = []
step splitter filler ref elements =
  (filler ref elements):grouped:(step splitter filler nextRef reminder)
  where (nextRef,(grouped,reminder)) = splitter ref elements

makeFiller :: (a -> Date) -> (Date -> a) -> Int -> Filler a
makeFiller a b c = fillToDate a b c

makeSplitter :: (a -> Date) -> Merger -> Int -> Splitter a
makeSplitter getDate merge interval =
 \ e r -> (r+interval, (splitUpTo merge getDate r (r+interval) e))

type SeriesConverter = SeriesConverter [a] -> [a]

createConverter merger interval accessor =
  \x -> (step splitter filler (accessor $ head x)) $ sorter x
  where splitter = makeSplitter accessor merger interval
        filler = makeFiller accessor creator interval
        sorter = sortby accessor
