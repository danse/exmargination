```
$ margin-ratio -n <income margins> -d <hours margins>
week: 34.5
month: 23.53
quarter: 12.45
year: 32.34
$ margin-goal-difference --weekly=35 <income margins>
week: 23.3
month: 32.3
quarter: 12.32
year: 23.1
```

ratio m1 m2 = (sum m1 / sum m2) / length m1

-- ((sum m1 / length m1) - g) * length m1
goalDifference m1 g = g * length m1 - sum m1

upTo date fun margins = fun filtered
 where (filtered, _) = span (<date) margins
