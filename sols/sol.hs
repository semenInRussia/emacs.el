formatDuration :: Int -> String
formatDuration 0 = "now"
formatDuration s =
  let (m, s') = s `divMod` 60
      (h, m') = m `divMod` 60
      (d, h') = h `divMod` 24
      (y, d') = d `divMod` 365
      showPart n label =
        if n == 0
        then "" else
            show n ++ " " ++ label ++ if n == 1 then "" else "s"
      parts = [ showPart y "year"
              , showPart d' "day"
              , showPart h' "hour"
              , showPart m' "minute"
              , showPart s' "second"]
      nonEmptyParts = filter (not . null) parts
  in case nonEmptyParts of
       [] -> ""
       [p] -> p
       ps -> intercalate ", " (init ps) ++ " and " ++ last ps
