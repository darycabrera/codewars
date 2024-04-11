module Likes where

likes :: [String] -> String
likes [] = "no one likes this"
likes [xs] = xs <> " likes this"
likes (xs : ys : zss) =
  let verb = " like this"
  in
    case zss of
      [] -> xs <> (" and " ++ ys ++ verb)
      [zs] -> xs <> (", " ++ ys ++ " and " ++ zs ++ verb)
      others -> xs <> (", " ++ ys ++ " and " ++ show (length others) ++ " others" ++ verb)

