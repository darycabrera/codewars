module BabyShark where
f=replicate
s z=z++" shark"
babySharkLyrics=unlines.reverse$"Run away,\8230":foldMap(\x->(x++"!"):(f 3.unwords$(x++","):f 6"doo"))["Let's go hunt",s"Grandpa",s"Grandma",s"Daddy",s"Mommy",s"Baby"]
