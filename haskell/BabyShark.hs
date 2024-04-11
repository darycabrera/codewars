module BabyShark where
f=replicate
s z=z++" shark"
babySharkLyrics=unlines$(foldMap(\x->(f 3.unwords$(x++","):f 6"doo")++[x++"!"])[s"Baby",s"Mommy",s"Daddy",s"Grandma",s"Grandpa","Let's go hunt"])++["Run away,\8230"]
