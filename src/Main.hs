import Yew.Parser

main = do
	code <- getContents
	case (parseYew code) of
		Right x -> print x
		Left  y -> print y
