sqrtrec x pi ps = if ps * ps - x < 0.00001
    then (if ps * ps - x > -0.00001
        then ps
        else sqrtrec x ps (2 * ps))
    else sqrtrec x pi ((pi + ps) / 2)

mysqrt x = sqrtrec x 0 x

func x = [[(head x)], (tail x), [(last x)], (init x)]

size x = length x

isempty x = null x

fourcycle x = take (4 * length x) (cycle x)

doublebigten = [x*2 | x <- 15:[1..10], x*2 >= 12]

removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z']]

tolist t = [(fst t), (snd t)]