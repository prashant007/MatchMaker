
infixl 9 @.
infixl 8 @+
class Semiring r where
  
zero, one :: r
closure :: r -> r
(@+), (@.) :: r -> r -> r

data Matrix a = Scalar a | Matrix [[a]]

closure (Matrix [[x]]) = Matrix [[closure x]]
closure m = mjoin
  (first’ @+ top’ @. rest’ @. left’, top’ @. rest’,rest’ @. left’, rest’)
  where
    (first, top, left, rest) = msplit m
    first’ = closure first
    top’ = first’ @. top
    left’ = left @. first’
    rest’ = closure (rest @+ left’ @. top)


type BlockMatrix a = (Matrix a, Matrix a,Matrix a, Matrix a)

mjoin :: BlockMatrix a -> Matrix a
mjoin (Matrix a, Matrix b, Matrix c, Matrix d) = Matrix ((a ‘hcat‘ b) ++ (c ‘hcat‘ d))
                                                 where hcat = zipWith (++)


msplit :: Matrix a -> BlockMatrix a
msplit (Matrix (row:rows)) = (Matrix [[first]], Matrix [top],Matrix left, Matrix rest)
                             where
                              (first:top) = row
                              (left, rest) = unzip (map (\(x:xs) -> ([x],xs)) rows)


instance Semiring a => Semiring (Matrix a) where
  zero = Scalar zero
  one = Scalar one

  Scalar a @+ Scalar b = Scalar (a @+ b)

  Matrix a @+ Matrix b =
  Matrix (zipWith (zipWith (@+)) a b)

  Scalar s @+ m = m @+ Scalar s

  Matrix [[a]] @+ Scalar b = Matrix [[a @+ b]]

  m @+ s = mjoin (first @+ s, top,left, rest @+ s)
             where (first, top,left, rest) = msplit m

  Scalar a @. Scalar b = Scalar (a @. b)
  Scalar a @. Matrix b = Matrix (map (map (a @.)) b)
  Matrix a @. Scalar b = Matrix (map (map (@. b)) a)

  Matrix a @. Matrix b = Matrix [[foldl1 (@+) (zipWith (@.) row col) | col <- cols] | row <- a]
                         where cols = transpose b