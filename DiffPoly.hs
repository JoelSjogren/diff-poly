{-# LANGUAGE RankNTypes #-}

import Control.Applicative

--------

data Poly = Poly (forall s. ((s -> s) -> (s -> s)) -> ((s -> s) -> (s -> s)))

poly_0 :: Poly
poly_0 = Poly (pure (pure id))

poly_1 :: Poly
poly_1 = Poly (pure id)

poly_x :: Poly
poly_x = Poly id

poly_add :: Poly -> Poly -> Poly
poly_add (Poly p) (Poly q) = Poly (liftA2 (liftA2 (.)) p q)

poly_mul :: Poly -> Poly -> Poly
poly_mul (Poly p) (Poly q) = Poly (liftA2 (.) p q)

poly_comp :: Poly -> Poly -> Poly
poly_comp (Poly p) (Poly q) = Poly ((.) p q)

--------

type Coeffs = [Nat]
type Nat = Int

coeffs_0 :: Coeffs
coeffs_0 = []

coeffs_1 :: Coeffs
coeffs_1 = [1]

coeffs_x :: Coeffs
coeffs_x = [0,1]

coeffs_add :: Coeffs -> Coeffs -> Coeffs
coeffs_add cs [] = cs
coeffs_add [] ds = ds
coeffs_add (c : cs) (d : ds) = (c+d : coeffs_add cs ds)

coeffs_mul :: Coeffs -> Coeffs -> Coeffs
coeffs_mul [] ds = []
coeffs_mul (c : cs) ds = coeffs_add [c*d | d <- ds] (0 : coeffs_mul cs ds)

coeffs_comp :: Coeffs -> Coeffs -> Coeffs
coeffs_comp [] ds = []
coeffs_comp (c : cs) ds = coeffs_add [c] (coeffs_mul (coeffs_comp cs ds) ds)

--------

encode :: Coeffs -> Poly
encode [] = poly_0
encode (a : as) = poly_add (poly_const a) (poly_mul poly_x (encode as))
  where
    poly_const :: Nat -> Poly
    poly_const n = (foldr poly_add poly_0 (take n (repeat poly_1)))

decode :: Poly -> Coeffs
decode (Poly p) = p level_2 level_1 level_0
  where
    level_0 :: Coeffs
    level_0 = coeffs_0

    level_1 :: Coeffs -> Coeffs
    level_1 cs = coeffs_add cs coeffs_1

    level_2 :: (Coeffs -> Coeffs) -> (Coeffs -> Coeffs)
    level_2 ws cs = coeffs_add cs (coeffs_mul coeffs_x (ws coeffs_0))

--------

type TPoly = (Poly, Poly)  -- tangent space with p + eps*q <-> (p, q)

-- This is a direct implementation of polynomial differentiation,
-- that does not use `decode` and `encode`.

poly_diff :: Poly -> Poly
poly_diff (Poly p) = p_diff
  where
    (p_, p_diff) = p level_2 level_1 level_0  -- p_ == p
    
    level_0 :: TPoly
    level_0 = (poly_0, poly_0)

    level_1 :: TPoly -> TPoly
    level_1 = \(f, f_diff) ->
      (poly_add f poly_1, f_diff)

    level_2 :: (TPoly -> TPoly) -> (TPoly -> TPoly)
    level_2 gen (h, h_diff) = (l, l_diff)
      where
        (g, g_diff) = gen (poly_0, poly_0)
        
        l = poly_add h (poly_mul poly_x g)
        l_diff = poly_add h_diff (poly_add g (poly_mul poly_x g_diff))

-- Example:

-- λ> let p = encode [1,1] in decode (poly_mul p (poly_mul p p))
-- [1,3,3,1,0]
-- λ> let p = encode [1,1] in decode (poly_diff (poly_mul p (poly_mul p p)))
-- [3,6,3,0,0]
