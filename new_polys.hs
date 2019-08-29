-- A variable of a polynomial can be thought of as an infinite list of coefficients, where the index
-- of each element is the power to raise the variable to, multiplied by the coefficient
-- A polynomial `x^2 + 4x + 6` can, for example, be represented by the list `[6, 4, 1, 0, ..., 0]`.
data PolyVar = PolyVar Char [Int]
