{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

import GHC.TypeLits
import GHC.Exts

--                     Coefficient  Power
data VarPart = VarPart Int          Int

--                     Name
data PolyVar = PolyVar Char [VarPart]

instance Show PolyVar where
  -- Dropping the first 3 chars is a messy way to get rid of the leading unwanted " + "
  show (PolyVar name parts) = drop 3 $
                                foldr (\part -> (++" + "++(varStr name part))) "" (sortByCoeff parts)
                              where varStr name (VarPart 1 power) = [name]++(show power)
                                    varStr name (VarPart coeff 0)     = (show coeff)
                                    varStr name (VarPart coeff 1)     = (show coeff)++[name]
                                    varStr name (VarPart coeff power) = (show coeff)++[name]++(show power)
                                    sortByCoeff = sortWith (\(VarPart _ power) -> power)

data Poly = Poly [PolyVar]

instance Show Poly where
  -- Again, dropping 3 chars gets rid of the leading " + "
  show (Poly parts) = drop 3 $ foldr (\partStr -> (++" + "++partStr)) "" $ map (show) parts

main :: IO ()
main = do
  putStrLn $ ("Hello, World: "++) $ show (Poly [(PolyVar 'x' [(VarPart 5 1),
                                                              (VarPart 3 2),
                                                              (VarPart 7 0)]),
                                                (PolyVar 'y' [(VarPart 14 1)])])
