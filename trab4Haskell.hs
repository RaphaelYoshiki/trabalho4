import Data.Maybe

type ProdutoEstoque = ([Char], Float, Int)
type EstoqueProdutos = [([Char], Float, Int)]

type ProdutoCarrinho = ([Char], Int)
type CarrinhoCompra = [([Char], Int)]


--Coisas do estoque
adicionaProduto :: ([Char], Float, Int) -> [([Char], Float, Int)] -> [([Char], Float, Int)]
adicionaProduto produto estoqueAtual = produto:estoqueAtual

monE :: ProdutoEstoque
monE = ("monitor", 500, 100)

telE :: ProdutoEstoque
telE = ("telefone", 150, 300)

tecE :: ProdutoEstoque
tecE = ("teclado", 70, 50)

mouE :: ProdutoEstoque
mouE = ("mouse", 50, 50)

cpuE :: ProdutoEstoque
cpuE = ("cpu", 10050, 50)

estoque :: EstoqueProdutos
estoque = adicionaProduto monE $ adicionaProduto telE $ adicionaProduto tecE $ adicionaProduto mouE $ adicionaProduto cpuE []


--Coisas do carrinho
adicionaItem :: ([Char], Int) -> [([Char], Int)]
adicionaItem produto = produto:[]

monC :: ProdutoCarrinho
monC = ("monitor", 2)

telC :: ProdutoCarrinho
telC = ("telefone", 5)

tecC :: ProdutoCarrinho
tecC = ("teclado", 2)

cpuC :: ProdutoCarrinho
cpuC = ("cpu", 1)

carrinho :: CarrinhoCompra
carrinho = adicionaItem monC ++ adicionaItem telC ++ adicionaItem tecC ++ adicionaItem cpuC


--Fim de compra
get1st :: ([Char], Float, Int) -> [Char]
get1st(x, _, _) = x

get2nd :: ([Char], Float, Int) -> Float
get2nd(_, x, _) = x

get3rd :: ([Char], Float, Int) -> Int
get3rd(_, _, x) = x

f1 :: ProdutoEstoque
f1 = ("monitor", 500 , get3rd monE - snd monC)

f2 :: ProdutoEstoque
f2 = ("telefone", 150 , get3rd telE - snd telC)

f3 :: ProdutoEstoque
f3 = ("teclado", 70, get3rd tecE - snd tecC)

f4 :: ProdutoEstoque
f4 = ("cpu", 10050, get3rd cpuE - snd cpuC)

estoqueFinal :: EstoqueProdutos
estoqueFinal = adicionaProduto f1 $ adicionaProduto f2 $ adicionaProduto f3 $ adicionaProduto f4 $ adicionaProduto mouE estoque


--Cálculo do total
--total :: Float
--total = (fromIntegral (snd monC)) * get2nd monE + (fromIntegral (snd telC)) * get2nd telE + (fromIntegral (snd tecC)) * get2nd tecE
getQtd :: ([Char], Int) -> Float
getQtd (_, x) = fromIntegral x

minhalookup :: Eq a => a -> [(a,b)] -> Maybe b
minhalookup _ [] = Nothing
minhalookup aux1 ((val1,val2):aux2)
    |aux1 == val1 = Just val2
    |otherwise = minhalookup aux1 aux2

getPrice :: ([Char], Float, Int) -> ([Char], Float)
getPrice produto = (get1st produto, get2nd produto)
    
listPrice :: [([Char], Float)]
listPrice = map getPrice estoque

calcMult :: ([Char], Int) -> Float
calcMult prod = (getQtd prod) * fromJust (minhalookup (fst prod) listPrice)

listMult :: [Float]
listMult = map calcMult carrinho

sumTotal :: Float
sumTotal = sum listMult


--Main
main :: IO ()
main = do

putStrLn "Estoque: "
print(estoqueFinal)
putStrLn "Carrinho: "
print(carrinho)
putStrLn "Total: "
print(sumTotal)
