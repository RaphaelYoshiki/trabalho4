type ProdutoEstoque = ([Char], Float, Int)
type EstoqueProdutos = [([Char], Float, Int)]

type ProdutoCarrinho = ([Char], Int)
type CarrinhoCompra = [([Char], Int)]


--Coisas do estoque
adicionaProduto :: ([Char], Float, Int) -> [([Char], Float, Int)]
adicionaProduto produto = produto:[]

monE :: ProdutoEstoque
monE = ("monitor", 500, 100)

telE :: ProdutoEstoque
telE = ("telefone", 150, 300)

tecE :: ProdutoEstoque
tecE = ("teclado", 70, 50)

mouE :: ProdutoEstoque
mouE = ("mouse", 50, 50)

estoque :: EstoqueProdutos
estoque = adicionaProduto monE ++ adicionaProduto telE ++ adicionaProduto tecE ++ adicionaProduto mouE


--Coisas do carrinho
adicionaItem :: ([Char], Int) -> [([Char], Int)]
adicionaItem produto = produto:[]

monC :: ProdutoCarrinho
monC = ("monitor", 2)

telC :: ProdutoCarrinho
telC = ("telefone", 5)

tecC :: ProdutoCarrinho
tecC = ("teclado", 2)

carrinho :: CarrinhoCompra
carrinho = adicionaItem monC ++ adicionaItem telC ++ adicionaItem tecC


--Fim de compra

get3rd :: ([Char], Float, Int) -> Int
get3rd(_, _, x) = x

f1 :: ProdutoEstoque
f1 = ("monitor", 500 , get3rd monE - snd monC)

f2 :: ProdutoEstoque
f2 = ("telefone", 150 , get3rd telE - snd telC)

f3 :: ProdutoEstoque
f3 = ("teclado", 70, get3rd tecE - snd tecC)

estoqueFinal :: EstoqueProdutos
estoqueFinal = adicionaProduto f1 ++ adicionaProduto f2 ++ adicionaProduto f3 ++ adicionaProduto mouE

total :: Int
total = snd monC * 500 + snd telC * 150 + snd tecC * 70


main :: IO ()
main = do

putStrLn "Estoque: "
print(estoqueFinal)
putStrLn "Carrinho: "
print(carrinho)
putStrLn "Total: "
print(total)