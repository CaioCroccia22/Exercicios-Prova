main = do
  putStrLn "Hello"
  putStrLn "World"

{- 5.1) Crie o tipo TipoProduto que possui os value constructors Escritorio,Informatica, Livro, Filme e Total. O tipo Produto possui um value constructor - de mesmo nome - e os campos valor (Double), tp (TipoProduto) e um value constructor Nada que representa a ausência de um Produto. Deseja-se calcular o valor total de uma compra de modo a não ter nenhuma conversão para inteiro e de forma combinável. Crie uma instância de Monóide para Produto de modo que o retorno sempre terá Total no campo tp e a soma dos dois produtos em valor. -}

data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show)

data Produto = Produto {valor :: Double, tp :: TipoProduto} | Nada deriving (Show)

instance Semigroup Produto where
  Nada <> Nada = Produto 0 Total
  (Produto vl1 _) <> (Produto vl2 _) = Produto (vl1 + vl2) Total
  (Produto vl1 _) <> x = Produto vl1 Total
  x <> (Produto vl1 _) = Produto vl1 Total

instance Monoid Produto where
  mempty = Nada

{- 5.2) Crie uma função totalGeral que recebe uma lista de produtos e retorna o preço total deles usando o monoide anterior. -}
totalGeral :: [Produto] -> Produto
totalGeral lista = foldl (<>) mempty lista

{- 5.3) -}
data Min = Min Int deriving (Ord, Eq, Show)

instance Semigroup Min where
  (Min a) <> (Min b) = Min (min a b)

instance Monoid Min where
  mempty = Min (maxBound)

{-
  Quanto vale a expressão: Min (-32) <> Min (-34) <> Min (-33)?
    - Min (-34)
-}

{- 5.4) -}

minAll :: [Min] -> Min
minAll lista = mconcat lista

{- 5.7) Usando a estrutura de árvore, monte uma função mapa, que jogue uma função passada por parâmetro para todos os elementos de uma árvore. Deixe explícito o tipo desta função -}

data Arvore a = Nulo | Folha a | Ramo a (Arvore a) (Arvore a) deriving (Show)

mapa :: (Int -> Int) -> Arvore Int -> [Int]
mapa f (Ramo x l r) = mapa f l ++ [f x] ++ mapa f r
mapa f (Folha y) = [f y]
mapa f Nulo = []

mapa2 :: (Int -> Int) -> Arvore Int -> Arvore Int
mapa2 f (Ramo x l r) = Ramo (f x) (mapa2 f l) (mapa2 f r)
mapa2 f (Folha y) = Folha (f y)
mapa2 f Nulo = Nulo

{- 5.8) Usando o exercício anterior, some 5 a cada elemento de uma árvore de inteiros -}
somaCinco :: Int -> Int
somaCinco x = x + 5

{- 5.10) Usando a estrutura de árvore vista, faça uma função que some todos os elementos de uma árvore de números -}
tamanhoArv :: Arvore Int -> Int
tamanhoArv (Ramo x l r) = tamanhoArv l + 1 + tamanhoArv r
tamanhoArv (Folha y) = 1
tamanhoArv Nulo = 0

somarElementos :: Arvore Int -> Int
somarElementos (Ramo x l r) = somarElementos l + x + somarElementos r
somarElementos (Folha y) = y
somarElementos Nulo = 0

{- 5.11) -}
preOrdem :: Show a => Arvore a -> [a]
preOrdem (Ramo x l r) = [x] ++ preOrdem l ++ preOrdem r
preOrdem (Folha y) = [y]
preOrdem Nulo = []

{-          TESTE DE MESA
> Ramo 15 (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo)) (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo))

> preOrdem Ramo 15 (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo)) (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo)) = [15] ++ preOrdem (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo)) ++ preOrdem (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo))

> preOrdem (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo)) = [11] ++ preOrdem (Folha 6) ++ preOrdem (Ramo 12 (Folha 10) Nulo))

> preOrdem Folha 6 = 6
> preOrdem (Ramo 12 (Folha 10) Nulo)) = [12] ++ preOrdem (Folha 10) ++ preOrdem Nulo
> preOrdem Folha 10 = 10
> preOrdem Nulo = []
> preOrdem Ramo 15 (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo)) (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo)) = [15] ++ [11, 6, 12, 10] ++ preOrdem (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo))

> preOrdem (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo)) = [20] ++ preOrdem Nulo ++ preOrdem (Ramo 22 (Folha 21) Nulo)

> preOrdem (Ramo 22 (Folha 21) Nulo) = [22] ++ preOrdem (Folha 21) ++ preOrdem Nulo
> preOrdem Folha 21 = 21
> preOrdem Ramo 15 (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo)) (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo)) = [15] ++ [11, 6, 12, 10] ++ [20,22,21]

> [15, 11, 6, 12, 10, 20, 22, 21]
-}

posOrdem :: Show a => Arvore a -> [a]
posOrdem (Ramo x l r) = posOrdem l ++ posOrdem r ++ [x]
posOrdem (Folha y) = [y]
posOrdem Nulo = []

{-        TESTE DE MESA
> Ramo 15 (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo)) (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo))

> posOrdem Ramo 15 (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo) (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo)) =  posOrdem (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo) ++ posOrdem (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo)) ++ [15]

> posOrdem Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo) = posOrdem (Folha 6) ++ posOrdem (Ramo 12 (Folha 10) Nulo) ++ [11]

> posOrdem Folha 6 = [6]
> posOrdem Ramo 12 (Folha 10) Nulo = posOrdem (Folha 10) ++ posOrdem Nulo ++ [12]
> posOrdem Folha 10 = [10]
> posOrdem Nulo = []
> posOrdem Ramo 15 (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo) (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo)) = [6,10,12,11] ++ posOrdem (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo)) ++ [15]

> posOrdem Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo) = posOrdem Nulo ++ posOrdem Ramo 22 (Folha 21) Nulo ++ [20]

> posOrdem Nulo = []
> posOrdem Ramo 22 (Folha 21) Nulo = posOrdem (Folha 21) ++ posOrdem Nulo ++ [22]
> posOrdem Folha 21 = [21]
> posOrdem Nulo = []
> posOrdem Ramo 15 (Ramo 11 (Folha 6) (Ramo 12 (Folha 10) Nulo) (Ramo 20 Nulo (Ramo 22 (Folha 21) Nulo)) = [6,10,12,11] ++ [21,22,20] ++ [15]

> [6,10,12,21,22,20,15]
-}

{- 7.1) Faça uma instância de Functor para o tipo Coisa, definido no início do capítulo 5. A função g deve "ir	para dentro" em todas as coordenadas de Coisa. No caso de ZeroCoisa, o fmap deve retornar ZeroCoisa -}

data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving (Show)

instance Functor Coisa where
  fmap g ZeroCoisa = ZeroCoisa
  fmap g (UmaCoisa x) = UmaCoisa (g x)
  fmap g (DuasCoisas x y) = DuasCoisas (g x) (g y)

{- 7.2) Aproveitando o exercício anterior, faça uma instância de Applicative Functor para o tipo Coisa -}

instance Applicative Coisa where
  pure = UmaCoisa
  ZeroCoisa <*> _ = ZeroCoisa
  (UmaCoisa f) <*> (UmaCoisa x) = UmaCoisa (f x)
  (UmaCoisa f) <*> (DuasCoisas x y) = DuasCoisas (f x) (f y)
  (DuasCoisas f g) <*> (DuasCoisas x y) = DuasCoisas (f x) (g y)

{- 7.4) Escreva uma instância para Functor e Applicative para o tipo Arvore, visto no capítulo 5 -}

data Arvore a = Nulo | Folha a | Ramo a (Arvore a) (Arvore a) deriving (Show)

instance Functor Arvore where
  fmap g Nulo = Nulo
  fmap g (Folha x) = Folha (g x)
  fmap g (Ramo c l r) = Ramo (g c) (fmap g l) (fmap g r)

instance Applicative Arvore where
  pure x = Folha x
  (<*>) Nulo _ = Nulo
  (<*>) (Folha f) tree = fmap f tree
  (<*>) (Ramo f l r) tree = Ramo (fmap f tree) (l <*> tree) (r <*> tree)

{-8.1)	Faça	um	 tipo		Caixa		com	um	type	 parameter 	a		 e	 três
construtores	chamados		Um	,		Dois		e		Tres		possuindo	um,	dois	e
três	campos	de	tipo		a	,	respectivamente-}

data Caixa a = Um a | Dois a a | Tres a a a deriving (Show)

instance Functor Caixa where
  fmap g (Um x) = Um (g x)
  fmap g (Dois x y) = Dois (g x) (g y)
  fmap g (Tres x y z) = Tres (g x) (g y) (g z)

instance Applicative Caixa where
  pure = Um
  (Um g) <*> (Um y) = Um (g y)
  (Um g) <*> Dois x y = Dois (g x) (g y)
  (Um g) <*> Tres x y z = Tres (g x) (g y) (g z)
  (Dois g h) <*> (Dois x y) = Dois (g x) (h y)
  (Tres g h i) <*> (Tres x y z) = Tres (g x) (h y) (i z)

instance Monad Caixa where
  return x = Um x
  Um x >>= f = f x
  Dois x y >>= f = f y
  Tres x y z >>= f = f z

{-8.2)	 Crie	 uma	 função	 	 mult234	 ::	 Double	 ->	 Caixa
Double		 que	 receba	 uma	 parâmetro	 x	 e	 devolva	 o	 dobro	 de	 x	na
primeira	coordenada,	o	triplo	na	segunda	e	o	quádruplo	na	terceira
usando	o	operador		>>=	.-}

mult234 :: Double -> Caixa Double
mult234 x = Tres x x x >>= \x -> return Tres x (x * 2) (x * 3) (x * 4)

{-8.3)	 Determine	 o	 valor	 das	 expressões	 a	 seguir	 (caso	 seja
possível),	sem	usar	o	GHCi:

	Tres	1	2	3	>>=	mult234	>>=	mult234
  Resposta: Tres 24.0 36.0 48.0

	Dois	2	4	>>=	mult234
  Resposta: Tres 8.0 12.0 16.0

  :kind	Coisa
  Resposta: Coisa :: * -> *

	Dois	2	3	>>=	\_	->	Dois	7 7
  Resposta: Dois 7 7
 -}

{-8.4)	 Faça	 um	 exemplo,	 usando	 a	 notação		do	,	 de	 um	 trecho
qualquer	de	código	usando	sua		Monad	Caixa	.-}

mult345 :: Double -> Caixa Double
mult345 x = do Tres (x * 2) (x * 3) (x * 4)