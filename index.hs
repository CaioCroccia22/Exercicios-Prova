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