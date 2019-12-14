
-- Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g031 where

import LI11819

import Data.List  
-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.

v1,v2 :: Vetor
v1 = (4 , 6)
v2 = (6 , 3)

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (l1,c1) (l2,c2) = ( l1+l2 , c1+c2 )

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (l1,c1) (l2,c2) = ( l1-l2 , c1-c2 )

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor  a (l1,c1) = (a*l1 , a*c1)
-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distânciaa à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (l1,c1) = (c1,-l1)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (l1 , c1) = (l1, -c1)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (l1 , c1) = (-l1, c1) 
-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor E = (0,-1)
direcaoParaVetor D = (0,1)
direcaoParaVetor B = (1,0)
direcaoParaVetor C = (-1,0)


-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x lista = if ((x >= 0) && (x < length lista)) then True else False
                         
-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
dimensaoMatriz :: Matriz a -> Dimensao 
dimensaoMatriz [] = (0,0)
dimensaoMatriz ([]:t) = (0,0)
dimensaoMatriz m = ((length m) , (length (head m)))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (x,y) m | x <= ((length m) -1) && x >= 0 && y >= 0 && y <= ((length (head m) -1)) = True
                             | otherwise = False

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (x,y) m = if ( x == 0 || y == 0 || x == length m -1 || y == length (head m) -1 ) then True else False 

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz I = [[False,True,False,False],
                         [False,True,False,False],
                         [False,True,False,False],
                         [False,True,False,False]] 
tetrominoParaMatriz J = [[False,True,False],
                         [False,True,False],
                         [True ,True,False]] 
tetrominoParaMatriz L = [[False,True,False],
                         [False,True,False],
                         [False,True,True ]] 
tetrominoParaMatriz S = [[False,True ,True ],
                         [True ,True ,False],
                         [False,False,False]] 
tetrominoParaMatriz Z = [[True ,True ,False],
                         [False,True ,True ],
                         [False,False,False]] 
tetrominoParaMatriz T = [[False,False,False],
                         [True ,True ,True],
                         [False,True ,False]]
tetrominoParaMatriz O = [[True,True],
                         [True,True]] 
-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a 
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista n (h:t) = encontraIndiceLista (n-1) t 

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista n x [] = []
atualizaIndiceLista n x (h:t) | n == 0 = x : t 
                              | otherwise = h : (atualizaIndiceLista (n-1) x t) 

-- ** Funções sobre matrizes.
-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a
rodaMatriz [] = []
rodaMatriz [[x]] = [[x]]
rodaMatriz m = transpose (reverse m )
-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH [] = []
inverteMatrizH (h:t) = reverse h : inverteMatrizH t  

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV [] = []
inverteMatrizV (h:t) = reverse (h:t)

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (0,0) n = [] 
criaMatriz (x,y) n = replicate x (replicate y n)
-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (0,y) (h:t) = encontraIndiceLista y h 
encontraPosicaoMatriz (x,y) (h:t) = encontraPosicaoMatriz (x-1,y) t 
-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.

atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (0,y) a (h:t) = atualizaIndiceLista y a h : t 
atualizaPosicaoMatriz (x,y) a [] = []
atualizaPosicaoMatriz (x,y) a (h:t) = h : atualizaPosicaoMatriz (x-1,y) a t 