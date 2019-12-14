-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g031 where

import LI11819 
import Tarefa0_2018li1g031

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Roda, MudaTetromino, MudaParede, Move B], [Move D, MudaTetromino, MudaParede, Move B],[MudaTetromino, MudaTetromino, MudaParede, Move B], 
            [MudaTetromino, MudaParede, Move C, Move D, Move C],[Roda, MudaParede, Move C, Move C], [Desenha, Move C], [Move C, Move C, MudaParede, MudaTetromino, Desenha],
            [MudaTetromino, Move C, Move C, Move C, Desenha] , [Move C, MudaParede, MudaTetromino, Move D] , 
            [Move B, MudaParede, MudaTetromino, Desenha, MudaTetromino, Move D, Desenha], [Move D, Move D, Roda, Roda, Desenha],
            [MudaParede, MudaTetromino, Move C, Move C, Desenha], [Move E, Move E, Move E, Roda, MudaTetromino, Desenha],
            [MudaParede, Roda, Move C, Move C, Move C, Move C, Desenha], [Roda, Move C, MudaTetromino, Desenha, Move D],
            [MudaParede, Roda, MudaTetromino, Desenha, Move B],[Move E, MudaTetromino, Desenha, Move D, Move C],[MudaTetromino, Desenha, Move E],
            [Roda, Move D, Move B, Desenha],[MudaTetromino, Desenha, Move C, Roda, Move E],
            [Move B,Move C,Move E,Move D,Roda,Roda,Roda,MudaTetromino,Roda,Roda,Roda,MudaTetromino,Roda,Roda,Roda,MudaTetromino,Roda,Roda,Roda,
            MudaTetromino,Roda,Roda,Roda,Roda,MudaTetromino,Roda,Roda,Roda,Roda,MudaTetromino,Roda,Roda,Roda,Roda,MudaTetromino,MudaParede,
            MudaParede,Desenha,Move D,Move D,MudaTetromino,Desenha,Move D,Move D,MudaTetromino,Desenha,Move B,Move B,Move B,MudaTetromino,
            Desenha,Move E,Move E,Desenha,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move C,
            Move D,MudaTetromino,Desenha,Move C,Move C,Move C,Desenha,Move D,Move D,Move D,MudaTetromino,Desenha,Move C,Move C,Move C,MudaTetromino,
            Desenha,Move E,Move E,Move E,MudaTetromino,Desenha,Move E,Move E,Move C,Move C],
            [Move B,Move C,Move E,Move E,Move E,Move E,Move E,Move E,Move B,Move B,Move B,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move C,
            Move C,Move C,Move C,Desenha,Move D,Move D,Move D,Roda,Desenha,Move B,Move B,Move B,MudaTetromino,Desenha,Move D,Move D,Move D,Move D,
            Roda,Desenha,Move C,Move C,Move C,Roda,Desenha,Move D,Move D,Move D,Move D,Roda,Desenha,Move B,Move B,Move B,Move B,Roda,Desenha,Move B,
            Move B,Move E,Move E,Move E,Move E,Move E,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,
            Move E,Move E,Move E,Move E,Move C,Move C,Move C,Move D,Move D,Move D,MudaTetromino,MudaTetromino,Desenha,Move D,Move D,Move D,Move D,Move C,
            Move C,Move C,Roda,Desenha,Move E,Move E,Move E,Roda,Desenha,Move C,Move C,Move C,Desenha,Move E,Move E,Move E,Roda,Move C,Move C,Move C,
            Desenha,Desenha,Move B,Move B,Move B,Move B,Move B,Move B,MudaTetromino,Desenha,Move C,Move C,Move C,Move E,Move E,Move E,Move E,Move E,Move E,
            Move E,Move E,Move B,Move B,Move D,Move D,Roda,Desenha,Move B,Move B,Move B,Move B,Roda,Desenha,Move D,Move D,Move D,Move D,Roda,Desenha,Move B,
            Move B,Move B,Roda,Roda,Roda,Roda,Roda,Roda,Roda,Roda,Roda,Roda,Roda,Roda,Roda,Move D,Move D,MudaTetromino,Move D,Move D,Move C,Move D,Move D,
            Move D,Move D,Desenha,Move C,Move C,Move C,Roda,Desenha,Move D,Move D,Move D,Roda,Desenha,Move B,Move B,Move B,Roda,Desenha,Move D,Move D,
            Move D,Move D,Move D,Roda,Move E,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move E,Move E,Move E,Move E,Move E,Move E,
            Move E,Move C,MudaTetromino,Desenha,Move C,Move C,Move C,Move C,Roda,Desenha,Move D,Move D,Move D,Roda,Desenha,Move C,Move C,Move C,Move C,
            Roda,Desenha,Move D,Move D,Move D,Move D,MudaTetromino,Desenha,Move C,Move C,Move C,Roda,MudaTetromino,MudaTetromino,Desenha,Move E,Move E,
            Move E,Roda,Desenha,Move C,Move C,Move C,Move C,Roda,Desenha,Move E,Move E,Move E,Move E,Roda,Desenha,Move E,Move E,Move E,Move E,Move E,Roda]]  
-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao (Move C) (Editor (l,c) d t pa m) = Editor (l-1,c) d t pa m 
instrucao (Move B) (Editor (l,c) d t pa m) = Editor (l+1,c) d t pa m 
instrucao (Move D) (Editor (l,c) d t pa m) = Editor (l,c+1) d t pa m 
instrucao (Move E) (Editor (l,c) d t pa m) = Editor (l,c-1) d t pa m

instrucao (MudaTetromino) (Editor p d I pa m) = Editor p d J pa m 
instrucao (MudaTetromino) (Editor p d J pa m) = Editor p d L pa m 
instrucao (MudaTetromino) (Editor p d L pa m) = Editor p d O pa m 
instrucao (MudaTetromino) (Editor p d O pa m) = Editor p d S pa m 
instrucao (MudaTetromino) (Editor p d S pa m) = Editor p d T pa m 
instrucao (MudaTetromino) (Editor p d T pa m) = Editor p d Z pa m 
instrucao (MudaTetromino) (Editor p d Z pa m) = Editor p d I pa m

instrucao (MudaParede) (Editor p d t Indestrutivel m) = Editor p d t Destrutivel m 
instrucao (MudaParede) (Editor p d t Destrutivel m) = Editor p d t Indestrutivel m

instrucao (Roda) (Editor p C t pa m) = Editor p D t pa m 
instrucao (Roda) (Editor p D t pa m) = Editor p B t pa m 
instrucao (Roda) (Editor p B t pa m) = Editor p E t pa m
instrucao (Roda) (Editor p E t pa m) = Editor p C t pa m 
 

instrucao (Desenha) (Editor (l,c) d t pa m) | d == C = Editor (l,c) d t pa (colocaBlocoAqui (descobrePosicoes (tetrominoParaMatriz t) (l,c)) (Bloco pa) m)
                                            | d == D = Editor (l,c) d t pa (colocaBlocoAqui (descobrePosicoes (rodaMatriz (tetrominoParaMatriz t)) (l,c)) (Bloco pa) m)
                                            | d == B = Editor (l,c) d t pa (colocaBlocoAqui (descobrePosicoes (rodaMatriz(rodaMatriz(tetrominoParaMatriz t))) (l,c)) (Bloco pa) m)
                                            | otherwise = Editor (l,c) d t pa (colocaBlocoAqui (descobrePosicoes (rodaMatriz(rodaMatriz(rodaMatriz(tetrominoParaMatriz t)))) (l,c)) (Bloco pa) m) 

-- * Funções auxiliares para a instrução 'Desenha'

{-| Função auxiliar que irá aplicar a 'atualizaPosicaoMatriz' a todas as posições onde ficaram identificadas peças do tetrónimo. 
Ao usarmos o tetrónimo representado como matriz de bool, identificamos essas posições como sendo as posições da matriz cujo elemento é 'True'
Nota: Utilizou-se o 'rodaMatriz' pois as função 'tetrominoParaMatriz' está definida com os tetrómino com direção para C,e foi necessário rodá-la para colocá-la na direção do Editor.-}

colocaBlocoAqui :: [Posicao] -- ^ Lista de posições onde iremos desenhar
                -> Peca -- ^ Peça a desenhar
                -> Mapa -- ^ Mapa onde iremos desenhar
                -> Mapa -- ^ Mapa resultante

colocaBlocoAqui [] _ m = m
colocaBlocoAqui (h:t) p m = atualizaPosicaoMatriz h p (colocaBlocoAqui t p m)

-- | Função auxiliar que determina as posições numa linha onde será necessário desenhar o novo bloco.

descobrePosicaoLinha :: [Bool] -- ^ Lista de Bool, onde cada True identifica uma posição onde será necessário desenhar
                     -> Posicao -- ^ Posição a partir da qual se começar a criar a lista
                     -> [Posicao] -- ^ Lista de posições onde se irá desenhar, numa linha

descobrePosicaoLinha [] (l,c) = []
descobrePosicaoLinha (h:t) (l,c) | h == True = (l,c) : descobrePosicaoLinha t (l,c+1)
                                 | otherwise = descobrePosicaoLinha t (l,c+1)

-- | Função auxliar que descobre todas as posições onde será necessário desenhar as novas peças.
descobrePosicoes :: [[Bool]] -- ^ Matriz Bool onde iremos descobrir as posições onde existem blocos a desenhar
                 -> Posicao -- ^ Posição a partir da qual iremos criar a lista
                 -> [Posicao] -- ^ Lista de posições onde seguidamente, no mapa, se irá desenhar os novos blocos
                 
descobrePosicoes [] (l,c) = []
descobrePosicoes (h:t) (l,c) = descobrePosicaoLinha h (l,c) ++ descobrePosicoes t (l+1,c) 

-- * Funções principais Tarefa 1 (Continuação)
-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'
instrucoes [] ed = ed
instrucoes (h:t) ed = instrucoes t (instrucao h ed) 

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
{-| Usamos 3 funções auxiliares:
A primeira função cria apenas uma linha de blocos indestrutíveis.
A segunda função aulixar irá criar a linha em que apenas o primeiro e último elemento são blocos indestrutíveis e os restantes são blocos vazios.
A terceira função auxiliar vai replicar a linha criada na segunda função (l-2) vezes, pois temos que retirar as duas linhas que são indestrutíveis-}
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (0,0) = [[]]
mapaInicial (_,0) = [[]]
mapaInicial (0,_) = [[]]
mapaInicial (1,c) = [(replicate c (Bloco Indestrutivel))]  
mapaInicial (l,c) = ((linhaIndestrutivel : linhasMeio) ++ [linhaIndestrutivel])                   
                  where linhaIndestrutivel = replicate c (Bloco Indestrutivel)
                        linhasm = (Bloco Indestrutivel) : (replicate (c-2) (Vazia)) ++ [Bloco Indestrutivel] 
                        linhasMeio = replicate (l-2) linhasm   
-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial inst = instrucoes inst (Editor (posicaoInicial inst) C I Indestrutivel (mapaInicial (dimensaoInicial inst)))  
-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa  -- ^ O 'Mapa' resultante.
constroi is = mapaEditor (editorInicial is)


