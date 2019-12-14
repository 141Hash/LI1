-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g031 where

import LI11819
import Tarefa0_2018li1g031
import Tarefa1_2018li1g031 

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(3 , (Movimenta C) , (Estado (mapaInicial (20,20)) (listaPlayers) [DisparoLaser 3 (8,4) E])), 
            (2 , (Movimenta D) , (Estado (mapaInicial (20,20)) (listaPlayers) [DisparoCanhao 2 (3,7) D])),
            (0 , (Movimenta D), (Estado (mapaInicial (20,20)) [Jogador (3,7) D 5 1 1] [])),
            (0, (Movimenta C), (Estado (mapaInicial (20,20)) [Jogador (6,6) C 3 2 4] [])),
            (3, (Movimenta E), (Estado (mapaInicial (20,20)) (listaPlayers) [])),
            (2, (Movimenta B), (Estado (mapaInicial (20,20)) (listaPlayers) [])),
            (2, (Dispara Canhao), (Estado (mapaInicial (20,20)) (listaPlayers) [DisparoCanhao 2 (3,7) D])),
            (2, (Dispara Laser), (Estado (mapaInicial (20,20)) (listaPlayers) [(DisparoLaser 2 (3,7) D), (DisparoCanhao 1 (5,6) B)])),
            (0, (Dispara Canhao), (Estado (mapaInicial (20,20)) (listaPlayers) [DisparoCanhao 0 (6,6) D])),
            (1, (Dispara Canhao), (Estado (mapaInicial (20,20)) (listaPlayers) [DisparoCanhao 0 (6,6) D])),
            (3, (Dispara Choque), (Estado (mapaInicial (20,20)) (listaPlayers) [(DisparoChoque 3 5),(DisparoCanhao 0 (5,6) C)])),
            (0, (Movimenta D), (Estado (mapaInicial (20,20)) [(Jogador (6,6) D 2 4 5), (Jogador (6,8) C 2 6 7)] [])),
            (0, (Movimenta D),(Estado (mapaInicial(20,20)) [(Jogador (6,8) D 2 3 4), (Jogador (7,10) C 2 3 4)] [])),
            (0, (Movimenta C),(Estado (mapaInicial(20,20)) [(Jogador (6,8) C 2 3 4), (Jogador (7,10) C 2 3 4)] [DisparoChoque 1 5])),
            (0, (Movimenta C),(Estado (mapaInicial(20,20)) [(Jogador (4,4) C 2 3 4), (Jogador (7,10) C 2 3 4)] [DisparoChoque 1 5])),
            (0, (Movimenta E),(Estado (mapaInicial(20,20)) [(Jogador (6,8) C 2 3 4), (Jogador (7,10) C 2 3 4)] [DisparoChoque 1 5])),
            (2, (Movimenta D),(Estado (mapaInicial(20,20)) (listaPlayers) [DisparoChoque 1 5])),
            (0, (Movimenta E),(Estado (mapaInicial (20,20)) [(Jogador (10,10) E 2 3 4), (Jogador (10,8) C 7 8 9)] [])),
            (0 ,(Movimenta C), (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 1 2 3] [])),
            (0, (Movimenta B), (Estado (mapaInicial (20,20)) [(Jogador (8,17) B 1 2 3)] [])),
            (0, (Movimenta B), (Estado (mapaInicial (20,20)) [Jogador (17,17) B 1 2 3] [])),
            (0, (Movimenta B), (Estado (mapaInicial (20,20)) [(Jogador (8,17) B 1 2 3) , (Jogador (10,17) D 6 7 8)] []))]

            

            
{-| Jogadores utilizados para correr a funçaõ 'testesT2' 
 Jogador 0 = Player 1 -}
jogador0 = Jogador (6,6) C 3 2 4
-- | Jogador 1 = Player 2
jogador1 = Jogador (5,6) B 0 4 3
-- | Jogador 2 = Player 3
jogador2 = Jogador (3,7) D 5 0 1
-- | Jogador 3 = Player 4
jogador3 = Jogador (8,1) E 2 2 8
-- | Lista de jogadores utilizados nos testes
listaPlayers = [jogador0,jogador1,jogador2,jogador3]

-- * Função principal
-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
-- | Movimentos
jogada n (Movimenta C) (Estado m js ds) = Estado m (substituiJogador n (moveJogadorC (Movimenta C) (Estado m js ds) (descobreJogador n js)) js) ds
jogada n (Movimenta B) (Estado m js ds) = Estado m (substituiJogador n (moveJogadorB (Movimenta B) (Estado m js ds) (descobreJogador n js)) js) ds
jogada n (Movimenta E) (Estado m js ds) = Estado m (substituiJogador n (moveJogadorE (Movimenta E) (Estado m js ds) (descobreJogador n js)) js) ds
jogada n (Movimenta D) (Estado m js ds) = Estado m (substituiJogador n (moveJogadorD (Movimenta D) (Estado m js ds) (descobreJogador n js)) js) ds

-- | Disparos
jogada n (Dispara Canhao) (Estado m js ds) = Estado m js (( executaDisparoCanhao n (Dispara Canhao) (descobreJogador n js)) ++ ds)
jogada n (Dispara Laser) (Estado m js ds) = Estado m (substituiJogador n (tiraBalas (Dispara Laser) (descobreJogador n js))  js) (( executaDisparoLaser n (Dispara Laser) (descobreJogador n js)) ++ ds)
jogada n (Dispara Choque) (Estado m js ds) = Estado m (substituiJogador n (tiraBalas (Dispara Choque) (descobreJogador n js))  js) (( executaDisparoChoque n (Dispara Choque) (descobreJogador n js)) ++ ds)  


-- * Funções auxiliares
-- | Esta função auxiliar irá descobrir a qual jogador nos estamos a referir dentro uma lista de jogadores. 
descobreJogador :: Int -- ^ Indice do jogador
                -> [Jogador] -- ^ Lista onde se vai procurar o jogador
                -> Jogador  -- ^ Jogador que se pretende descobrir  
descobreJogador 0 (h:t) = h 
descobreJogador n (h:t) = descobreJogador (n-1) t 

{-| As funcões seguintes têm o intuito de determinar o resultado de um jogador aṕos lhe aplicarmos um movimento. 
   Sendo que quando indicarmos ao jogador para se movimentar contra a parede a sua posição nao se vai alterar.
   Nota : Em todas as funções auxiliares, quando queremos determinar se o tanque tinha ou não uma parede ou um bloco à sua direita ou para baixo,
   foi necessário usar os vetores (0,2) e (2,0), respetivamente.
   Função auxiliar que calcula todas as restrições a implementar quando se executa a jogada 'Movimenta C' -}
moveJogadorC :: Jogada -- ^ Jogada a implementar, neste caso, Movimenta C
             -> Estado -- ^ Estado de onde se retirará o mapa
             -> Jogador -- ^ Jogador afetado
             -> Jogador -- ^ Jogador resultante após jogada

moveJogadorC (Movimenta C) (Estado m js ds) (Jogador (l,c) d1 vida laser choque) 
     | d1 == C && (vida == 0) = Jogador (l,c) d1 vida laser choque
     | d1 == C && (eBordaMatriz (l-1,c) m == True) = Jogador (l,c) d1 vida laser choque
     | d1 == C && (encontraPosicaoMatriz (l-1,c) m == Bloco Indestrutivel) = Jogador (l,c) d1 vida laser choque
     | d1 == C && (encontraPosicaoMatriz (l-1,c) m == Bloco Destrutivel) = Jogador (l,c) d1 vida laser choque
     | d1 == C && (temJogadorNaPosicaoC (Jogador (l,c) d1 vida laser choque) js) == True = (Jogador (l,c) d1 vida laser choque)
     | d1 == C && (existeChoque ds) && (comparaAreaAfetada (Jogador (l,c) d1 vida laser choque) (descobreJogador (quemDisparouChoque (devolveDisparo ds)) js)) = (Jogador (l,c) d1 vida laser choque)   
     | d1 == C = Jogador (l-1,c) d1 vida laser choque   
     | otherwise = Jogador (l,c) C vida laser choque 

-- | Função auxiliar que calcula todas as restrições a implementar quando se executa a jogada 'Movimenta B'
moveJogadorB :: Jogada -- ^ Jogada a implementar, neste caso, Movimenta B
             -> Estado -- ^ Estado de onde se retirará o mapa
             -> Jogador -- ^ Jogador afetado
             -> Jogador -- ^ Jogador resultante após jogada

moveJogadorB (Movimenta B) (Estado m js ds) (Jogador (l,c) d1 vida laser choque) 
     | d1 == B && (vida == 0) = Jogador (l,c) d1 vida laser choque
     | d1 == B && (eBordaMatriz (l+2,c) m == True) = Jogador (l,c) d1 vida laser choque
     | d1 == B && (encontraPosicaoMatriz (l+2,c) m == Bloco Indestrutivel) = Jogador (l,c) d1 vida laser choque
     | d1 == B && (encontraPosicaoMatriz (l+2,c) m == Bloco Destrutivel) = Jogador (l,c) d1 vida laser choque
     | d1 == B && (temJogadorNaPosicaoB (Jogador (l,c) d1 vida laser choque) js) == True = (Jogador (l,c) d1 vida laser choque)
     | d1 == B && (existeChoque ds) && (comparaAreaAfetada (Jogador (l,c) d1 vida laser choque) (descobreJogador (quemDisparouChoque (devolveDisparo ds)) js)) = (Jogador (l,c) d1 vida laser choque)   
     | d1 == B = Jogador (l+1,c) d1 vida laser choque
     | otherwise = Jogador (l,c) B vida laser choque

-- | Função auxiliar que calcula todas as restrições a implementar quando se executa a jogada 'Movimenta D'
moveJogadorD :: Jogada -- ^ Jogada a implementar, neste caso, Movimenta D
             -> Estado -- ^ Estado de onde se retirará o mapa
             -> Jogador -- ^ Jogador afetado
             -> Jogador -- ^ Jogador resultante após jogada

moveJogadorD (Movimenta D) (Estado m js ds) (Jogador (l,c) d1 vida laser choque) 
     | d1 == D && (vida == 0) = Jogador (l,c) d1 vida laser choque
     | d1 == D && (eBordaMatriz (l,c+2) m == True) = Jogador (l,c) d1 vida laser choque
     | d1 == D && (encontraPosicaoMatriz (l,c+2) m) == Bloco Indestrutivel = Jogador (l,c) d1 vida laser choque
     | d1 == D && (encontraPosicaoMatriz (l,c+2) m) == Bloco Destrutivel = Jogador (l,c) d1 vida laser choque
     | d1 == D && (temJogadorNaPosicaoD (Jogador (l,c) d1 vida laser choque) js) == True = (Jogador (l,c) d1 vida laser choque)
     | d1 == D && (existeChoque ds) && (comparaAreaAfetada (Jogador (l,c) d1 vida laser choque) (descobreJogador (quemDisparouChoque (devolveDisparo ds)) js)) = (Jogador (l,c) d1 vida laser choque)   
     | d1 == D = Jogador (l,c+1) d1 vida laser choque
     | otherwise = Jogador (l,c) D vida laser choque

-- | Função auxiliar que calcula todas as restrições a implementar quando se executa a jogada 'Movimenta E'
moveJogadorE :: Jogada -- ^ Jogada a implementar, neste caso, Movimenta D
             -> Estado -- ^ Estado de onde se retirará o mapa
             -> Jogador -- ^ Jogador afetado
             -> Jogador -- ^ Jogador resultante após jogada

moveJogadorE (Movimenta E) (Estado m js ds) (Jogador (l,c) d1 vida laser choque) 
     | d1 == E && (vida == 0) = Jogador (l,c) d1 vida laser choque
     | d1 == E && (eBordaMatriz (l,c-1) m) = Jogador (l,c) d1 vida laser choque
     | d1 == E && (encontraPosicaoMatriz (l,c-1) m) == Bloco Indestrutivel = Jogador (l,c) d1 vida laser choque
     | d1 == E && (encontraPosicaoMatriz (l,c-1) m) == Bloco Destrutivel = Jogador (l,c) d1 vida laser choque
     | d1 == E && (temJogadorNaPosicaoE (Jogador (l,c) d1 vida laser choque) js) == True = (Jogador (l,c) d1 vida laser choque)
     | d1 == E && (existeChoque ds) && (comparaAreaAfetada (Jogador (l,c) d1 vida laser choque) (descobreJogador (quemDisparouChoque (devolveDisparo ds)) js)) = (Jogador (l,c) d1 vida laser choque)   
     | d1 == E = Jogador (l,c-1) d1 vida laser choque
     | otherwise = Jogador (l,c) E vida laser choque 
 

-- |Função auxiliar que irá substituir o jogador afetado na lista de jogadores.
substituiJogador :: Int -- ^ Índice do jogador a substituir
                 -> Jogador -- ^ Jogador a substituir
                 -> [Jogador] -- ^ Lista onde se substituirá jogador
                 -> [Jogador] -- ^ Lista resultante após substituição

substituiJogador _ j [] = []
substituiJogador 0 j (h:t) = (j:t)
substituiJogador n j (h:t) = h : substituiJogador (n-1) j t
{-| Conjunto de auxiliares para averiguar se o tanque irá mover-se para uma posição onde já esteja outro tanque.
 Auxiliar que averigua se existe tanque na direção 'Cima' -}
temJogadorNaPosicaoC :: Jogador 
                     -> [Jogador] 
                     -> Bool

temJogadorNaPosicaoC (Jogador (l,c) d vida laser choque) [] = False 
temJogadorNaPosicaoC (Jogador (l,c) d vida laser choque) ((Jogador (l1,c1) _ _ _ _):t) | d /= C = False
                                                                                       | d == C && (somaVetores (l,c) (-2,0) == (l1,c1)) = True
                                                                                       | d == C && (somaVetores (l,c) (-2,0) == (l1,c1-1)) = True
                                                                                       | d == C && (somaVetores (l,c) (-2,0) == (l1,c1+1)) = True
                                                                                       | otherwise = temJogadorNaPosicaoC (Jogador (l,c) d vida laser choque) t
-- | Auxiliar que averigua se existe tanque na direção 'Baixo'
temJogadorNaPosicaoB :: Jogador 
                     -> [Jogador] 
                     -> Bool

temJogadorNaPosicaoB (Jogador (l,c) d vida laser choque) [] = False 
temJogadorNaPosicaoB (Jogador (l,c) d vida laser choque) ((Jogador (l1,c1) _ _ _ _):t) | d /= B = False
                                                                                       | d == B && (somaVetores (l,c) (2,0) == (l1,c1)) = True
                                                                                       | d == B && (somaVetores (l,c) (2,0) == (l1,c1-1)) = True
                                                                                       | d == B && (somaVetores (l,c) (2,0) == (l1,c1+1)) = True
                                                                                       | otherwise = temJogadorNaPosicaoB (Jogador (l,c) d vida laser choque) t 
-- | Auxiliar que averigua se existe tanque na direção 'Direita'
temJogadorNaPosicaoD :: Jogador 
                     -> [Jogador] 
                     -> Bool 

temJogadorNaPosicaoD (Jogador (l,c) d vida laser choque) [] = False 
temJogadorNaPosicaoD (Jogador (l,c) d vida laser choque) ((Jogador (l1,c1) _ _ _ _):t) | d /= D = False
                                                                                       | d == D && (somaVetores (l,c) (0,2) == (l1,c1)) = True
                                                                                       | d == D && (somaVetores (l,c) (0,2) == (l1-1,c1)) = True
                                                                                       | d == D && (somaVetores (l,c) (0,2) == (l1+1,c1)) = True
                                                                                       | otherwise = temJogadorNaPosicaoD (Jogador (l,c) d vida laser choque) t 
-- | Auxiliar que averigua se existe tanque na direção 'Esquerda'
temJogadorNaPosicaoE :: Jogador
                     -> [Jogador] 
                     -> Bool

temJogadorNaPosicaoE (Jogador (l,c) d vida laser choque) [] = False 
temJogadorNaPosicaoE (Jogador (l,c) d vida laser choque) ((Jogador (l1,c1) _ _ _ _):t) | d /= E = False
                                                                                       | d == E && (somaVetores (l,c) (0,-2) == (l1,c1)) = True
                                                                                       | d == E && (somaVetores (l,c) (0,-2) == (l1-1,c1)) = True
                                                                                       | d == E && (somaVetores (l,c) (0,-2) == (l1+1,c1)) = True
                                                                                       | otherwise = temJogadorNaPosicaoE (Jogador (l,c) d vida laser choque) t
{-| 
 Estas funções, dadas a jogada "Dispara Arma" a um jogador, irá determinar a posicao que o disparo vai ocupar. 
 Sendo que se o jogador nao tiver vidas, o disparo não irá acontecer. 
 E nos casos das jogadas "Dispara Laser" "Dispara Choque" se o jogador não possuir esse tipo de munições o disparo não ocorre. -}

-- | Função auxiliar que restringe as limitações da jogada 'Dispara Laser' e devolve a lista composta pelo disparo resultante
executaDisparoLaser :: Int -- ^ índice do jogador a disparar
                    -> Jogada -- ^ Disparo a efetuar, neste caso, disparo laser
                    -> Jogador -- ^ Jogador que disparará
                    -> [Disparo] -- ^ Lista composta pelo disparo efetuado, que será depois usado para atualizar a lista de disparos do Estado Inicial usado na função 'jogada'

executaDisparoLaser n (Dispara Laser) (Jogador (l,c) d vida laser choque) | vida == 0 = []
                                                                          | laser == 0 = []
                                                                          | d == C = [DisparoLaser n (l-1,c) C]
                                                                          | d == B = [DisparoLaser n (l+1,c) B]
                                                                          | d == D = [DisparoLaser n (l,c+1) D]
                                                                          | d == E = [DisparoLaser n (l,c-1) E]

-- | Função auxiliar que restringe as limitações da jogada 'Dispara Canhao' e devolve a lista composta pelo disparo resultante
executaDisparoCanhao :: Int -- ^ índice do jogador a disparar
                     -> Jogada -- ^ Disparo a efetuar, neste caso, disparo canhao
                     -> Jogador -- ^ Jogador que disparará
                     -> [Disparo] -- ^ Lista composta pelo disparo efetuado, que será depois usado para atualizar a lista de disparos do Estado Inicial usado na função 'jogada'

executaDisparoCanhao n (Dispara Canhao) (Jogador (l,c) d vida laser choque) | vida == 0 = []
                                                                            | d == C = [DisparoCanhao n (l-1,c) C]
                                                                            | d == B = [DisparoCanhao n (l+1,c) B]
                                                                            | d == D = [DisparoCanhao n (l,c+1) D] 
                                                                            | d == E = [DisparoCanhao n (l,c-1) E]

-- | Função auxiliar que restringe as limitações da jogada 'Dispara Choque' e devolve a lista composta pelo disparo resultante
executaDisparoChoque :: Int -- ^ índice do jogador a disparar
                     -> Jogada -- ^ Disparo a efetuar, neste caso, disparo choque
                     -> Jogador -- ^ Jogador que disparará
                     -> [Disparo] -- ^ Lista composta pelo disparo efetuado, que será depois usado para atualizar a lista de disparos do Estado Inicial usado na função 'jogada'

executaDisparoChoque n (Dispara Choque) (Jogador (l,c) d vida laser choque) | vida == 0 = []
                                                                            | choque == 0 = []
                                                                            | otherwise = [DisparoChoque n 5]

-- | Função auxiliar que irá retirar as munições de um jogador ou avaliar se este já não as possui
tiraBalas :: Jogada -- ^ Tipo de jogada de disparo efetuada
          -> Jogador -- ^ Jogador afetado pela jogada
          -> Jogador -- ^ Jogador resultante, com número atualizado de balas
tiraBalas (Dispara Laser) (Jogador (l,c) d vida 0 choque) = Jogador (l,c) d vida 0 choque
tiraBalas (Dispara Laser) (Jogador (l,c) d vida laser choque) = Jogador (l,c) d vida (laser - 1) choque
tiraBalas (Dispara Choque) (Jogador (l,c) d vida laser 0) = Jogador (l,c) d vida laser 0
tiraBalas (Dispara Choque) (Jogador (l,c) d vida laser choque) = Jogador (l,c) d vida laser (choque -1)

-- | Função auxilair que avetigua se existe um 'DisparoChoque' numa lista de disparos
existeChoque :: [Disparo] -- ^ Lista de disparos onde se vai analisar se existe 'DisparoChoque'
             -> Bool 
existeChoque [] = False
existeChoque ((DisparoChoque n ti) : t) = True
existeChoque (h:t) = existeChoque t 

-- | Auxiliar que vai devolver um 'DisparoChoque' de uma lista de disparos
devolveDisparo :: [Disparo] -- ^ Lista de disparos onde está o 'DisparoChoque'
               -> Disparo -- ^ 'DisparoChoque' pretendido
devolveDisparo ((DisparoChoque n ti):t) = DisparoChoque n ti
devolveDisparo (h:t) = devolveDisparo t 

-- | Auxiliar que identificará, através do identificador existente em 'DisparoChoque', o jogador que efetuou o disparo
quemDisparouChoque :: Disparo -- ^ 'DisparoChoque' analisado
                   -> Int -- ^ Identificador do jogador de executou o disparo
quemDisparouChoque (DisparoChoque n ti) = n 

-- | Auxiliar que servirá para comparar a área abrangida pelo choque e a posição do jogador afetado
comparaAreaAfetada :: Jogador -- ^ Jogador a ser afetado pelo choque
                   -> Jogador -- ^ Jogador de executou 'DisparoChoque'
                   -> Bool
comparaAreaAfetada (Jogador (l,c) d vida laser choque) (Jogador (l1,c1) _ _ _ _) 
 | (l <= l1+3) && (l >= l1-3) && (c <= c1+3) && (c >= c1-3) = True 
 | otherwise = False 

 