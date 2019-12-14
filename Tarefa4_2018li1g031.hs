-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g031 where

import LI11819
import Tarefa0_2018li1g031
import Tarefa1_2018li1g031
import Tarefa2_2018li1g031

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [(Estado (mapaInicial (20,20)) [Jogador (12,12) C 2 3 4] [DisparoChoque 0 4]),
            (Estado (mapaInicial (20,20)) [Jogador (12,12) C 2 3 4] [DisparoChoque 0 0]),
            (Estado (mapaInicial (20,20)) listaPlayers [DisparoLaser 0 (5,6) C]),
            (Estado (mapaInicial (20,20)) listaPlayers [(DisparoLaser 0 (5,6) C),(DisparoChoque 1 4)]),
            (Estado (mapaInicial (20,20)) [(Jogador (12,12) C 2 3 4),(Jogador (5,12) D 2 2 4)] [DisparoLaser 0 (11,12) C]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                     [Jogador (3,1) D 2 3 4] [DisparoLaser 0 (3,2) D]),
            (Estado (mapaInicial (20,20)) [(Jogador (12,12) D 2 3 4),(Jogador (16,9) C 2 3 4)] [DisparoLaser 0 (12,12) D, DisparoCanhao 1 (12,15) C]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                     [Jogador (3,1) D 2 3 4] [DisparoLaser 0 (3,2) D]),
            
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                     [Jogador (3,1) D 2 3 4] [DisparoLaser 0 (3,2) D]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                     [Jogador (3,1) D 2 3 4] [DisparoLaser 0 (3,2) D]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                     [Jogador (3,1) C 2 3 4] [DisparoLaser 0 (2,1) C]),
          (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                     [Jogador (3,1) D 2 3 4] [DisparoCanhao 0 (3,2) D]),
          (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (1,1) D 2 3 4] [DisparoCanhao 0 (1,3) D]),
          (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                     [Jogador (3,1) D 2 3 4] [DisparoCanhao 0 (3,2) D]),
          (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                     [Jogador (3,1) D 2 3 4] [DisparoCanhao 0 (3,2) D]),
          (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                     [Jogador (3,1) C 2 3 4] [DisparoCanhao 0 (1,1) C]),
          (Estado (mapaInicial(20,20)) [Jogador (10,10) C 2 4 5] [DisparoCanhao 0 (8,10) C, DisparoChoque 0 2]),
          (Estado (mapaInicial (20,20)) [(Jogador (12,12) C 2 3 4),(Jogador (15,12) C 2 2 4)] [DisparoCanhao 1 (13,12) C]),
          (Estado (mapaInicial (20,20)) [(Jogador (12,12) C 2 3 4),(Jogador (15,12) C 2 2 4)] [DisparoCanhao 0 (10,10) D, DisparoCanhao 1 (10,10) C]),
          (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
                    Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
                   Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                   [Jogador (4,1) C 2 3 4, Jogador (7,2) D 2 3 4, Jogador (7,9) B 2 5 4, Jogador (4,6) E 1 2 3] 
                   [DisparoCanhao 0 (2,1) C, DisparoLaser 1 (7,3) D, DisparoChoque 2 2, DisparoCanhao 0 (5,5) D, DisparoCanhao 1 (5,5) C, DisparoCanhao 2 (7,5) D, DisparoCanhao 0 (2,5) D, DisparoCanhao 3 (4,2) E]),
          (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
                    Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                   [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
                   Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                   [Jogador (2,4) C 2 3 4, Jogador (7,2) D 2 3 4, Jogador (5,7) B 2 5 4, Jogador (2,8) E 1 2 3]
                   [DisparoLaser 3 (2,7) E, DisparoCanhao 1 (7,4) D, DisparoLaser 2 (6,7) B, DisparoCanhao 1 (9,2) D, DisparoCanhao 2 (9,2) C, DisparoCanhao 3 (9,2) E, DisparoChoque 0 0]),
          (Estado (mapaInicial (20,20)) [Jogador (4,1) D 2 4 5, Jogador (6,6) C 3 4 5, Jogador (15,15) E 5 6 3] [DisparoCanhao 0 (4,2) D, DisparoCanhao 1 (5,6) C, DisparoCanhao 2 (15,12) E]),        
          (Estado (mapaInicial (10,10)) [Jogador (5,5) D 4 5 0] [DisparoLaser 0 (2,1) D, DisparoCanhao 0 (2,3) B, DisparoCanhao 0 (1,3) B]),
          (Estado (mapaInicial (20,20)) [Jogador (8,8) D 4 5 0] [DisparoLaser 0 (2,6) B, DisparoCanhao 0 (5,6) D, DisparoCanhao 0 (5,5) D]),
          (Estado (mapaInicial (20,20)) [Jogador (4,1) D 4 5 5, Jogador (3,10) C 4 5 5, Jogador (5,10) B 4 5 5] [DisparoLaser 0 (4,2) D]),
          (Estado (mapaInicial (20,20)) [Jogador (4,5) D 4 4 4] [DisparoCanhao 0 (4,6) D]),
          (Estado (mapaInicial (20,20)) [Jogador (2,1) D 1 1 1, Jogador (5,6) B 1 1 1, Jogador (10,10) C 4 4 4] [DisparoCanhao 0 (10,9) D, DisparoCanhao 0 (5,7) E]),
          (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (1,1) D 1 1 1] [DisparoCanhao 0 (1,2) D]),
          (Estado (mapaInicial (20,20)) [Jogador (5,5) B 2 3 4] [DisparoCanhao 0 (1,1) D, DisparoCanhao 0 (1,2) E]),
          (Estado (mapaInicial (20,20)) [Jogador (2,1) E 2 3 4] [DisparoCanhao 0 (5,5) D, DisparoCanhao 0 (5,4) E])]
          
-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick e = let estadoChoque = tickChoques e
             estadoCanhao = tickCanhoes estadoChoque
             estadoFinal = tickLasers estadoCanhao
         in  estadoFinal 

--tickChoques (Estado m js ds) . tickCanhoes (Estado m js ds) . tickLasers (Estado m js ds)

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado 
           -> Estado
tickLasers (Estado m js ds) = Estado (ondeMudaBlocos (ondeTemBlocos (blocosAdjacente (ondeTemBlocos (zonaLaser (devolveLaser ds) m) m)) m) m) 
                                     (tiraVidas js (zonaLaser (devolveLaser ds) m)) 
                                     (removeLaser ((modificaCanhoes ds (zonaLaser (devolveLaser ds) m)))) 

                                     
-- | Função que tem como finalidade remover Disparos do tipo Laser da lista de Disparos.                            
removeLaser :: [Disparo] -- ^ Lista de Disparos inicial 
            -> [Disparo] -- ^ Lista de Disparos após removermos os Disparos do tipo Laser.

removeLaser [] = []
removeLaser ((DisparoLaser n (l,c) d):t) = removeLaser t 
removeLaser (h:t) = h : removeLaser t 


-- | Função que tem a finalidade de nos indicar o conjuntos das posições abrangidas pelo Laser disparado.
zonaLaser :: [Disparo] -- ^ Lista de Disparos inicial 
          -> Mapa      -- ^ Mapa onde ocorrem os Disparos
          -> [Posicao] -- ^ Conjunto das posições cobertas pelo Laser, dependendo da direção do Disparo

zonaLaser [] m = []

zonaLaser ((DisparoLaser n (l,c) C):t) m | encontraPosicaoMatriz (l,c) m /= Bloco Indestrutivel = (l-1,c) : zonaLaser ((DisparoLaser n (l-1,c) C):t) m 
                                         | otherwise = zonaLaser t m  

zonaLaser ((DisparoLaser n (l,c) B):t) m | encontraPosicaoMatriz (l,c) m /= Bloco Indestrutivel = (l+1,c) : zonaLaser ((DisparoLaser n (l+1,c) B):t) m 
                                         | otherwise = zonaLaser t m  

zonaLaser ((DisparoLaser n (l,c) D):t) m | encontraPosicaoMatriz (l,c) m /= Bloco Indestrutivel = (l,c+1) : zonaLaser ((DisparoLaser n (l,c+1) D):t) m 
                                         | otherwise = zonaLaser t m  

zonaLaser ((DisparoLaser n (l,c) E):t) m | encontraPosicaoMatriz (l,c) m /= Bloco Indestrutivel = (l,c-1) : zonaLaser ((DisparoLaser n (l,c-1) E):t) m 
                                         | otherwise = zonaLaser t m  

zonaLaser (h:t) m = zonaLaser t m   


-- | Função que dada uma Lista de Disparos devolve a lista com apenas os Disparos do tipo Laser.
devolveLaser :: [Disparo] -- ^ Lista de Disparos inicial
             -> [Disparo] -- ^ Lista de Disparos com apenas disparos do tipo Laser

devolveLaser [] = []
devolveLaser ((DisparoLaser n (l,c) d):t) = (DisparoLaser n (l,c) d) : devolveLaser t 
devolveLaser (h:t) = devolveLaser t 


-- | Função que dada uma lista de posições devolve a lista de posições em que se encontram Blocos Destrutíveis.
ondeTemBlocos :: [Posicao] -- ^ Lista de posições inicial
              -> Mapa      -- ^ Mapa a que nos referimos 
              -> [Posicao] -- ^ Lista das posições onde se encontram Blocos Destrutiveis

ondeTemBlocos [] m = []
ondeTemBlocos ((l,c):t) m | encontraPosicaoMatriz (l,c) m == Bloco Destrutivel = (l,c) : ondeTemBlocos t m
                          | otherwise = ondeTemBlocos t m 


-- | Função que tem como finalidade de através de uma lista de posições nos devolver cada posição e as suas posições adjacentes, á direita e a baixo.
blocosAdjacente :: [Posicao] -- ^ Lista das posições inicial 
                -> [Posicao] -- ^ Lista das posições, mais as suas adjacentes á direira e a baixo

blocosAdjacente [] = []
blocosAdjacente ((l,c):t) = (l,c) : (l+1,c) : (l,c+1)  : blocosAdjacente t  


-- | Função que irá mudar um bloco de uma certa posição para um Bloco Vazio
ondeMudaBlocos :: [Posicao] -- ^ Lista de posições a alterar
               -> Mapa      -- ^ Mapa a que nos referimos
               -> Mapa      -- ^ Mapa alterado 

ondeMudaBlocos [] m = m 
ondeMudaBlocos (h:t) m = atualizaPosicaoMatriz h (Vazia) (ondeMudaBlocos t m)

-- | Função que nos irá dizer se um jogador é ou não afetado de acordo com a sua posição.
jogadorAfetado :: Jogador   -- ^ Jogador a que nos referimos
               -> [Posicao] -- ^ Posições de comparação
               -> Bool

jogadorAfetado (Jogador (l,c) d 0 la ch) p = False
jogadorAfetado j [] = False 
jogadorAfetado (Jogador (l,c) d v la ch) ((l1,c1):t) | (l1 <= l+1) && (l1 >= l-1) && (c1 <= c+1) && (c1 >= c-1) = True
                                                     | otherwise = jogadorAfetado (Jogador (l,c) d v la ch) t


-- | Função que faz com que quando o jogador a que nos referimos seja afetado lhe seja retirada uma vida.
tiraVida :: Jogador   -- ^ Jogador a que nos referimos
         -> [Posicao] -- ^ conjunto das posições de comparação
         -> Jogador   -- ^ Jogador com o nº de vidas atualizado

tiraVida (Jogador (l,c) d v la ch) [] = (Jogador (l,c) d v la ch)
tiraVida (Jogador (l,c) d v la ch) lp | jogadorAfetado (Jogador (l,c) d v la ch) lp = (Jogador (l,c) d (v-1) la ch)
                                      | otherwise = (Jogador (l,c) d v la ch)


-- | Função com a mesma finalidade que a anterior mas que faz com que o mesmo seja executado a uma lista de jogadores e não a apenas um.
tiraVidas :: [Jogador] -- ^ Lista de jogadores
          -> [Posicao] -- ^ Posições de comparação
          -> [Jogador] -- ^ Lista de jogadores com o nº de vidas atualizado

tiraVidas js [] = js
tiraVidas [] _ = [] 
tiraVidas (h:t) lp = tiraVida h lp : tiraVidas t lp 


-- | Função que diz se tem ou não uma bala de Canhão numa determinada posição. 
balaNaPosicao :: Disparo   -- ^ Disparo a que nos referimos
              -> [Posicao] -- ^ Conjunto das posições de comparação
              -> Bool
balaNaPosicao (DisparoCanhao n (l,c) d) [] = False
balaNaPosicao (DisparoCanhao n (l,c) C) (h:t) | (l-1,c) == h || (l+1,c) == h  = True
                                              | otherwise = balaNaPosicao (DisparoCanhao n (l,c) C) t 
balaNaPosicao (DisparoCanhao n (l,c) B) (h:t) | (l-1,c) == h || (l+1,c) == h  = True
                                              | otherwise = balaNaPosicao (DisparoCanhao n (l,c) B) t 
balaNaPosicao (DisparoCanhao n (l,c) D) (h:t) | (l,c-1) == h || (l,c+1) == h  = True
                                              | otherwise = balaNaPosicao (DisparoCanhao n (l,c) D) t 
balaNaPosicao (DisparoCanhao n (l,c) E) (h:t) | (l,c-1) == h || (l,c+1) == h  = True
                                              | otherwise = balaNaPosicao (DisparoCanhao n (l,c) E) t 
balaNaPosicao d l = False


-- | Função que tira todas as Balas do tipo Canhão de uma certa posição.
modificaCanhoes :: [Disparo] -- ^ Lista de Disparos Inicial
                -> [Posicao] -- ^ Posições de comparação 
                -> [Disparo] -- ^ Lista sem os Disparos do tipo Canhão de certas posições 

modificaCanhoes ds [] = ds 
modificaCanhoes [] l = []
modificaCanhoes ((DisparoCanhao n (l,c) d):t) p | balaNaPosicao (DisparoCanhao n (l,c) d) p = modificaCanhoes t p
                                                | otherwise = (DisparoCanhao n (l,c) d) : modificaCanhoes t p
modificaCanhoes (h:t) p = h : modificaCanhoes t p 



-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.

tickCanhoes :: Estado 
            -> Estado
tickCanhoes (Estado m js ds) = Estado (destroiMapa (filtraPosicao (blocosAdjacenteCanhao (filtraPosicao (posicaoCanhao (descobreCanhao ds)) m)) m) m) 
                                      (tiraVidasC ds js) 
                                      (atualizaCanhoes (removeCruzadas2 (removeCruzadas ((removeBalas (removeIguais (balaContraBalas (posicaoCanhao (removeCanhoesBlocoOuPlayer ds js m)))) (removeCanhoesBlocoOuPlayer ds js m)))) ((removeBalas (removeIguais (balaContraBalas (posicaoCanhao (removeCanhoesBlocoOuPlayer ds js m)))) (removeCanhoesBlocoOuPlayer ds js m))))) 


-- | Função que nos indica as posições de um conjunto de Disparos do tipo Canhão, e a posição á direita de cada um.
posicaoCanhao :: [Disparo] -- ^ Lista de Disparos 
              -> [Posicao] -- ^ Lista das posições de cada disparo e á direita de cada um

posicaoCanhao []  = []
posicaoCanhao ((DisparoCanhao n (l,c) C):t) = (l,c) : (l,c+1) : posicaoCanhao t
posicaoCanhao ((DisparoCanhao n (l,c) B):t) = (l,c) : (l,c+1) : posicaoCanhao t
posicaoCanhao ((DisparoCanhao n (l,c) d):t) = (l,c) : posicaoCanhao t
posicaoCanhao (h:t) = posicaoCanhao t


-- | Função que tem como finalidade nos indicar as posições a baixo de determinadas posições e as próprias posições.
blocosAdjacenteCanhao :: [Posicao] -- ^ Lista de posições
                      -> [Posicao] -- ^ Lista de posições incluindo as posições a baixo das iniciais

blocosAdjacenteCanhao [] = []
blocosAdjacenteCanhao ((l,c):t) = (l,c) : (l+1,c) : blocosAdjacenteCanhao t 


-- | Função que devolve o conjunto de Posições, com uma posição inicial ou alguma posição adjacente a uma inicial em que se encontra um Bloco Destrutivel.
filtraPosicao :: [Posicao] -- ^ Conjunto das Posições inciais
              -> Mapa      -- ^ Mapa a que nos referimos
              -> [Posicao] -- ^ Conjunto das Posições inicias ou adjacentes a uma inicial, em que se encontram Blocos Destrutiveis.

filtraPosicao [] m = []
filtraPosicao ((l,c):t) m | encontraPosicaoMatriz (l,c) m == Bloco Destrutivel = (l,c) : filtraPosicao t m
                          | encontraPosicaoMatriz (l-1,c) m == Bloco Destrutivel = (l-1,c) : filtraPosicao t m
                          | encontraPosicaoMatriz (l+1,c) m == Bloco Destrutivel = (l+1,c) : filtraPosicao t m 
                          | encontraPosicaoMatriz (l,c+1) m == Bloco Destrutivel = (l,c+1) : filtraPosicao t m  
                          | encontraPosicaoMatriz (l,c-1) m == Bloco Destrutivel = (l,c-1) : filtraPosicao t m 
                          | otherwise = filtraPosicao t m 


-- | Função com a finalidade de atualizar um mapa, alterando um conjunto de posições para uma Peça Vazia.                           
destroiMapa :: [Posicao] -- ^ Posições a alterar para Peça Vazia
            -> Mapa      -- ^ Mapa Inicial
            -> Mapa      -- ^ Mapa Resultante

destroiMapa [] m = m 
destroiMapa (h:t) m = atualizaPosicaoMatriz h (Vazia) (destroiMapa t m)


-- | Funçao que a partir de uma Lista de Disparos nos devolve a Lista de Disparos com apenas Disparos do tipo Canhão.
descobreCanhao :: [Disparo] -- ^ Lista de Disparos 
               -> [Disparo] -- ^ Lista de Disparos com apenas Disparos do tipo Canhão

descobreCanhao [] = []
descobreCanhao ((DisparoCanhao n p d):t) = (DisparoCanhao n p d) : descobreCanhao t
descobreCanhao (h:t) = descobreCanhao t


-- | Função que nos diz se um bala de Canhão bateu ou não em algum Bloco.
bateuEmBloco :: Disparo -- ^ Disparo a que nos referimos
             -> Mapa    -- ^ Mapa onde o Disparo ocorre
             -> Bool    

bateuEmBloco (DisparoCanhao n (l,c) C) m | encontraPosicaoMatriz (l-1,c) m /= Vazia = True
                                         | encontraPosicaoMatriz (l,c) m /= Vazia = True
                                         | otherwise = False
bateuEmBloco (DisparoCanhao n (l,c) B) m | encontraPosicaoMatriz (l+1,c) m /= Vazia = True
                                         | encontraPosicaoMatriz (l,c) m /= Vazia = True
                                         | otherwise = False
bateuEmBloco (DisparoCanhao n (l,c) D) m | encontraPosicaoMatriz (l,c+1) m /= Vazia = True
                                         | encontraPosicaoMatriz (l,c) m /= Vazia = True
                                         | otherwise = False
bateuEmBloco (DisparoCanhao n (l,c) E) m | encontraPosicaoMatriz (l,c-1) m /= Vazia = True
                                         | encontraPosicaoMatriz (l,c) m /= Vazia = True
                                         | otherwise = False
bateuEmBloco d m = False


-- | Função que nos diz se um Disparo bateu ou não em algum Jogador.
bateuEmPlayer :: Disparo   -- ^ Disparo a que nos referimos
              -> [Jogador] -- ^ Lista de Jogadores
              -> Bool
bateuEmPlayer ds [] = False
bateuEmPlayer ds ((Jogador (l,c) d1 v1 la ch):t) | elem (l,c) (posicaoCanhao [ds])     ||
                                                   elem (l-1,c) (posicaoCanhao [ds])   ||
                                                   elem (l+1,c) (posicaoCanhao [ds])   ||
                                                   elem (l,c+1) (posicaoCanhao [ds])   ||
                                                   elem (l,c-1) (posicaoCanhao [ds])   ||
                                                   elem (l+1,c-1) (posicaoCanhao [ds]) ||
                                                   elem (l+1,c+1) (posicaoCanhao [ds]) ||
                                                   elem (l-1,c+1) (posicaoCanhao [ds]) ||
                                                   elem (l-1,c-1) (posicaoCanhao [ds]) = True
                                                 | otherwise = bateuEmPlayer ds t 


-- | Função que remove de uma Lista de Jogadores, um Jogador de determinado indíce.
removePlayer :: Int       -- ^ Indice do Jogador
             -> [Jogador] -- ^ Lista de Jogadores 
             -> [Jogador] -- ^ Lista de Jogadores, sem o Jogador do indice indicado

removePlayer 0 (h:t) = t 
removePlayer n (h:t) = h : removePlayer (n-1) t


-- | Função que devolve o Jogador de determinado indice, dentro de uma Lista de Jogadores.
devolvePlayer :: Int       -- ^ Indice do Jogador
              -> [Jogador] -- ^ Lista de Jogadores
              -> Jogador   -- ^ Jogador do indice indicado
devolvePlayer 0 (h:t) = h 
devolvePlayer n (h:t) = devolvePlayer (n-1) t


-- | Função que coloca um determinado "a" numa lista de "a's" de acordo com o indice dado.
colocaNaPosicao :: Int -- ^ Indice 
                -> a   -- ^ Elemento do tipo "a"
                -> [a] -- ^ Lista de elementos do tipo "a"
                -> [a] -- ^ Lista de elementos to tipo "a" com o "a" dado inserido

colocaNaPosicao n x [] = [x] 
colocaNaPosicao 0 x (h:t) = (x:h:t)
colocaNaPosicao n x (h:t) = h : colocaNaPosicao (n-1) x t 


-- | Função que indica se um disparo afeta ou não um ou mais Jogadores dentro de uma Lista de Jogadores.
naoAfetaPlayer :: Disparo   -- ^ Disparo a que nos referimos
               -> [Jogador] -- ^ Lista de Jogadores
               -> Bool

naoAfetaPlayer ds [] = False
naoAfetaPlayer (DisparoCanhao n (l,c) d) js = bateuEmPlayer (DisparoCanhao n (l,c) d) (removePlayer n js)
naoAfetaPlayer d js = False 


-- | Função que nos diz se um determinado Disparo atinge um certo Jogador ou não.
jogadorLevouTiro :: Disparo  -- ^ Disparo a que nos referimos
                 -> Jogador  -- ^ Jogador a que nos referimos
                 -> Bool

jogadorLevouTiro (DisparoCanhao n (l1,c1) d) (Jogador (l,c) d1 v1 la1 ch1) | v1 == 0 = False
                                                                           | (l1 <= l+1) && (l1 >= l-1) && (c1 <= c+1) && (c1 >= c-1) = True
                                                                           | otherwise = False 


-- | Função que devolve a Lista de Jogadores atualizada, retirará a vida a um determinado Jogador atingido por um certo Disparo.
tiraVidaCanhao :: Disparo   -- ^ Disparo referente
               -> [Jogador] -- ^ Lista de Jogadores
               -> [Jogador] -- ^ Lista de Jogadores com o nº de vidas atualizado

tiraVidaCanhao ds [] = []
tiraVidaCanhao ds ((Jogador (l,c) d v la ch):t) | jogadorLevouTiro  ds (Jogador (l,c) d v la ch) = (Jogador (l,c) d (v-1) la ch) : tiraVidaCanhao ds t 
                                                | otherwise = (Jogador (l,c) d v la ch) : tiraVidaCanhao ds t 


-- | Tira vida aos jogadores.
tiraVidasC :: [Disparo] -- ^ Lista de Disparos
           -> [Jogador] -- ^ Lista de Jogadores
           -> [Jogador] -- ^ Lista de Jogadores, com as vidas atualizadas

tiraVidasC [] js = js 
tiraVidasC ds [] = []
tiraVidasC ((DisparoCanhao n (l,c) d):t) js = let listJoga = colocaNaPosicao n (devolvePlayer n js) (tiraVidaCanhao (DisparoCanhao n (l,c) d) (removePlayer n js))
                                                  listFinal = tiraVidasC t listJoga
                                              in listFinal 
tiraVidasC (h:t) js = tiraVidasC t js 


-- | Remove da Lista de Disparos, os Disparos do tipo Canhão que bateram em jogadores ou blocos.
removeCanhoesBlocoOuPlayer :: [Disparo] -- ^ Lista de Disparos
                           -> [Jogador] -- ^ Lista de Jogadores
                           -> Mapa      -- ^ Mapa referente
                           -> [Disparo] -- ^ Lista de Disparos atualizada

removeCanhoesBlocoOuPlayer [] js m = []
removeCanhoesBlocoOuPlayer ((DisparoCanhao n (l,c) d):t) js m | bateuEmBloco (DisparoCanhao n (l,c) d) m = removeCanhoesBlocoOuPlayer t js m 
                                                              | naoAfetaPlayer (DisparoCanhao n (l,c) d) js = removeCanhoesBlocoOuPlayer t js m
                                                              | otherwise = (DisparoCanhao n (l,c) d) : removeCanhoesBlocoOuPlayer t js m 

removeCanhoesBlocoOuPlayer (h:t) js m = h : removeCanhoesBlocoOuPlayer t js m


-- | Função que recebe uma Lista de Posições e nos devolve a Lista das Posições que aparecem pelo menos duas vezes na Lista.
balaContraBalas :: [Posicao] -- ^ Conjunto de Posições inicial
                -> [Posicao] -- ^ Conjunto das Posições que aparecem pelo menos duas vezes na Lista Inicial

balaContraBalas [] = []
balaContraBalas [(l,c)] = []
balaContraBalas ((l,c) : (l1,c1) :t) | (l,c) == (l1,c1) = (l,c) :  balaContraBalas t 
                                     | otherwise = balaContraBalas ((l,c): t) ++ balaContraBalas ((l1,c1):t)


-- | Função que através de um conjunto de Posições irá devolver a lista em que todas as Posições são diferentes entre si.
removeIguais :: [Posicao] -- ^ Posições referentes
             -> [Posicao] -- ^ Conjunto das posições em que já não igualdade entre elas

removeIguais [] = [] 
removeIguais (h:t) = h :  removeIguais (escolheIguais h t) 


-- | Função que devolve o conjunto das Posições de uma lista, diferentes de uma posição dada.
escolheIguais :: Posicao   -- ^ Posição a que nos referimos
              -> [Posicao] -- ^ Posições de comparação
              -> [Posicao] -- ^ Posições diferentes da posição dada

escolheIguais n [] = []
escolheIguais n (h:t) | n /= h = h : escolheIguais n t 
                      | otherwise = escolheIguais n t 


-- | Função que irá remover um Disparo do tipo Canhão de uma Lista de Disparos, caso este se encontre na posição dada.
removeBalaPosicao :: Posicao   -- ^ Posição referente
                  -> [Disparo] -- ^ Lista de Disparos 
                  -> [Disparo] -- ^ Lista de Disparos, sem os Disparos do tipo Canhão que se encontravam na posição dada

removeBalaPosicao p [] = []
removeBalaPosicao (l,c) ((DisparoCanhao n (l1,c1) d):t) | (l,c) == (l1,c1) = removeBalaPosicao (l,c) t 
                                                        | otherwise = (DisparoCanhao n (l1,c1) d) : removeBalaPosicao (l,c) t 
removeBalaPosicao (l,c) (h:t) = h : removeBalaPosicao (l,c) t 


-- | Função que irá remover os Disparos que se encontrem em determinadas Posições.
removeBalas :: [Posicao] -- ^ Conjunto das Posições referentes
            -> [Disparo] -- ^ Lista de Disparos
            -> [Disparo] -- ^ Lista de Disparos, sem os Disparos do tipo Canhão que se encontravam nas posições dadas.

removeBalas [] ds = ds
removeBalas p [] = []  
removeBalas (h:t) (h1:t1) = removeBalas t (removeBalaPosicao h (h1:t1))


-- | Função que devolve a lista dos Disparos do tipo Canhão que se cruzaram com uma bala. 
balasCruzaram :: Disparo   -- ^ Disparo referente
              -> [Disparo] -- ^ Lista de Disparos
              -> [Disparo] -- ^ Lista de Disparos que se cruzaram.

balasCruzaram d [] = [] 
balasCruzaram (DisparoCanhao n (l,c) C) ((DisparoCanhao n1 (l1,c1) d):t) | d == B && ((l+1,c) == (l1,c1)) = [(DisparoCanhao n (l,c) C),(DisparoCanhao n1 (l1,c1) d)]
                                                                         | otherwise = balasCruzaram (DisparoCanhao n (l,c) C) t

balasCruzaram (DisparoCanhao n (l,c) B) ((DisparoCanhao n1 (l1,c1) d):t) | d == C && ((l-1,c) == (l1,c1)) = [(DisparoCanhao n (l,c) B),(DisparoCanhao n1 (l1,c1) d)]
                                                                         | otherwise = balasCruzaram (DisparoCanhao n (l,c) B) t

balasCruzaram (DisparoCanhao n (l,c) D) ((DisparoCanhao n1 (l1,c1) d):t) | d == E && ((l,c-1) == (l1,c1)) = [(DisparoCanhao n (l,c) D),(DisparoCanhao n1 (l1,c1) d)]
                                                                         | otherwise = balasCruzaram (DisparoCanhao n (l,c) D) t

balasCruzaram (DisparoCanhao n (l,c) E) ((DisparoCanhao n1 (l1,c1) d):t) | d == D && ((l,c+1) == (l1,c1)) = [(DisparoCanhao n (l,c) E),(DisparoCanhao n1 (l1,c1) d)]
                                                                         | otherwise = balasCruzaram (DisparoCanhao n (l,c) E) t

balasCruzaram (DisparoCanhao n (l,c) d) (h:t) = balasCruzaram (DisparoCanhao n (l,c) d) t 

balasCruzaram ds (h:t) = []


-- | Função que vai devolver todas as balas que se cruzaram.
removeCruzadas :: [Disparo] -- ^ Lista de Disparos
               -> [Disparo] -- ^ Lista de todos os Disparos que se cruzaram entre si

removeCruzadas [] = []
removeCruzadas (h:t) = (balasCruzaram h (h:t)) ++ (removeCruzadas t)


-- | Função que vai remover as balas cruzadas
removeCruzadas2 :: [Disparo] -- ^ Lista de Disparos
                -> [Disparo] -- ^ Lista de Disparos de comparação
                -> [Disparo] -- ^ Lista de Disaros, sem os Disparos que se cruzam

removeCruzadas2 [] ds = ds
removeCruzadas2 d [] = []
removeCruzadas2 (h:t) (h1:t1) | h == h1 = removeCruzadas2 t t1 
                              | otherwise = h1 : removeCruzadas2 (h:t) t1 


-- | Função que irá atualizar a posição de Disparos de tipo Canhão de acordo com a sua direção.
atualizaCanhoes :: [Disparo] -- ^ Lista de Disparos referente
                -> [Disparo] -- ^ Lista de Disparos com as posições atualizadas

atualizaCanhoes [] = []
atualizaCanhoes ((DisparoCanhao n (l,c) d):t) | d == C = (DisparoCanhao n (l-1,c) d) : atualizaCanhoes t 
                                              | d == B = (DisparoCanhao n (l+1,c) d) : atualizaCanhoes t 
                                              | d == D = (DisparoCanhao n (l,c+1) d) : atualizaCanhoes t 
                                              | d == E = (DisparoCanhao n (l,c-1) d) : atualizaCanhoes t 
atualizaCanhoes (h:t) = h : atualizaCanhoes t 


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado 
            -> Estado
tickChoques (Estado m js ds) = Estado m js (atualizaTickChoque ds)


-- | Função que irá atualizar o tempo dos Disparos do tipo Choque.
atualizaTickChoque :: [Disparo] -- ^ Lista de Disparos referente 
                   -> [Disparo] -- ^ Lista de Disparos com o tempo restante de cada Disparo do tipo Choque atualizado

atualizaTickChoque [] = []
atualizaTickChoque ((DisparoChoque n 0):t) = atualizaTickChoque t
atualizaTickChoque ((DisparoChoque n ti):t) = (DisparoChoque n (ti-1)): atualizaTickChoque t
atualizaTickChoque (h:t) = h : atualizaTickChoque t 


