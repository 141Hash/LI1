{-| 
Module      : Tarefa5_2018li1g031
Description : Implementação do jogo em Gloss
Copyright   : André Martins <a89586@alunos.uminho.pt>
              Tiago Alves   <a89554@alunos.uminho.pt>

= Introdução Tarefa 5:
* O objetivo desta tarefa era utilizarmos todas as outras tarefas que definimos e animar-mos o nosso jogo em Gloss, ou seja, o objetivo era tornar o nosso jogo executável.
Infelizmente, não conseguimos desenvolver muito esta tarefa e apenas conseguimos desenhar um mapa e um jogador. A estratégia era desenvolver primeiro o mapa e, depois,
desenhar os jogadores e os disparos e associar teclas a cada jogada, mas encontramos dificuldades em trabalhar em Gloss e não conseguimos desenvolver muito esta
tarefa.

= Objetivo geral para a tarefa 5:
O primeiro obejtivo era desenhar o mapa onde iria decorrer o jogo, e para isso definimos 2 funções :

1. 'desenhaLinha' , que devolve a __lista de Pictures associada a uma linha do mapa__.
2. 'criaMapa', que devolve a __lista completa__ de Pictures associada a um mapa.

* Em ambas as funções atribuímos valores do tipo Float como argumento, para depois ser possível realizar a translação do mapa para as posições corretas.
De salientar que cada bloco assume a dimensão de 30x30 pixeis, e tivemos que ter em atenção esses valores para realizar as translações e para desenhar todas
as linhas do mapa separadamente.

* Para desenhar o jogado, apenas definimos a função 'desenhaPlayer', que faz a translação da imagem associada a um jogador, de acordo com a posição do jogador.

= Reação a eventos

* Na parte de alteração do Estado Gloss ao receber um evento, definimos que o jogador andaria 5 pixeis sempre que recebesse um evento.

= Outras componentes da tarefa:
*Infelizmente, não conseguimos desenvolver as outras componentes da tarefa nem reutilizar de forma eficaz todas as outras tarefas que definimos.

= Conclusão

* Esta tarefa trouxe-nos muitas dificuldades e foi a mais difícil de definir de todas as tarefas que nos foram dadas. Não conseguimos trabalhar muito bem com a 
biblioteca Gloss, e, por isso, não conseguimos desenvolver muito bem esta parte do trabalho.
Cremos que estas dificuldades se deveram a ser uma biblioteca com conceitos e estilos aos quais não estávamos habituados a trabalhar e que nos pôs muito confusos.
Gostávamos de ter apresentado esta tarefa mais desenvolvida e com mais funções definidas, mas tal não foi possível.

-}


module Main where

import LI11819
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
import Tarefa1_2018li1g031
import Tarefa2_2018li1g031
import Tarefa3_2018li1g031
import Tarefa4_2018li1g031



-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
type EstadoGloss = (Mapa,[Jogador],[Disparo])

-- | Estado Gloss definido
estadoGlossInicial :: EstadoGloss
estadoGlossInicial = ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
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
                        Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]],
                        [Jogador (5,6) B 4 4 4],[])

-- | Picture associada a bloco indestrutível
bi :: Picture
bi = Color orange (Polygon [(0,0),(20,0),(20,20),(0,20),(0,0)])

-- | Picture associada a bloco destrutível
bd :: Picture
bd = Color blue (Polygon [(0,0),(20,0),(20,20),(0,20),(0,0)]) 

-- | Picture associada à peça Vazia
v :: Picture
v = Color white (Polygon [(0,0),(30,0),(30,30),(0,30),(0,0)])

-- | Picture associada a jogador
player :: Picture 
player = Color yellow (Polygon [(0,0),(60,0),(60,60),(0,60),(0,0)])

-- | Função auxiliar que irá dar a lista de pictures associadas ao mapa
criaMapa :: EstadoGloss -> Float -> Float -> Picture -> Picture -> Picture -> [Picture]
criaMapa ([],js,ds) x y bd bi v = []
criaMapa ((h:t),js,ds) x y bd bi v = (desenhaLinha h x y bd bi v) ++ (criaMapa (t,js,ds) x (y-30) bd bi v)  

-- | Função auxiliar que irá dar a lista de pictures associadas a uma linha do mapa 
desenhaLinha :: [Peca] -> Float -> Float -> Picture -> Picture -> Picture -> [Picture]
desenhaLinha [] x y  bd bi v = []
desenhaLinha (h:t) x y  bd bi v  | h == Bloco Indestrutivel = (Translate x y bi) : desenhaLinha t (x+30) y bd bi v
                                 | h == Bloco Destrutivel =  (Translate x y bd) : desenhaLinha t (x+30) y bd bi v
                                 | otherwise = (Translate x y v) : desenhaLinha t (x+30) y bd bi v
 
-- | Auxliar que desenha o Estado Gloss
desenhaEstadoGloss :: Picture -> Picture -> Picture -> Picture -> EstadoGloss -> Picture
desenhaEstadoGloss bi bd v player (m,js,ds) = Pictures ((criaMapa (m,js,ds) (-300.0) (350.0) bd bi v) ++ (desenhaPlayer js player))    
-- | Desenha jogador
desenhaPlayer :: [Jogador] -> Picture -> [Picture]
desenhaPlayer [] p = []
desenhaPlayer [(Jogador (x,y) d v la ch)] player = [Translate ((fromIntegral x)-150.0) ((fromIntegral y)+250.0) player]

-- | Auxiliar que devolve o Estado Gloss após acontecer algum evento
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss (EventKey (SpecialKey KeyUp)       Down _ _) (m,[Jogador (x,y) d v la ch], ds) = (m, [Jogador (x,y+5) d v la ch] , ds)
reageEventoGloss (EventKey (SpecialKey KeyDown)     Down _ _) (m,[Jogador (x,y) d v la ch], ds) = (m, [Jogador (x,y-5) d v la ch] , ds)  
reageEventoGloss (EventKey (SpecialKey KeyLeft)     Down _ _) (m,[Jogador (x,y) d v la ch], ds) = (m, [Jogador (x-5,y) d v la ch] , ds) 
reageEventoGloss (EventKey (SpecialKey KeyRight)    Down _ _) (m,[Jogador (x,y) d v la ch], ds) = (m, [Jogador (x+5,y) d v la ch] , ds) 
reageEventoGloss s e = e 

-- | Auxilair que devolve o Estado Gloss após a passagem de cada tick.
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss n e = e

-- | Frame rate
fr :: Int
fr = 50

-- | Janela onde irá aparecer o jogo
dm :: Display
dm = InWindow "Tankz" (600, 600) (0, 0)

-- | Função principal da Tarefa 5 
main :: IO ()
main = do
    Just (bi) <- loadJuicy "Bloco.jpg"
    Just (bd) <- loadJuicy "BlocoD.jpg"
    Just (v)  <- loadJuicy "Vazio.jpg"
    Just (player) <- loadJuicy "Tank.png"
    play dm                                 -- janela onde irá correr o jogo
        (greyN 0.5)                         -- côr do fundo da janela
        fr                                  -- frame rate
        estadoGlossInicial                  -- estado inicial
        (desenhaEstadoGloss bi bd v player) -- desenha o estado do jogo
        reageEventoGloss                    -- reage a um evento
        reageTempoGloss                     -- reage ao passar do tempo
