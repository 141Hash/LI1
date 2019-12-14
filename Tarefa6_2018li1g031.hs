{-| 
Module      : Tarefa6_2018li1g031
Description : Implementar um robot
Copyright   : André Martins <a89586@alunos.uminho.pt>
              Tiago Alves   <a89554@alunos.uminho.pt>

= Introdução Tarefa 6:
* Neste módulo, o que nos era proposta era que conseguíssemos programar um bot que fosse capaz de __jogar autonomamente o jogo__ . Nós definimos a estratégia de
primeiro fazer que, sempre que haja possibilidades, o nosso bot __ataque os outros jogadores__, e, quando isso não é possível, ande pelo mapa, __desviando-se__ de
blocos indestrutíveis, e __destruíndo__ os blocos destrutíveis.
Para tal ser possível, tívemos que definir __duas funções auxiliares__ que são importantes para o bot ser funcional.
* A 'descobrePlayer', que nos devolve o jogador ao qual se associou o bot, e a função 'removePlayer2', que iria retirar esse jogador da lista de jogadores, pois,
posteriormente, as outras funções auxiliares comparam diversas vezes as posições do nosso bot com as dos restantes jogadores, e se o nosso jogador 
permanecesse na lista de jogadores, essas funções auxliares não iriam funcionar.

= Objetivos e estratégias de ataque:
* Para estratégia de ataque, delineamos que o nosso bot iria usar o __DisparoLaser__ sempre que tivesse oportunidade. Para tal, definimos a função 'jogadorParaLaser'
que, vai utilizar o trajeto percorrido pelo laser e comparar com os jogadores que se encontram nesse caminho. Foi necessário recorrer à função 'zonaLaser', da
tarefa 4, para o nosso bot apenas utilizar o disparoLaser quando __não existem blocos indestrutíveis no seu caminho__, caso contrário, o bot iria usar o disparoLaser, 
e não iria surtir qualquer efeito.
* Seguidamente, definos as condições em que o nosso bot usa o disparoChoque. O nosso bot utiliza este disparo __sempre que algum jogador entra na área delimitada pelo choque__.
O disparo é efetuado através da função 'usaChoque' que compara a posição do nosso jogador com as posições dos restantes jogadores.

* Depois, definimos uma função para realizar __ataques enquanto o disparoChoque está a ser usado__. Para tal, primeiro tivemos que definir a função 'jáUsouChoque',
que __verifica se o nosso bot está a utilizar um disparoChoque__. Quando essa condição se verifica, o bot utiliza a função 'atacarChoque', que compara a posição do 
nosso bot com a do jogador que está afetado pelo choque . Essa função faz com que o nosso bot se posicione de maneira a conseguir usar o disparoCanhao de forma eficaz.
* Finalmente, o nosso bot utiliza o disparoCanhao quando já não possui munições laser, e sempre que tem um jogador __a menos de 5 posições de distância__.
Definimos as 5 posições de distância, para o nosso bot ter algum espaço de manobra e conseguir atacar de mais longe, mas de forma eficaz, os outros jogadores.

= Objetivos e estratégias de defesa:
* A nossa ideia principal para o bot seria que ele atacasse os inimigos e os procurasse, mas também é necessário definir funções que façam com que o nosso bot
se desvie das balas que estão em jogo. Para tal, definimos a função 'temDeDesviar', que analisa a posição do nosso jogador em relação às balas que estão em
jogo. Quando esta função determina que o nosso jogador está numa situação de perigo, a função 'desviarBalas' faz com que o nosso bot se tente desviar das balas
que o rodeiam.

= Movimentações pelo mapa
* Para o nosso bot realizar movimentações pelo mapa, definimos 3 funções auxiliares que decidem se o bot pode ou não avançar:

1. Função 'abreCaminho', __que decide se o bot tem blocos destrutíveis para destruir à sua frente__.
2. Função 'temParedeFrente' , que vê se o bot tem __blocos indestrutíveis à sua frente__.
3. Função 'estaCaminhoLivre' , que vê se o bot __ se pode movimentar para a posição seguinte__.

* Associada a cada uma das anteriores funções estão __mais três funções auxiliares, que executarão as jogadas indicadas a cada situação__.

1. Função 'destruirParedes', que faz com que o bot __destrua os blocos destruitíveis que estão no seu caminho__.
2. Função 'desviarParedes', que faz com que o bot __ se desvie dos blocos indestrutíveis, rodando no sentido contráio aos ponteiros do relógio__.
3. Função 'moveTanque', que faz com que o bot __avance para a posição seguinte, consoante a sua direção__.

=Conclusão e resultados obtidos:
* Resumindo, o nosso bot consegue atacar inimigos eficazmente __utilizando os 3 disparos que possui__. É capaz de analisar se está numa __situação de perigo__ e tenta
desviar-se. E é também capaz de se mover por __todo o mapa__, tomando a decisão mais apropriada ao tipo de blocos que se encontram à sua frente.
*Para concluir, gostamos de programar este desafio do bot, pois faz-nos ter que pensar em __vários cenários e estratégias de jogo__, e cremos que o
nosso bot consegue ser __eficaz__, de um modo geral, __a jogar autonomamente o jogo__ e a analisar as __diferentes situações__ com que se depara no decorrer de cada jogo.
-}


module Tarefa6_2018li1g031 where

import LI11819
import Tarefa0_2018li1g031
import Tarefa1_2018li1g031
import Tarefa2_2018li1g031
import Tarefa4_2018li1g031
-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n (Estado m js ds) | jogadorParaLaser n (descobrePlayer n js) (removePlayer2 n js) m                       = Just (Dispara Laser)
                       | jaUsouChoque n ds                                                                     = atacarChoque (descobrePlayer n js) (jogadorNoChoque (descobrePlayer n js) (removePlayer2 n js))
                       | usaChoque (descobrePlayer n js) (removePlayer2 n js)                                  = Just (Dispara Choque)
                       | destroiBalas (descobrePlayer n js) ds                                                 = Just (Dispara Canhao)
                       | usaCanhao (descobrePlayer n js) (removePlayer2 n js)                                  = Just (Dispara Canhao) 
                       | estaCaminhoLivre (descobrePlayer n js) m && temDeDesviar (descobrePlayer n js) m ds   = desviarBalas (descobrePlayer n js) m ds
                       | abreCaminho (descobrePlayer n js) m                                                   = destruirParedes (descobrePlayer n js) m  
                       | temParedeFrente (descobrePlayer n js) m                                               = desviarParedes (descobrePlayer n js) m
                       | estaCaminhoLivre (descobrePlayer n js) m                                              = moveTanque (descobrePlayer n js) m

                       
-- * Funções auxiliares 

-- | Função que irá descobrir a que jogador nos estamos a referir dentro de uma lista de jogadores. 
descobrePlayer :: Int       -- ^ Indice do Jogador
               -> [Jogador] -- ^ Lista onde se vai procurar o jogador
               -> Jogador   -- ^ Jogador que se pretende descobrir 

descobrePlayer 0 (h:t) = h 
descobrePlayer n (h:t) = descobrePlayer (n-1) t


-- | Função que irá remover da lista de jogadores o jogador do indice indicado.
removePlayer2 :: Int       -- ^ Indice do Jogador
             -> [Jogador] -- ^ Lista onde se vai procurar o jogador
             -> [Jogador] -- ^ Lista de jogadores sem o jogador com o indice indicado.

removePlayer2 n [] = []
removePlayer2 0 (h:t) = t 
removePlayer2 n (h:t) = h : (removePlayer2 (n-1) t) 


-- | Função Auxiliar com o objetivo de nos indicar se o jogador a que nos referimos tem uma Parede impenetrável no seu caminho, ou seja um Bloco Indestrutivel.
temParedeFrente :: Jogador -- ^ Jogador Selecionado
                -> Mapa    -- ^ Mapa onde o Jogador se encontra
                -> Bool 

temParedeFrente (Jogador (l,c) d va la ch) m | (d == C && encontraPosicaoMatriz (l-1,c) m == Bloco Indestrutivel)   = True
                                             | (d == B && encontraPosicaoMatriz (l+2,c) m == Bloco Indestrutivel)   = True
                                             | (d == D && encontraPosicaoMatriz (l,c+2) m == Bloco Indestrutivel)   = True     
                                             | (d == E && encontraPosicaoMatriz (l,c-1) m == Bloco Indestrutivel)   = True  
                                             | otherwise = False


-- | Função Auxiliar com a finalidade de nos dizer se o caminho que o jogador percorre está livre, ou seja se tem uma peça Vazia á sua frente.                                             
estaCaminhoLivre :: Jogador -- ^ Jogador Selecionado
                 -> Mapa    -- ^ Mapa onde o Jogador se encontra
                 -> Bool

estaCaminhoLivre (Jogador (l,c) d va la ch) m | (d == C && encontraPosicaoMatriz (l-1,c) m == Vazia)   = True
                                              | (d == B && encontraPosicaoMatriz (l+2,c) m == Vazia)   = True  
                                              | (d == D && encontraPosicaoMatriz (l,c+2) m == Vazia)   = True 
                                              | (d == E && encontraPosicaoMatriz (l,c-1) m == Vazia)   = True
                                              | otherwise = False


-- | Função Auxiliar com o intuito de nos indicar se á frente do jogador se encontra uma Parede que possa ser destruída, ou seja um Bloco Destrutivel.
abreCaminho :: Jogador -- ^ Jogador Selecionado
            -> Mapa    -- ^ Mapa onde o Jogador se encontra
            -> Bool

abreCaminho (Jogador (l,c) d v la ch) m | (d == C && encontraPosicaoMatriz (l-1,c) m == Bloco Destrutivel)   = True
                                        | (d == B && encontraPosicaoMatriz (l+2,c) m == Bloco Destrutivel)   = True  
                                        | (d == D && encontraPosicaoMatriz (l,c+2) m == Bloco Destrutivel)   = True
                                        | (d == E && encontraPosicaoMatriz (l,c-1) m == Bloco Destrutivel)   = True
                                        | otherwise = False


-- | Esta função diz-nos que se o caminho estiver livre , o Bot irá avançar dependendo da sua direção.
moveTanque :: Jogador      -- ^ Jogador Selecionado
           -> Mapa         -- ^ Mapa onde o Jogador selecionado se encontra
           -> Maybe Jogada -- ^ Jogada efetuada pelo jogador selecionado

moveTanque (Jogador (l,c) d va la ch) m | (d == C && encontraPosicaoMatriz (l-1,c) m == Vazia)   = Just (Movimenta C)
                                        | (d == B && encontraPosicaoMatriz (l+2,c) m == Vazia)   = Just (Movimenta B)
                                        | (d == D && encontraPosicaoMatriz (l,c+2) m == Vazia)   = Just (Movimenta D)
                                        | (d == E && encontraPosicaoMatriz (l,c-1) m == Vazia)   = Just (Movimenta E)
                                        | otherwise = Nothing


-- | Esta função diz-nos que se no caminho do nosso tank estiver uma Parede Indestrutivel, o nosso Bot terá de se desviar desses blocos.
desviarParedes :: Jogador      -- ^ Jogador Selecionado
               -> Mapa         -- ^ Mapa onde esse Jogador se encontra
               -> Maybe Jogada -- ^ Jogada efetuada por esse Jogador 

desviarParedes (Jogador (l,c) d va la ch) m | (d == C && encontraPosicaoMatriz (l-1,c) m == Bloco Indestrutivel)   = Just (Movimenta E)
                                            | (d == B && encontraPosicaoMatriz (l+2,c) m == Bloco Indestrutivel)   = Just (Movimenta D)
                                            | (d == D && encontraPosicaoMatriz (l,c+2) m == Bloco Indestrutivel)   = Just (Movimenta C)  
                                            | (d == E && encontraPosicaoMatriz (l,c-1) m == Bloco Indestrutivel)   = Just (Movimenta B)
                                            | otherwise = Nothing


-- | Esta função irá fazer com que se uma Parede Destrutivel se encontrar no caminho do nosso Bot , ele a destrua.
destruirParedes :: Jogador      -- ^ Jogador Selecionado
                -> Mapa         -- ^ Mapa onde o Jogador se encontra
                -> Maybe Jogada -- ^ Jogada efetuada pelo Jogador

destruirParedes (Jogador (l,c) d v la ch) m | (d == C && encontraPosicaoMatriz (l-1,c) m == Bloco Destrutivel)   = Just (Dispara Canhao)
                                            | (d == B && encontraPosicaoMatriz (l+2,c) m == Bloco Destrutivel)   = Just (Dispara Canhao)   
                                            | (d == D && encontraPosicaoMatriz (l,c+2) m == Bloco Destrutivel)   = Just (Dispara Canhao)     
                                            | (d == E && encontraPosicaoMatriz (l,c-1) m == Bloco Destrutivel)   = Just (Dispara Canhao)   
                                            | otherwise = Nothing


-- | Esta função diz-nos se o Jogador Selecionado, como sua defesa, tem ou não de se desviar de Disparos do tipo Canhão ou Laser, dependendo das situações.
temDeDesviar :: Jogador   -- ^ Jogador Selecionado
             -> Mapa      -- ^ Mapa onde o Jogador se encontra
             -> [Disparo] -- ^ Lista de Disparos 
             -> Bool

temDeDesviar j m [] = False
temDeDesviar (Jogador (l,c) d v la ch) m ((DisparoCanhao j (x,y) D):t) | elem (x,y) (dangerDir (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) D):t)) = True
                                                                       | otherwise = False 

temDeDesviar (Jogador (l,c) d v la ch) m ((DisparoCanhao j (x,y) E):t) | elem (x,y) (dangerEsq (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) E):t)) = True
                                                                       | otherwise =  False

temDeDesviar (Jogador (l,c) d v la ch) m ((DisparoCanhao j (x,y) C):t) | elem (x,y) (dangerDir (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) C):t)) = True
                                                                       | otherwise = False

temDeDesviar (Jogador (l,c) d v la ch) m ((DisparoCanhao j (x,y) B):t) | elem (x,y) (dangerDir (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) B):t)) = True
                                                                       | otherwise =  False
temDeDesviar j m (h:t) = temDeDesviar j m t 

-- | Esta função faz com que o Jogador se desvie em caso de perigo.
desviarBalas :: Jogador      -- ^ Jogador Selecionado
             -> Mapa         -- ^ Mapa onde o Jogador se encontra
             -> [Disparo]    -- ^ Lista de Disparos 
             -> Maybe Jogada -- ^ Jogada efetuada pelo Jogador Selecionado

desviarBalas j m [] = Nothing
desviarBalas (Jogador (l,c) d v la ch) m ((DisparoCanhao j (x,y) D):t) | elem (x,y) (dangerDir (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) D):t)) = Just (Movimenta C)
                                                                       | otherwise = Nothing 

desviarBalas (Jogador (l,c) d v la ch) m ((DisparoCanhao j (x,y) E):t) | elem (x,y) (dangerEsq (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) E):t)) = Just (Movimenta B)
                                                                       | otherwise =  Nothing

desviarBalas (Jogador (l,c) d v la ch) m ((DisparoCanhao j (x,y) C):t) | elem (x,y) (dangerDir (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) C):t)) = Just (Movimenta D)
                                                                       | otherwise = Nothing

desviarBalas (Jogador (l,c) d v la ch) m ((DisparoCanhao j (x,y) B):t) | elem (x,y) (dangerDir (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) B):t)) = Just (Movimenta E)
                                                                       | otherwise =  Nothing
desviarBalas j m (h:t) = desviarBalas j m t 


{-| Conjunto de funções auxiliares que têm como utilidade nos indicar a lista de posições de perigo para um determinado Jogador,
   de acordo com os Disparos que se encontram na lista, e dependendo do tipo e da direção de cada Disparo. -}

 
-- | Função Auxiliar que nos indica as posições de perigo de Canhão á Direita.
dangerDir :: Jogador   -- ^ Jogador Selecionado
          -> [Disparo] -- ^ Lista de Disparos 
          -> [Posicao] -- ^ Lista de Posições de perigo

dangerDir j [] = []
dangerDir (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) D):t) | (l == x)  = (x,y) : dangerDir (Jogador (l,c) d v la ch) t
                                                                  | otherwise = dangerDir (Jogador (l,c) d v la ch) t
dangerDir j (h:t) = dangerDir j t 


-- | Função Auxiliar que nos indica as posições de perigo de Canhão á Esquerda.
dangerEsq :: Jogador   -- ^ Jogador Selecionado
          -> [Disparo] -- ^ Lista de Disparos 
          -> [Posicao] -- ^ Lista de Posições de perigo

dangerEsq j [] = []
dangerEsq (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) E):t) | (l == x)  = (x,y) : dangerEsq (Jogador (l,c) d v la ch) t 
                                                                  | otherwise = dangerEsq (Jogador (l,c) d v la ch) t
dangerEsq j (h:t) = dangerEsq j t


-- | Função Auxiliar que nos indica as posições de perigo de Canhão a Cima.
dangerCima :: Jogador   -- ^ Jogador Selecionado
           -> [Disparo] -- ^ Lista de Disparos 
           -> [Posicao] -- ^ Lista de Posições de perigo

dangerCima j [] = []
dangerCima (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) C):t) | (c == y)  = (x,y) : dangerCima (Jogador (l,c) d v la ch) t
                                                                   | otherwise = dangerCima (Jogador (l,c) d v la ch) t
dangerCima j (h:t) = dangerCima j t


-- | Função Auxiliar que nos indica as posições de perigo de Canhão a Baixo.
dangerBaixo :: Jogador   -- ^ Jogador Selecionado
            -> [Disparo] -- ^ Lista de Disparos
            -> [Posicao] -- ^ Lista de Posições de perigo

dangerBaixo j [] = []
dangerBaixo (Jogador (l,c) d v la ch) ((DisparoCanhao j (x,y) B):t) | (c == y)  = (x,y) : dangerBaixo (Jogador (l,c) d v la ch) t
                                                                    | otherwise = dangerBaixo (Jogador (l,c) d v la ch) t
dangerBaixo j (h:t) = dangerBaixo j t


-- | Função que dirá se o Bot tem ou não de destruir Balas.
destroiBalas :: Jogador   
             -> [Disparo] 
             -> Bool

destroiBalas j [] = False
destroiBalas (Jogador (l,c) C v la ch) ((DisparoCanhao n (l1,c1) B):t) | c == c1 = True
                                                                       | otherwise = destroiBalas (Jogador (l,c) C v la ch) t 

destroiBalas (Jogador (l,c) B v la ch) ((DisparoCanhao n (l1,c1) C):t) | c == c1 = True
                                                                       | otherwise = destroiBalas (Jogador (l,c) B v la ch) t

destroiBalas (Jogador (l,c) D v la ch) ((DisparoCanhao n (l1,c1) E):t) | l == l1 = True
                                                                       | otherwise = destroiBalas (Jogador (l,c) D v la ch) t 

destroiBalas (Jogador (l,c) E v la ch) ((DisparoCanhao n (l1,c1) D):t) | l == l1 = True
                                                                       | otherwise = destroiBalas (Jogador (l,c) E v la ch) t
destroiBalas j (h:t) = destroiBalas j t  


-- | Função que tem a finalidade de nos transmitir se o inimigo que pretendemos atacar está numa posição em que é favorável utilizarmos o Disparo Laser.
jogadorParaLaser :: Int       -- ^ Indice do Jogador 
                 -> Jogador   -- ^ Jogador Selecionado
                 -> [Jogador] -- ^ Lista de Jogadores no Mapa
                 -> Mapa      -- ^ Mapa onde o Jogador Selecionado se encontra
                 -> Bool

jogadorParaLaser n j [] m = False
jogadorParaLaser n (Jogador (l,c) d v 0 ch) js m  = False

jogadorParaLaser n (Jogador (l,c) C v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) m | c == c1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l-1,c) C] m))   = True
                                                                                   | c == c1-1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l-1,c) C] m)) = True
                                                                                   | c == c1+1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l-1,c) C] m)) = True
                                                                                   | otherwise = jogadorParaLaser n (Jogador (l,c) C v la ch) t m 

jogadorParaLaser n (Jogador (l,c) B v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) m | c == c1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l+1,c) B] m))   = True
                                                                                   | c == c1-1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l+1,c) B] m)) = True
                                                                                   | c == c1+1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l+1,c) B] m)) = True
                                                                                   | otherwise = jogadorParaLaser n (Jogador (l,c) B v la ch) t m 

jogadorParaLaser n (Jogador (l,c) D v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) m | l == l1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l,c+1) D] m))   = True
                                                                                   | l == l1-1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l,c+1) D] m)) = True
                                                                                   | l == l1+1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l,c+1) D] m)) = True
                                                                                   | otherwise = jogadorParaLaser n (Jogador (l,c) D v la ch) t m

jogadorParaLaser n (Jogador (l,c) E v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) m | l == l1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l,c-1) E] m))   = True
                                                                                   | l == l1-1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l,c-1) E] m)) = True
                                                                                   | l == l1+1 && (elem (l1,c1) (zonaLaser [DisparoLaser n (l,c-1) E] m)) = True
                                                                                   | otherwise = jogadorParaLaser n (Jogador (l,c) E v la ch) t m


-- | Esta função tem o objetivo de nos confirmar se o jogador a que nos referimos já utilizou algum Disparo do tipo Choque.
jaUsouChoque :: Int       -- ^ Indice do Jogador 
             -> [Disparo] -- ^ Lista de Disparos
             -> Bool      

jaUsouChoque n [] = False
jaUsouChoque n ((DisparoChoque n1 tick):t) | n == n1 = True
                                           | otherwise = jaUsouChoque n t 
jaUsouChoque n (h:t) = jaUsouChoque n t 


-- | Esta função tem a finalidade de nos dizer se o Jogador a que nos referimos deve utilizar ou não Disparo do tipo Choque. 
usaChoque :: Jogador   -- ^ Jogador Selecionado
          -> [Jogador] -- ^ Lista de Jogadores 
          -> Bool

usaChoque j [] = False
usaChoque j ((Jogador (l,c) d 0 la ch):t) = usaChoque j t 
usaChoque (Jogador (l,c) d v la 0) js = False
usaChoque (Jogador (l,c) d v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) | (l1 <= l+3) && (l1 >= l-3) && (c1 <= c+3) && (c1 >= c-3) = True
                                                                        | otherwise = usaChoque (Jogador (l,c) d v la ch) t 

-- | Função que nos diz que Jogador da lista de Jogadores , se encontra na área do Choque disparado pelo Jogador Selecionado.
jogadorNoChoque :: Jogador   -- ^ Jogador Selecionado
                -> [Jogador] -- ^ Lista de Jogadores 
                -> Jogador   -- ^ Jogador afetado pelo Choque, ou seja que está na área do Choque

jogadorNoChoque (Jogador (l,c) d v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) | (l1 <= l+3) && (l1 >= l-3) && (c1 <= c+3) && (c1 >= c-3) = (Jogador (l1,c1) d1 v1 la1 ch1)
                                                                              | otherwise = jogadorNoChoque (Jogador (l,c) d v la ch) t 

-- | Função que faz com que o nosso Bot, quando algum inimigo se encontrar na área do choque, efetue jogadas do tipo Disparo ou Movimenta dependendo do posicionamento de ambos os jogadores (inimigo e o Bot).
atacarChoque :: Jogador       -- ^ Jogador Selecionado
             -> Jogador       -- ^ Inimigo dentro da área do Choque
             -> Maybe Jogada  -- ^ Jogada a efetuar dependendo do caso

atacarChoque (Jogador (l,c) C v la ch) (Jogador (l1,c1) d1 v1 la1 ch1) | c == c1 && l1 < l   = Just (Dispara Canhao)
                                                                       | c == c1+1 && l1 < l = Just (Dispara Canhao)
                                                                       | c == c1-1 && l1 < l = Just (Dispara Canhao)
                                                                       | c1 > c  && l == l1  = Just (Movimenta D)
                                                                       | c1 > c  && l < l1   = Just (Movimenta B)
                                                                       | c1 > c  && l > l1   = Just (Movimenta C) 
                                                                       | c1 < c  && l == l1  = Just (Movimenta E)
                                                                       | c1 < c  && l < l1   = Just (Movimenta B)
                                                                       | c1 < c  && l > l1   = Just (Movimenta C) 
                                                                       | c == c1 && l < l1   = Just (Movimenta B)

atacarChoque (Jogador (l,c) B v la ch) (Jogador (l1,c1) d1 v1 la1 ch1) | c == c1 && l < l1   = Just (Dispara Canhao)
                                                                       | c == c1+1 && l < l1 = Just (Dispara Canhao)
                                                                       | c == c1-1 && l < l1 = Just (Dispara Canhao)
                                                                       | c == c1 && l1 < l   = Just (Movimenta C)
                                                                       | c1 > c  && l == l1  = Just (Movimenta D)
                                                                       | c1 > c  && l < l1   = Just (Movimenta B)
                                                                       | c1 > c  && l > l1   = Just (Movimenta C) 
                                                                       | c1 < c  && l == l1  = Just (Movimenta E)
                                                                       | c1 < c  && l < l1   = Just (Movimenta B)
                                                                       | c1 < c  && l > l1   = Just (Movimenta C) 


atacarChoque (Jogador (l,c) D v la ch) (Jogador (l1,c1) d1 v1 la1 ch1) | l == l1 && c < c1   = Just (Dispara Canhao)
                                                                       | l == l1+1 && c < c1 = Just (Dispara Canhao)
                                                                       | l == l1-1 && c < c1 = Just (Dispara Canhao)
                                                                       | l == l1 && c1 < c   = Just (Movimenta E)
                                                                       | l1 > l  && c == c1  = Just (Movimenta B)
                                                                       | l1 > l  && c < c1   = Just (Movimenta D)
                                                                       | l1 > l  && c > c1   = Just (Movimenta E)
                                                                       | l1 < l  && c == c1  = Just (Movimenta C)
                                                                       | l1 < l  && c < c1   = Just (Movimenta D)
                                                                       | l1 < l  && c > c1   = Just (Movimenta E)


atacarChoque (Jogador (l,c) E v la ch) (Jogador (l1,c1) d1 v1 la1 ch1) | l == l1 && c1 < c   = Just (Dispara Canhao)
                                                                       | l == l1+1 && c1 < c = Just (Dispara Canhao)
                                                                       | l == l1-1 && c1 < c = Just (Dispara Canhao)
                                                                       | l1 > l  && c == c1  = Just (Movimenta C)
                                                                       | l1 > l  && c < c1   = Just (Movimenta D)
                                                                       | l1 > l  && c > c1   = Just (Movimenta E)
                                                                       | l1 < l  && c == c1  = Just (Movimenta B)
                                                                       | l1 < l  && c < c1   = Just (Movimenta D)
                                                                       | l1 < l  && c > c1   = Just (Movimenta E)
                                                                       | l == l1 && c < c1   = Just (Movimenta D)


-- | Função que nos diz se, de acordo com o posicionamento do Jogador Selecionado e de um inimigo pertencente a uma Lista de Jogadores, é favorável ou não o uso de Disparos do tipo Canhão.
usaCanhao :: Jogador   -- ^ Jogador Selecionado
          -> [Jogador] -- ^ Lista de Jogadores 
          -> Bool

usaCanhao j [] = False
usaCanhao j ((Jogador (l1,c1) d1 0 la1 ch1):t) = usaCanhao j t 
usaCanhao (Jogador (l,c) C v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) | (c1 == c )  && (l1 == l-5) = True
                                                                        | (c1 == c+1) && (l1 == l-5) = True
                                                                        | (c1 == c-1) && (l1 == l-5) = True
                                                                        | (c1 == c )  && (l1 == l-4) = True
                                                                        | (c1 == c+1) && (l1 == l-4) = True
                                                                        | (c1 == c-1) && (l1 == l-4) = True
                                                                        | (c1 == c )  && (l1 == l-3) = True
                                                                        | (c1 == c+1) && (l1 == l-3) = True
                                                                        | (c1 == c-1) && (l1 == l-3) = True
                                                                        | (c1 == c )  && (l1 == l-2) = True
                                                                        | (c1 == c+1) && (l1 == l-2) = True
                                                                        | (c1 == c-1) && (l1 == l-2) = True
                                                                        | otherwise                  = usaCanhao (Jogador (l,c) C v la ch) t 

usaCanhao (Jogador (l,c) B v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) | (c1 == c)   && (l1 == l+5) = True
                                                                        | (c1 == c+1) && (l1 == l+5) = True
                                                                        | (c1 == c-1) && (l1 == l+5) = True
                                                                        | (c1 == c)   && (l1 == l+4) = True
                                                                        | (c1 == c+1) && (l1 == l+4) = True
                                                                        | (c1 == c-1) && (l1 == l+4) = True
                                                                        | (c1 == c)   && (l1 == l+3) = True
                                                                        | (c1 == c+1) && (l1 == l+3) = True
                                                                        | (c1 == c-1) && (l1 == l+3) = True
                                                                        | (c1 == c)   && (l1 == l+2) = True
                                                                        | (c1 == c+1) && (l1 == l+2) = True
                                                                        | (c1 == c-1) && (l1 == l+2) = True
                                                                        | otherwise                  = usaCanhao (Jogador (l,c) B v la ch) t 

usaCanhao (Jogador (l,c) D v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) | (l1 == l)   && (c1 == c+5) = True
                                                                        | (l1 == l+1) && (c1 == c+5) = True
                                                                        | (l1 == l-1) && (c1 == c+5) = True
                                                                        | (l1 == l)   && (c1 == c+4) = True
                                                                        | (l1 == l+1) && (c1 == c+4) = True
                                                                        | (l1 == l-1) && (c1 == c+4) = True
                                                                        | (l1 == l)   && (c1 == c+3) = True
                                                                        | (l1 == l+1) && (c1 == c+3) = True
                                                                        | (l1 == l-1) && (c1 == c+3) = True
                                                                        | (l1 == l)   && (c1 == c+2) = True
                                                                        | (l1 == l+1) && (c1 == c+2) = True
                                                                        | (l1 == l-1) && (c1 == c+2) = True
                                                                        | otherwise                  = usaCanhao (Jogador (l,c) D v la ch) t 

usaCanhao (Jogador (l,c) E v la ch) ((Jogador (l1,c1) d1 v1 la1 ch1):t) | (l1 == l)   && (c1 == c-5) = True
                                                                        | (l1 == l+1) && (c1 == c-5) = True
                                                                        | (l1 == l-1) && (c1 == c-5) = True
                                                                        | (l1 == l)   && (c1 == c-4) = True
                                                                        | (l1 == l+1) && (c1 == c-4) = True
                                                                        | (l1 == l-1) && (c1 == c-4) = True
                                                                        | (l1 == l)   && (c1 == c-3) = True
                                                                        | (l1 == l+1) && (c1 == c-3) = True
                                                                        | (l1 == l-1) && (c1 == c-3) = True
                                                                        | (l1 == l)   && (c1 == c-2) = True
                                                                        | (l1 == l+1) && (c1 == c-2) = True
                                                                        | (l1 == l-1) && (c1 == c-2) = True
                                                                        | otherwise                  = usaCanhao (Jogador (l,c) E v la ch) t 

