{-| 
Module      : Tarefa3_2018li1g031
Description : Compressão e descompressão do estado de jogo
Copyright   : André Martins <a89586@alunos.uminho.pt>
              Tiago Alves   <a89554@alunos.uminho.pt>

= Introdução Tarefa 3:
* Neste módulo, trabalhamos sobre duas funções principais, a função 'comprime' e a função 'descomprime'.
O objetivo deste módulo era comprimir o estado de jogo e descomprimi-lo, sendo que sempre que se realizava a compressão e ,seguidamente,
descompressão do estado de jogo, teríamos que obter o mesmo estado. Outro objetivo deste módulo era que tentássemos alcançar a maior taxa de compressão possivel,
ou seja, que ao realizar a compressão do estado de jogo, usássemos o menor número possível de carateres,

== Exemplo:
 descomprime (comprime (Estado m js ds)) = Estado m js ds.

* Resumidamente, ao realizar a descompressão e compressão do estado de jogo, não podemos perder informação sobre o estado de jogo. 

= Objetivo e estratégias utilizadas (Compressão):
* O primeiro objetivo foi tentar comprimir o estado usando o menor número de carateres possível, mas esse número teria de ser suficiente para depois,
ao definir a função 'descomprime', fosse possível obter o mesmo estado de jogo que fora comprimido.
* Como, ao realizar a compressão do mapa, cada bloco iria requerer um elevado número de carateres, asssociamos a cada tipo de peça do mapa
um carater, o que reduziu drasticamente o número de carateres que seriam necessários.
No caso dos jogadores e dos disparos, tivemos que recorrer ao uso de carateres para separar o nº de vidas, nº de munições, entre outros, pois, ao realizarmos a
descompressão, iríamos necessitar de carateres que dividissem os diferentes componentes dos disparos e dos jogadores.

== Exemplo:
1. comprimeJogador (Jogador (5,5) C 4 2 4) = "5&5&C&4&2&4&".
2. comprimeJs (Estado [] [(Jogador (5,5) C 4 2 4),(Jogador (10,10) B 2 2 2)] []) = "5&5&C&4&2&4&/10&10&B&2&2&2&"

* O uso do carater __&__ foi essencial para podermos dividir as diferentes componentes do jogador, sendo usada também esta estratégia na compressão dos disparos, 
e do mapa. Sem esse carater, não seria possível separar componentes com mais de dois digítos.
* O uso do carater __/__ também é essencial para dividirmos os diferentes jogadores comprimidos dentro de uma @String@.
* Estas estratégias também foram aplicadas ao mapa e à lista de disparos, e o uso é que permitiu definir e realizar posteriormente uma descompressão eficaz. 

= Objetivo e estratégias utilizadas (Descompressão):
* Ao realizarmos a descompressão, tivemos que utilizar diversas auxiliares que fossem capazes de reconhecer os carateres de separação entre componentes, e entre 
vários jogadores e disparos comprimidos, simultaneamente.
* A estratégia foi, primeiramente, descomprimir apenas uma linha do mapa, um jogador e um disparo, e depois aplicar essas auxiliares recursivamente para
descomprimir as várias linhas, jogadores e disparos comprimidos. Novamente, os carateres de separação desempenharam um papel importante para saber até que ponto
é que a função que apenas descomprime um elemento de uma lista de elementos comprimidos ia atuar.

== Exemplo de descompressão de jogadores:

1. descomprimePlayer "5&5&C&4&2&4&" = Jogador (5,5) C 4 2 4, sendo esta a função base para descomprimir um jogador.

2. descomprimeJs "5&5&C&4&2&4&/10&10&B&2&2&2&" = [(Jogador (5,5) C 4 2 4),(Jogador (10,10) B 2 2 2)], esta função vai utlizar a anterior recursivamente, descomprimindo todos os jogadores.

=== Nota:

* Ao realizar a descompressão de um jogador, definimos a estratégia de usar __uma função auxiliar para descomprimir cada componente__, ou seja, 
uma auxiliar descomprimia a linha onde está o jogador, outra auxiliar descomprimia a coluna, outra auxiliar as vidas, etc...
* Para a função 'descomprimeJs' ser funcional, tivemos que definir a função 'descobreSeparacao', que iria percorrer a lista de jogadores comprimidos e iria
dizer onde terminava a compressão de um jogador e começava a de outro jogador. Esta função utilizava como referência o carater __/__ utilizado na fase de compressão.
* A mesma estratégia foi utilizada na descompressão de mapa e da lista de disparos, e também na descompressão geral de um estado. 

= Conclusão:
* Em suma, cremos que esta tarefa ficou __bem executada__, pois as nossas funções foram capazes de realizar a compressão e descompressão de qualquer estado de jogo,
obtendo uma média de taxa de compressão de __cerca de 90%__. Acreditamos que a taxa de compressão poderia ter sido um pouco mais elevada, mas, no geral,
achamos que esta tarefa __cumpriu os objetivos estipulados.__ -}
module Tarefa3_2018li1g031 where

import LI11819
import Tarefa1_2018li1g031
import Tarefa2_2018li1g031


-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [(Estado (mapaInicial (20,20)) (listaPlayers) [DisparoCanhao 1 (4,4) C ]),
            (Estado (mapaInicial (20,20)) (listaPlayers) [DisparoChoque 3 5, DisparoLaser 0 (6,5) D]),
            (Estado (mapaInicial (10,10)) [Jogador (6,6) B 3 2 1 , Jogador (5,4) C 3 3 1] [DisparoChoque 1 5, DisparoLaser 1 (6,5) D]),
            (Estado (mapaInicial (20,20)) [Jogador (3,3) C 2 3 4] [DisparoChoque 0 5, DisparoCanhao 0 (6,5) D]),
            (Estado (mapaInicial (8,8)) [Jogador (5,5) B 1 2 3] [DisparoChoque 0 2]),
            (Estado (mapaInicial (8,8)) [Jogador (3,5) E 0 2 3] [DisparoChoque 0 8]),
            (Estado (mapaInicial (20,20)) (listaPlayers) [DisparoChoque 3 5, DisparoLaser 0 (6,5) D, DisparoCanhao 5 (5,5) E])]  
 
-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -> String
comprime (Estado m js ds) = comprimeM (Estado m js ds)  ++ "." ++ comprimeJs (Estado m js ds) ++ "." ++ comprimeDs (Estado m js ds)

-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime l = Estado (descomprimeMapa l) (descomprimeJs (drop 1 (dropWhile ( /= '.') l))) (descomprimeDs (drop 1 (dropWhile ( /= '.' ) (tail ((dropWhile ( /= '.') l))))))

-- * Funções Auxiliares para comprimir

-- | Auxiliar que comprime o mapa
comprimeM :: Estado -- ^ Estado que se pretende comprimir
          -> String -- ^ Mapa comprimido
comprimeM (Estado m js ds) = comprimeBlocos m 
-- | Auxiliar que comprime todo o mapa, usando a auxiliar 'comprimeLinha'
comprimeBlocos :: Mapa -- ^ Mapa que se pretende comprimir
               -> String -- ^ Mapa comprimido
comprimeBlocos [] = []
comprimeBlocos (h:t) = comprimeLinha h ++ comprimeBlocos t   
-- | Auxiliar que comprime uma linha de peças, ou seja, que comprime uma linha de um mapa
comprimeLinha :: [Peca]
              -> String
comprimeLinha [] = "|"
comprimeLinha (h:t) | h == Bloco Indestrutivel = 'I' : comprimeLinha t 
                    | h == Bloco Destrutivel = 'D' : comprimeLinha t 
                    | otherwise = 'V' : comprimeLinha t 
-- | Auxiliar que comprime uma lista de disparos
comprimeDs :: Estado -- ^ Estado que se pretende comprimir
           -> String -- ^ Lista de disparos comprimida
comprimeDs (Estado m js []) = []
comprimeDs (Estado m js (h:[])) = comprimeDisparo h ++ []
comprimeDs (Estado m js (h:t)) = (comprimeDisparo h) ++ "#" ++ (comprimeDs (Estado m js t))

-- | Auxiliar que comprime um disparo, dependendo do tipo de disparo recebido
comprimeDisparo :: Disparo -- ^ Disparo que se pretende comprimir
                -> String -- ^ Disparo comprimido
comprimeDisparo (DisparoCanhao n (l,c) C) = "C" ++ "^"  ++ (show n) ++ "^" ++ (show l) ++ "^" ++ (show c) ++ "^" ++ "c"
comprimeDisparo (DisparoCanhao n (l,c) B) = "C" ++ "^"  ++ (show n) ++ "^" ++ (show l) ++ "^" ++ (show c) ++ "^" ++ "b"
comprimeDisparo (DisparoCanhao n (l,c) D) = "C" ++ "^"  ++ (show n) ++ "^" ++ (show l) ++ "^" ++ (show c) ++ "^" ++ "d"
comprimeDisparo (DisparoCanhao n (l,c) E) = "C" ++ "^"  ++ (show n) ++ "^" ++ (show l) ++ "^" ++ (show c) ++ "^" ++ "e"

comprimeDisparo (DisparoLaser n (l,c) C) = "L" ++ "^"  ++ (show n) ++ "^"  ++ (show l) ++ "^" ++ (show c) ++ "^"  ++ "c"
comprimeDisparo (DisparoLaser n (l,c) B) = "L" ++ "^"  ++ (show n) ++ "^"  ++ (show l) ++ "^" ++ (show c) ++ "^"  ++ "b"
comprimeDisparo (DisparoLaser n (l,c) E) = "L" ++ "^"  ++ (show n) ++ "^"  ++ (show l) ++ "^" ++ (show c) ++ "^"  ++ "e"
comprimeDisparo (DisparoLaser n (l,c) D) = "L" ++ "^"  ++ (show n) ++ "^"  ++ (show l) ++ "^" ++ (show c) ++ "^"  ++ "d"

comprimeDisparo (DisparoChoque n ticks) = "X" ++ "^" ++ (show n) ++ "^" ++ (show ticks)                                                    

-- | Auxiliar que comprime uma lista de jogadores
comprimeJs :: Estado -- ^ Estado que se pretende comprimir
           -> String -- ^ Lista de jogadores comprimida
comprimeJs (Estado m [] ds) = []
comprimeJs (Estado m (h:[]) ds) = comprimeJogador h ++ []
comprimeJs (Estado m (h:t) ds) = (comprimeJogador h) ++ "/" ++ (comprimeJs (Estado m t ds)) 

-- | Auxiliar que comprime um jogador, dependendo da direção
comprimeJogador :: Jogador -- ^ Jogador que se pretende comprimir
                -> String -- ^ Jogador comprimido
comprimeJogador (Jogador (l,c) C vida laser choque) = (show l) ++ "&" ++ (show c) ++ "&" ++ "C" ++ "&" ++ (show vida) ++ "&" ++ (show laser) ++ "&" ++ (show choque) ++ "&"
comprimeJogador (Jogador (l,c) B vida laser choque) = (show l) ++ "&" ++ (show c) ++ "&" ++ "B" ++ "&" ++ (show vida) ++ "&" ++ (show laser) ++ "&" ++ (show choque) ++ "&"
comprimeJogador (Jogador (l,c) D vida laser choque) = (show l) ++ "&" ++ (show c) ++ "&" ++ "D" ++ "&" ++ (show vida) ++ "&" ++ (show laser) ++ "&" ++ (show choque) ++ "&"
comprimeJogador (Jogador (l,c) E vida laser choque) = (show l) ++ "&" ++ (show c) ++ "&" ++ "E" ++ "&" ++ (show vida) ++ "&" ++ (show laser) ++ "&" ++ (show choque) ++ "&"

-- * Auxiliares para descompressão                                                  
-- | Auxiliar que servirá para separar o Estado comprimido nos seus diferentes elementos. Como estão separados por '.', esta função irá ser capaz de restringir os diferentes componentes comprimidos
descobreSeparacaoGeral :: String -- ^ Estado comprimido 
                       -> String -- ^ Diferentes componentes do Estado, ou seja, devolve apenas o mapa comprimido, ou a lista de jogadores comprimida ou a lista de disparos comprimida
descobreSeparacaoGeral [] = []
descobreSeparacaoGeral (h:t) | h == '.' = []
                             | otherwise = h : descobreSeparacaoGeral t  


-- | Auxiliar que descomprime uma linha de peças.
descomprimeLinhaMapa :: String -- ^ Linha de mapa comprimida, sendo o '|' o carater que separa as diferentes linhas comprimidas de um mesmo mapa
                     -> [Peca] -- ^ Linha de mapa descomprimida
descomprimeLinhaMapa [] = []
descomprimeLinhaMapa (h:t) | h == 'I' = (Bloco Indestrutivel) : descomprimeLinhaMapa t 
                           | h == 'D' = (Bloco Destrutivel) : descomprimeLinhaMapa t
                           | h == 'V' = (Vazia) : descomprimeLinhaMapa t
                           | h == '|' = []  

-- | Auxiliar que servirá para separar as diferentes linhas do mapa que estão comprimidas, estando separadas por '|'
descobreSeparacaoMapa :: String -- ^ Mapa comprimido 
                      -> String -- ^ Uma linha do mapa comprimido
descobreSeparacaoMapa [] = []
descobreSeparacaoMapa (h:t) | h == '|' = []
                            | otherwise = h : descobreSeparacaoMapa t

-- | Função auxiliar que irá descomprimir o Mapa. Descomprime a primeira linha do mapa, e depois, recursivamente, irá descomprimir as restantes, usando as função 'descobreSeparacaoMapa'
descomprimeMapa :: String -- ^ Mapa comprimido
                -> Mapa -- ^ Mapa descomprimido
descomprimeMapa "" = []
descomprimeMapa (h:t) | h == '|' = []
                      | h == 'D' = descomprimeLinhaMapa (h:t) : (descomprimeMapa (drop ((length (descobreSeparacaoMapa (h:t))) +1) (h:t)))
                      | h == 'I' = descomprimeLinhaMapa (h:t) : (descomprimeMapa (drop ((length (descobreSeparacaoMapa (h:t))) +1) (h:t)))
                      | h == 'V' = descomprimeLinhaMapa (h:t) : (descomprimeMapa (drop ((length (descobreSeparacaoMapa (h:t))) +1) (h:t)))
                      | otherwise = []

-- | Auxiliar para descomprimir jogador. Utilizará as auxiliares 'descompL','descompC','descompDir','descompVida','descompLaser' e 'descompChoque', para descomprimir cada componente do jogador.
 
descomprimePlayer :: String -- ^ Jogador comprimido 
                  -> Jogador -- ^ Jogador descomprimido
descomprimePlayer l = let l1 =  descompL l
                          c1 =  descompC (drop ((length l1) +1) l)
                          d1 =  descompDir (drop ((length c1) +1) (drop ((length l1) +1) l))
                          v1 =  descompVida (drop ((length d1) +1) (drop ((length c1) +1) (drop ((length l1) +1) l)))
                          la1 = descompLaser (drop ((length v1) +1) (drop ((length d1) +1) (drop ((length c1) +1) (drop ((length l1) +1) l))))
                          ch1 = descompChoque (drop ((length la1) +1) (drop ((length v1) +1) (drop ((length d1) +1) (drop ((length c1) +1) (drop ((length l1) +1) l)))))
                          in  Jogador ((read l1), (read c1)) (read d1) (read v1) (read la1) (read ch1) 
 
-- | Auxiliar que devolve uma String só com o primeiro elemento da 'PosicaoGrelha' onde se encontra o jogador
descompL :: String -- ^ Jogador comprimido
         -> String -- ^ Primeiro elemento da 'PosicaoGrelha' onde se encontra o jogador
descompL [] = []
descompL ('&':t) = []
descompL ('^':t) = []
descompL (h:t) = h : descompL t

-- | Auxiliar que devolve uma String só com o segundo elemento da 'PosicaoGrelha' onde se encontra o jogador
descompC :: String -- ^ Jogador comprimido
         -> String -- ^ Segundo elemento da 'PosicaoGrelha' onde se encontra o jogador
descompC [] = []
descompC ('&':t) = []
descompC ('^':t) = []
descompC (h:t) = h : descompC t

-- | Auxiliar que devolve uma String com a 'Direcao' do jogador
descompDir :: String -- ^ Jogador comprimido
           -> String -- ^ 'Direcao' do jogador
descompDir [] = [] 
descompDir (h:t) | h == 'C' || h == 'c' = "C"
                 | h == 'B' || h == 'b' = "B"
                 | h == 'E' || h == 'e' = "E"
                 | h == 'D' || h == 'd' = "D"
                 | h == '&' = [] 

-- | Auxiliar que devolve uma String com o número de vidas do jogador
descompVida :: String -- ^ Jogador comprimido
            -> String -- ^ Número de vidas do jogador
descompVida [] = []
descompVida ('&':t) = []
descompVida (h:t) = h : descompVida t 

-- | Auxiliar que devolve uma String com o número de munições de 'DisparoLaser' que o jogador possui
descompLaser :: String -- ^ Jogador comprimido
             -> String -- ^ Número de munições de 'DisparoLaser'
descompLaser ('&':t) = []
descompLaser (h:t) = h : descompLaser t 

-- | Auxiliar que devolve uma String com o número de munições de 'DisparoChoque' que o jogador possui
descompChoque :: String -- ^ Jogador comprimido
              -> String -- ^ Número de munições de 'DisparoChoque'
descompChoque [] = []
descompChoque ('/':t) = []  
descompChoque ('&':t) = []
descompChoque (h:t) = h : descompChoque t 

-- | Auxiliar que servirá para separar os diferentes jogadores comprimidos, numa lista de jogadores comprimidos
descobreSeparacao :: String -- ^ Jogadores comprimidos
                  -> String -- ^ Apenas um jogador comprimido
descobreSeparacao [] = []
descobreSeparacao (h:t) | h == '/' = []
                        | otherwise = h : descobreSeparacao t


-- | Auxiliar que irá descomprimir uma lista de jogadores, usando a 'descobreSeparacao' como auxiliar para identificar os diferentes jogadores e descomprimi-los, separadamente
descomprimeJs :: String -- ^ Lista de jogadores comprimidos
              -> [Jogador] -- ^ Lista de jogadores descomprimidos
descomprimeJs "" = []
descomprimeJs (h:t) | h == '/' = []
                    | h == '.' = [] ++ (descomprimeJs (drop ((length (descobreSeparacao t)) + 1) t))
                    | otherwise = descomprimePlayer (h:t) : (descomprimeJs (drop ((length (descobreSeparacao (h:t))) + 1) (h:t)))


-- | Auxiliar que irá descomprimir um disparo, devolvendo os diferentes tipo de disparos dependendo do primeiro carater do disparo comprimido
descomprimeDisparo :: String -- ^ Disparo comprimido
                   -> Disparo -- ^ Disparo descomprimido
descomprimeDisparo ('C':'^':t) = let n1 = descompIntOuTicks t
                                     l1 = descompL (drop ((length n1) +1) t)
                                     c1 = descompC (drop ((length l1)+1) (drop ((length n1) +1) t))
                                     d1 = descompDir (drop ((length c1) +1) (drop ((length l1)+1) (drop ((length n1) +1) t)))
                                     in DisparoCanhao (read n1) ((read l1),(read c1)) (read d1)

descomprimeDisparo ('L':'^':t) = let n1 = descompIntOuTicks t
                                     l1 = descompL (drop ((length n1) +1) t)
                                     c1 = descompC (drop ((length l1)+1) (drop ((length n1) +1) t))
                                     d1 = descompDir (drop ((length c1) +1) (drop ((length l1)+1) (drop ((length n1) +1) t)))
                                     in DisparoLaser (read n1) ((read l1),(read c1)) (read d1)

descomprimeDisparo ('X':'^':t) = let n1 = descompIntOuTicks t 
                                     t1 = descompIntOuTicks (drop ((length n1) + 1) t)
                                     in  DisparoChoque (read n1) (read t1)

-- | Auxiliar que devolve os elementos que são do tipo Int
descompIntOuTicks :: String 
                  -> String
descompIntOuTicks [] = []
descompIntOuTicks ('^':t) = []
descompIntOuTicks ('#':t) = []
descompIntOuTicks (h:t) = h : descompIntOuTicks t

-- | Auxiliar que servirá para identificar os diferentes disparos comprimidos, estando eles separados por '#'
descobreSeparacaoDisparo :: String 
                         -> String
descobreSeparacaoDisparo "" = []
descobreSeparacaoDisparo (h:t) | h == '#' = []
                               | otherwise = h : descobreSeparacaoDisparo t
-- | Função auxiliar que descomprime uma lista de disparos, utilizando a auxiliar 'descobreSeparacaoDisparo' para aplicar as auxiliares aos argumentos certos.
descomprimeDs :: String 
              -> [Disparo]
descomprimeDs [] = []
descomprimeDs (h:t) | h == '#' = []
                    | h == '.' = descomprimeDisparo t : (descomprimeDs (drop ((length (descobreSeparacaoDisparo t)) + 1) t))
                    | otherwise = descomprimeDisparo (h:t) : (descomprimeDs (drop ((length (descobreSeparacaoDisparo (h:t))) + 1) (h:t))) 

 









