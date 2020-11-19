-- TRABALHO 1 : HASKELL
-- ALUNO: JOAO GABRIEL FRITSCH
-- "Binary Tree algorithm" :
-- http://weblog.jamisbuck.org/2011/2/1/maze-generation-binary-tree-algorithm#


import Text.Printf
import System.Random
import Data.Char
import Data.List

type Point     = (Float,Float) -- (x,y)
type Cell      = (Point,(Bool,Bool,Bool,Bool)) -- ((x, y), (north, south, west, east))
type Rect      = (Point,Float,Float) -- ((x,y),w,h), onde: w = largura, h = altura
type Color     = (Int,Int,Int) -- tipo RGB


-------------------------------------------------------------------------------
-- Funções:
-------------------------------------------------------------------------------

-- Função para criação da lista de pontos 
bornPoint :: Float -> [Point]
bornPoint w = [(x,y) | x <- [1..w], y <- [1..w]]


-- Função para criação da lista de células 
bornCells :: Float -> [Cell]
bornCells dimension = map (\(x,y) -> ((x,y),(True,True,True,True))) pointlist
    where pointlist = bornPoint dimension


-- Função para criação da lista de retângulos
bornRects :: [Cell] -> [Rect]
bornRects = cellList2RectList


-- Função para criação da lista de cores
bornColors :: [Rect] -> [Color]
bornColors listOfRects = colRects (length listOfRects)


-- Cria parede norte
createNorth :: Float -> Float -> Rect
createNorth x y = ((25*x,25*y),25,3)


-- Cria parede Sul
createSouth :: Float -> Float -> Rect
createSouth x y = ((25*x,(25*y)+25),25,3)


-- Cria parede Oeste
createWest :: Float -> Float -> Rect
createWest x y = ((25*x,25*y),3,25)


--Cria parede Leste
createEast :: Float -> Float -> Rect
createEast x y = (((25*x)+25,25*y),3,25)


-- Funcao que cria uma "parede" dependendo da direção que foi passada.
createRect :: Bool -> Point -> Int -> Rect
createRect False   (_,_) d = ((0,0),0,0)
createRect True    (x,y) 1 = createNorth x y --SE FOR 1, CRIA PAREDE PARA O NORTE
createRect True    (x,y) 2 = createSouth x y  --SE FOR 2, CRIA PAREDE PARA O SUL
createRect True    (x,y) 3 = createWest x y --SE FOR 3, CRIA PAREDE PARA O OESTE
createRect True    (x,y) 4 = createEast x y --SE FOR 4, CRIA PAREDE PARA O LESTE


-- Funcão que recebe uma célula e devolve uma lista de retangulos.
cellToRectList :: Cell -> [Rect]
cellToRectList ((x, y),(north,south,west,east)) = [(createRect north (x,y) 1), (createRect south (x,y) 2), (createRect west (x,y) 3), (createRect east (x,y) 4)]


-- Funcão que recebe uma lista de célula e devolve uma lista de retangulos.
cellList2RectList :: [Cell] -> [Rect]
cellList2RectList list = concatMap (\z -> cellToRectList z) list


-- Funcão que recebe um numero (n) que faz uma lista do tipo cor com o tamanho n. Cores pre definidas.
colRects :: Int -> [Color]
colRects n = [ (x,y,z) | x <- (replicate n 0), y <- (replicate n 0), z <- (replicate n 0)]


-- Funcão para converter uma lista em um numero
fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d


-- Funcão para gera lista de números aleatorios
wayRandom :: StdGen -> Int -> [Int]
wayRandom gen n = take n $ (randomRs (1,2)gen) :: [Int]


-- Função recursiva de construção do caminho
buildMaze :: [Cell] -> [Cell] -> [Int] -> Float -> [Cell]
buildMaze listOfCells mazelist randomlist dim
    | (length listOfCells) == 0 = mazelist
    | otherwise = buildMaze actualizedlistOfCells actualizedmazelist actualizedrandomlist dim
    where actualizedlistOfCells = (tail listOfCells)
          actualizedmazelist = chooseWay (head listOfCells) mazelist randomlist dim 
          actualizedrandomlist = (tail randomlist)


-- Retorna uma célula buscada em uma lista
returnCellofList :: [Cell] -> Cell -> Cell
returnCellofList [] cell = ((0,0),(False,False,False,False))
returnCellofList (x:xs) cell
    | (compareCells x cell) = x
    | otherwise = returnCellofList xs cell 


-- Escolhe caminho (1)
chooseWay :: Cell -> [Cell] -> [Int] -> Float -> [Cell]
chooseWay cell mazelist randomlist dim 
    | ((possibleConections cell dim) == 0) = mazelist
    | ((possibleConections cell dim) == 1) = rebuildMazeList mazelist north_tuple
    | ((possibleConections cell dim) == 2) = rebuildMazeList mazelist west_tuple
    | otherwise = chooseWay2 cell mazelist randomlist dim
    where north_tuple = compareTwo (cell) (returnCellofList (mazelist) (returnNorthCell cell))
          west_tuple = compareTwo (cell) (returnCellofList (mazelist) (returnWestCell cell))


-- Escolhe caminho (2)
chooseWay2 :: Cell -> [Cell] -> [Int] -> Float -> [Cell]
chooseWay2 cell mazelist randomlist dim 
    | ((head randomlist) == 1) = rebuildMazeList mazelist north_tuple
    | otherwise = rebuildMazeList mazelist west_tuple
    where north_tuple = compareTwo (cell) (returnCellofList (mazelist) (returnNorthCell cell))
          west_tuple = compareTwo (cell) (returnCellofList (mazelist) (returnWestCell cell))


-- Atualiza o labirinto com as novas condições de caminhos estabelecidas
rebuildMazeList :: [Cell] -> (Cell, Cell) -> [Cell]
rebuildMazeList mazelist tuple = compare2
    where compare1 = map (\elem -> if (compareCells elem (fst tuple)) then (fst tuple) else elem) mazelist
          compare2 = map (\elem2 -> if (compareCells elem2 (snd tuple)) then (snd tuple) else elem2) compare1


-- Compara duas células e retorna se são iguais (os pontos)
compareCells :: Cell -> Cell -> Bool
compareCells (p1, (_,_,_,_)) (p2, (_,_,_,_)) = (p1 == p2)


-- Procura possiveis conexões dependendo da situação e retorna os caminhos possíveis
possibleConections :: Cell -> Float -> Int
possibleConections ((x,y),(n,s,w,e)) dim
    | ((x == 1) && (y == 1)) = 0 -- NAO VAI PRA NENHUMA
    | (x == 1) = 1 -- VAI PARA NORTE
    | (y == 1) = 2 -- VAI PARA OESTE
    | otherwise = 3 -- VAI PARA NORTE/OESTE


-- Retorna célula norte de uma célula recebida
returnNorthCell :: Cell -> Cell 
returnNorthCell ((x,y),(n,s,e,w)) = ((x,y-1),(n,s,e,w))


-- Retorna célula oeste de uma célula recebida
returnWestCell :: Cell -> Cell 
returnWestCell ((x,y),(n,s,e,w)) = ((x-1,y),(n,s,e,w))


-- Compara duas células e retorna uma tupula atualizando as paredes
compareTwo :: Cell -> Cell -> (Cell, Cell)
compareTwo ((x1,y1),(n1,s1,w1,e1)) ((x2,y2),(n2,s2,w2,e2))
    | ((x1 == x2) && (y2 < y1)) = (((x1,y1),(False,s1,w1,e1)), ((x2,y2),(n2,False,w2,e2)))
    | ((x1 == x2) && (y2 > y1)) = (((x1,y1),(n1,False,w1,e1)), ((x2,y2),(False,s2,w2,e2)))
    | ((x1 < x2) && (y2 == y1)) = (((x1,y1),(n1,s1,w1,False)), ((x2,y2),(n2,s2,False,e2)))
    | otherwise = (((x1,y1),(n1,s1,False,e1)), ((x2,y2),(n2,s2,w2,False)))



-------------------------------------------------------------------------------
-- Funções SVG:
-------------------------------------------------------------------------------

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 


-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"


-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensoes do retângulo e uma string com atributos de estilo
svgRect :: Rect -> Color -> String 
svgRect ((x,y),w,h) color = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style
  where style = svgStyle color
  

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: Color -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b


-- Retorna o svgstrs
fileSvg :: [Rect] -> [Color] -> String
fileSvg listOfRects listOfColors = svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = concat (zipWith svgRect listOfRects listOfColors) 
        (w,h) = (1500,1500) -- width, height da imagem SVG



-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do

    -- OBTENÇÃO DA DIMENSÃO DO LABIRINTO
    gen <- getStdGen
    putStrLn $ show ("----- LABIRINTO -----")
    putStrLn $ show ("Digite a dimensao do labirinto: (max 18 para o tamanho da tela)")
    dimension <- getLine
    let d = fromDigits (map digitToInt dimension)
    let dim = (fromIntegral d) -- DIMENSAO EM FLOAT
    let mazefull = bornCells dim -- LABIRINTO FECHADO
    let randomlist = wayRandom gen (d^2) -- LISTA RANDOMICA DE 1 OU 2 PARA SELECIONAR CAMINHO
    let mazepuzzle = buildMaze mazefull mazefull randomlist dim -- LABIRINTO PRONTO EM FORMATO CELL
    let listOfRects = bornRects mazepuzzle -- TRANSFORMA CELL EM RECTS
    let listOfColors = bornColors listOfRects -- COR DO LABIRINTO

    -- CRIAÇÃO DO ARQUIVO SVG    
    writeFile "maze.svg" $ fileSvg listOfRects listOfColors 