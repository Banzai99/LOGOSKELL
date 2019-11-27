module Main where

import Prelude hiding (Left, Right)

-- Type Crayon pour gérer les coordonnées et l'angle
data Crayon = TaillerCrayon Float Float Float deriving(Show)

-- Type Programme qui est juste un nom plus compréhensible pour la liste d'instructions
type Programme = [TypeInstruction]

-- Type TypeInstruction qui permet à la fonction read de convertir la liste donnée en entrée en liste d'instructions.
data TypeInstruction = Forward Float
                     | Repeat Int [TypeInstruction]
                     | Left Float
                     | Right Float
                     deriving (Show, Read)

-- Variables
monCrayon = TaillerCrayon 100.0 100.0 0.0
svgList = "<?xml version=\"1.0\" encoding=\"utf-8\"?><svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"400\" height=\"400\"><title>Exemple</title>"

-- Fonctions
parse chaineDeCaracteres = read (chaineDeCaracteres) :: Programme -- Fonction avec argument "chaineDeCaracteres"

addElemInList x c lst
    | c <= 0 = lst
    | c > 0 = addElemInList x (c-1) (x ++ lst)

fonctionnement [] _ listesvg = listesvg ++ "</svg>" -- Quand il n'y a plus d'instructions à gérer dans la liste, Haskell vient ici automatiquement-

fonctionnement (instruction:resteProgramme) (TaillerCrayon x y angle) listesvg = case instruction of
                (Forward oui) -> (fonctionnement resteProgramme monCrayon nouvellelistesvg)
                    where monCrayon = TaillerCrayon (x + oui*cos(angle*pi/180)) (y + oui*(-sin(angle*pi/180))) (angle)
                          nouvellelistesvg = listesvg ++ "<line x1=\""++(show x)++"\" y1=\""++(show y)++"\" x2=\""++(show (x+oui*cos(angle * pi/180)))++"\" y2=\""++(show(y+oui*(-sin(angle * pi/180))))++"\" stroke=\"red\"/>"
                (Left oui) -> (fonctionnement resteProgramme monCrayon listesvg)
                    where monCrayon = TaillerCrayon x y (angle-oui)
                (Right oui) -> (fonctionnement resteProgramme monCrayon listesvg)
                    where monCrayon = TaillerCrayon x y (angle+oui)
                (Repeat nombreDeFois liste) -> (fonctionnement resteprogrammeavecrepeat monCrayon listesvg)
                    where monCrayon = (TaillerCrayon x y angle)
                          resteprogrammeavecrepeat = (addElemInList liste nombreDeFois resteProgramme)

main :: IO ()
main = do
    putStrLn "Entrez un programme (entre crochets, avec les mots-clés Forward, Left, Right ou repeat [])"
    line <- getLine
    let programme = parse line
    let svg = fonctionnement programme monCrayon svgList
    putStrLn svg
    writeFile "s.svg" svg
