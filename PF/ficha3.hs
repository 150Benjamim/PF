

--exc1

data Hora = H Int Int deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]


--testa se uma etapa está bem construída
etapaPossivel :: Etapa -> Bool
etapaPossivel (h1,h2) = if (testHoras h1 == True) && (testHoras h2 == True) && (horaAnt (h1,h2) == True) then True else False

--testa se hora é anterior
horaAnt :: Etapa -> Bool
horaAnt (H h1 m1, H h2 m2) | h1 < h2 = True
                           | h1 > h2 = False
                           | h1 == h2 = if (m1 < m2) then True else False

--testa se são horas reais
testHoras :: Hora -> Bool
testHoras (H h m) | (h >= 0 && h < 24) && (m >= 0 && m < 60) = True
                  | otherwise = False


--testa se uma viagem está bem construída
viagemPossivel :: Viagem -> Bool
viagemPossivel []  = True
viagemPossivel [h] = etapaPossivel h
viagemPossivel (h1:h2:t) | etapaPossivel h1 == True && etapaPossivel (snd h1, fst h2) == True = viagemPossivel (h2:t)
                         | otherwise = False


--calcula hora de partida e chegada de uma viagem
horPartCheg :: Viagem -> Etapa
horPartCheg l = (fst (head l), snd (last l))

--tempo viagem efetiva
tempViagem :: Viagem -> Int
tempViagem [] = 0
tempViagem v  = sum (map (tempEtapa) v) 

--tempo etapa
tempEtapa :: Etapa -> Int
tempEtapa (H h1 m1, H h2 m2) = (h2*60 + m2) - (h1*60 + m1)


--calcula tempo de espera
tempEspera :: Viagem -> Int
tempEspera [] = 0
tempEspera v  = tempEtapa (horPartCheg v) - tempViagem v

--tempo total viagem
tempTotal :: Viagem -> Int
tempTotal [] = 0
tempTotal v  = tempViagem v + tempEspera v



--exc 3

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]


acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n c [] = [(n,[Email c])]
acrescEmail n c a  = ((n, [Email c]):a)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((n,contato):a) | nome == n = Just (mailsContacto contato)
                               | otherwise = verEmails nome a

mailsContacto :: [Contacto] -> [String]
mailsContacto [] = []
mailsContacto (h:t) = case h of Email x -> x : mailsContacto t
                                otherwise -> mailsContacto t

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (h:t) = case h of Casa x -> x : consTelefs t
                             Trab x -> x : consTelefs t
                             Tlm x  -> x : consTelefs t
                             otherwise -> consTelefs t

casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome ((n,contato):t) | nome == n = telCasa contato
                          | otherwise = casa nome t

telCasa :: [Contacto] -> Maybe Integer
telCasa [] = Nothing
telCasa (h:t) = case h of Casa x -> Just x
                          otherwise -> telCasa t



--exc4

type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome,Data)]

procura :: Nome -> TabDN -> Maybe Data
procura nome [] = Nothing
procura nome ((n,dat):t) | nome == n = Just dat
                         | otherwise = procura nome t

idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade (D d1 m1 a1) nome ((n,D d2 m2 a2):t) | nome == n = Just (div ( (d1+m1*31+a1*365) - (d2+m2*31+a2*365) ) 365)

anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2) | a1 < a2 = True
                                   | a1 == a2 && m1 < m2 = True
                                   | a1 == a2 && m1 == m2 && d1 < d2 = True
                                   | otherwise = False

ordena :: TabDN -> TabDN
ordena [] = []
ordena (h:t) = insere h (ordena t)

insere :: (Nome,Data) -> TabDN -> TabDN
insere a [] = [a]
insere a@(n1,dat1) (b@(n2,dat2):t) | anterior dat1 dat2 == True = a : b : t
                                   | otherwise = b : insere a t

porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade dat1 tab = map (porIdadeAux dat1) (reverse (ordena tab))

porIdadeAux :: Data -> (Nome,Data) -> (Nome,Int)
porIdadeAux (D d1 m1 a1) (n,D d2 m2 a2) = (n, (div ( (d1+m1*31+a1*365) - (d2+m2*31+a2*365) ) 365))



--exc5

data Movimento = Credito Float | Debito Float deriving Show

--data Data = D Int Int Int deriving Show

data Extracto = Ext Float [(Data, String, Movimento)] deriving Show


extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext inicial (a@(dat,desc,Credito x):t)) min | x > min   = Credito x : extValor (Ext inicial t) min
                                                      | otherwise = extValor (Ext inicial t) min
extValor (Ext inicial (a@(dat,desc,Debito x):t)) min  | x > min   = Debito x : extValor (Ext inicial t) min
                                                      | otherwise = extValor (Ext inicial t) min


filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro ( Ext inicial ((dat,desc,mov):t) ) l | elem desc l = (dat,mov) : filtro (Ext inicial t) l
                                            | otherwise   = filtro (Ext inicial t) l


creDeb :: Extracto -> (Float,Float)
creDeb (Ext inicial []) = (0,0)
creDeb (Ext inicial [(_,_,mov)]) = case mov of Credito x -> (1,0)
                                               Debito x -> (0,1)
creDeb (Ext inicial (h:t)) = creDebAux (creDeb (Ext inicial [h])) (creDeb (Ext inicial t))

creDebAux :: (Float,Float) -> (Float,Float) -> (Float,Float)
creDebAux (x1,y1) (x2,y2) = (x1+x2,y1+y2)


saldo :: Extracto -> Float
saldo (Ext inicial []) = inicial
saldo (Ext inicial [(_,_,mov)]) = case mov of Debito x -> inicial + x
                                              Credito x -> inicial - x
saldo (Ext inicial (h:t)) = saldo (Ext 0 [h]) + saldo (Ext inicial t)
