module Helpers where


import Estruturas
import Control.Concurrent
import System.Process

-- PEGAR OS 9 PRIMEIROS NUMEROS DE UMA STRING
primeirosNumeros::String->String
primeirosNumeros num =take 9 num
-- PEGAR OS 3 ULTIMOS NUMEROS DE UMA STRING
ultimasLetras::String->String
ultimasLetras num = drop 11 num
-- PEGAR O 10 E O 11 LETRAS DE UMA STRING
letrasDoMeio::String->String
letrasDoMeio num = take 2 (drop 9 num)
-- VERIFICA SE O BILHETE DE IDENTIDADE +E VALIDA
validarBilhete::String->Bool
validarBilhete num = if((length num)== 14 && (length (primeirosNumeros num))==9 && (length (ultimasLetras num))==3 && (length (letrasDoMeio num))==2 && (isNumero(primeirosNumeros num) && (contExistenteNumNaString (letrasDoMeio num))==0 && isNumero(ultimasLetras num))) then True else False
saindo::IO ()
saindo = espera ("\tSAINDO .")
espera::String->IO ()
espera titul0 = do putStr (titul0);threadDelay 109009;putStr ".";threadDelay 1000090;putStrLn "."
-- VERIFICA AS SENHS DOS USUARIOS
autenticarDados :: String -> String ->Bool
autenticarDados nome senha = if (((nome=="jp") || (nome=="ana" || nome=="obed" || nome=="lela")) && senha=="1012") then True else False
contExistenteNumNaString::String->Int
contExistenteNumNaString ""= 0
contExistenteNumNaString (x:xs) | ((x >='0') && (x<='9')) = 1+contExistenteNumNaString xs
                                | otherwise = 0+contExistenteNumNaString xs
-- VERIFICA SE NUMA STRING CONTEM NUMERO
isNumero:: String -> Bool
isNumero ""= False
isNumero numero | (length numero)==(contExistenteNumNaString numero) = True
                | otherwise= False
-- LIMPA TELA
clear= system "cls"
menorQue10::String->String
menorQue10 numero = if((read numero<10) && (length numero)==1) then "0"++numero else numero
-- CONVERTE A,B,C,D,E,F,G EM MAIUSCULAS
letrasConvert::String->IO String
letrasConvert letra = case letra of
                        "a" -> return "A"
                        "b" -> return "B"
                        "c" -> return "C"
                        "d" -> return "D"
                        "e" -> return "E"
                        "f" -> return "F"
                        "g" -> return "G"
                        _ -> return letra

-- PEGA OS DADOS DE UM CLIENTE E RETORNA UMA STING PARA O CADASTRAMENTO DO CLIENTE
cadastrarCliente::Clientes->NumeroConta->String->String->String->String->String->BilheteIdentidade->Telefone->Saldo->DataCadastro->Clientes
cadastrarCliente [] cod nome sobrenome dia mes ano bi tel sal dataCadastro = [(cod,nome++" "++sobrenome,( dia)++"/"++(mes)++"/"++(ano),bi,(tel),sal,dataCadastro)]
cadastrarCliente lista cod nome sobrenome dia mes ano bi tel sal dataCadastro=lista++[(cod,nome++" "++sobrenome,( dia)++"/"++(mes)++"/"++(ano),bi,(tel),sal,dataCadastro)]

-- ALTERA OS VALORES (SALDO) DE UMA LISTA
inserirValor::Clientes->Int->Float->Clientes
inserirValor ((codigo, nome, dat, bi, tel, salario, dataCadastro):xs) cod valor | (codigo==cod) = (codigo, nome, dat, bi, tel, (salario+(valor)),dataCadastro):(inserirValor xs cod valor)
                                                                                | (codigo/=cod) = (codigo, nome, dat, bi, tel, salario, dataCadastro):(inserirValor xs cod valor)
                                                                                | otherwise = inserirValor xs cod valor
inserirValor [] _ _ = []
-- ALTERA OS VALORES (SALDO) DE UMA LISTA
inserirValorTransferencia::Clientes->Int->Int->Float->Clientes
inserirValorTransferencia ((codigo, nome, dat, bi, tel, salario, dataCadastro):xs) cod iban valor | (codigo==cod) = (codigo, nome, dat, bi, tel, (salario - valor),dataCadastro):(inserirValorTransferencia xs cod iban valor)
                                                                                                  | (iban==codigo) = (codigo, nome, dat, bi, tel, (salario + valor),dataCadastro):(inserirValorTransferencia xs cod iban valor)
                                                                                                  | (codigo/=cod) = (codigo, nome, dat, bi, tel, salario, dataCadastro):(inserirValorTransferencia xs cod iban valor)
                                                                                                  | otherwise = inserirValorTransferencia xs cod iban valor
inserirValorTransferencia [] _ _ _ = []
-- INSERE SENHAS NO VECTOR
insertSenha::Senhas->String->Int->Senhas
insertSenha [] letra codi = [(letra,codi)]
insertSenha  lista letra codi =lista++[(letra,codi)]
-- *******************************************************************************************************************************
-- *******************************************************************************************************************************
-- ******************************************* VERIFICAR EXISTENCIA DE UM DADO ********************************************************
-- VERIFICA SE A SENHA EXISTE
existeSenha::Senhas->String->Int->Bool
existeSenha [] _ _ = False
existeSenha ((letra,cod):xs) lett codigo | ((letra,cod)==(lett,codigo)) = True
                                         | otherwise = existeSenha xs (lett) (codigo)
-- VERIFICA SE O CODIGO EXISTE EM UMA LISTA
existeCodigo::Clientes->Int->Bool
existeCodigo [] codigo = False
existeCodigo ((cod, nome, dat, bi, tel, salario,dataCad):xs) codigo | (show codigo ==  show cod) || (codigo == cod) = True
                                                                    | (length xs == 0) = False
                                                                    | otherwise = existeCodigo xs codigo


-- VERIFICA SE O Telefone EXISTE EM UMA LISTA
existeTelefo::Clientes->Int->Bool
existeTelefo [] codigo = False
existeTelefo ((cod, nome, dat, bi, tel, salario,dataCad):xs) telefo | (show tel ==  show telefo) || (telefo == tel) = True
                                                                    | (length xs == 0) = False
                                                                    | otherwise = existeTelefo xs telefo

-- VERIFICA SE O Telefone EXISTE EM UMA LISTA
existeBilhet::Clientes->String->Bool
existeBilhet [] codigo = False
existeBilhet ((cod, nome, dat, bi, tel, salario,dataCad):xs) bilhet | (show bi ==  show bilhet) || (bilhet == bi) = True
                                                                    | (length xs == 0) = False
                                                                    | otherwise = existeBilhet xs bilhet

-- *******************************************************************************************************************************
-- *******************************************************************************************************************************
-- ******************************************* BUSCAR UM DETERMINADO DADO ********************************************************
-- PEGA O NOME DE UMA CONTA
buscaNome::Clientes->Int->Nome
buscaNome ((codigo, nome, dat, bi, tel, salario,dataCadastro):xs) codi |((show codigo)==(show codi)) = nome::Nome
                                                                       |otherwise = buscaNome xs codi
-- PEGA O CODIGO DE UMA CONTA
buscaCodigo::Clientes->Int->NumeroConta
buscaCodigo ((codigo, nome, dat, bi, tel, salario,dataCadastro):xs) codi |((show codigo)==(show codi)) = codigo::NumeroConta
                                                                         |otherwise = buscaCodigo xs codi
-- PEGA A DATA DE NASCIMENTO DE UMA CONTA
buscaDataNascimento::Clientes->Int->Data
buscaDataNascimento ((codigo, nome, dat, bi, tel, salario,dataCadastro):xs) codi |((show codigo)==(show codi)) = dat::Data
                                                                                 |otherwise = buscaDataNascimento xs codi
-- PEGA A BILHETE DE IDENTIDADE DE UMA CONTA
buscaBi::Clientes->Int->BilheteIdentidade
buscaBi ((codigo, nome, dat, bi, tel, salario,dataCadastro):xs) codi |((show codigo)==(show codi)) = bi::BilheteIdentidade
                                                                     |otherwise = buscaBi xs codi
-- PEGA O NUMERO DE TELEFONE DE UMA CONTA
buscaTel::Clientes->Int->Telefone
buscaTel ((codigo, nome, dat, bi, tel, salario,dataCadastro):xs) codi |((show codigo)==(show codi)) = tel::Telefone
                                                                      |otherwise = buscaTel xs codi
-- PEGA O SALARIO DE UMA CONTA
buscaSalario::Clientes->Int->Saldo
buscaSalario ((codigo, nome, dat, bi, tel, salario,dataCadastro):xs) codi |((show codigo)==(show codi) || (codigo==codi)) = salario::Saldo
                                                                          |otherwise = buscaSalario xs codi
-- PEGA A DATA DE CADASTRO DE UMA CONTA
buscaDataCadastro::Clientes->Int->DataCadastro
buscaDataCadastro ((codigo, nome, dat, bi, tel, salario,dataCadastro):xs) codi | ((show codigo)==(show codi)) = dataCadastro::DataCadastro
                                                                               |otherwise = buscaDataCadastro xs codi

-- CONTADOR DE TODOS CADASTROS FEITO DIARIAMENTE
contCadastroDia::Clientes->String->Int
contCadastroDia [] dataa= 0
contCadastroDia ((codigo, nome, dat, bi, tel, salario, dataCadastro):xs) dataa| (show dataa==show dataCadastro) || (dataa==dataCadastro) = 1+contCadastroDia xs dataa
                                                                              | otherwise = contCadastroDia xs dataa
-- CONTADOR DE TODAS TRANSFERENCIAS FEITA DIARIAMENTE
contTransferenciaDia::Transferencias->String->Int
contTransferenciaDia [] dataa= 0
contTransferenciaDia ((cont,iba,sald,dat,hora):xs) dataa | (dataa==dat) =1+contTransferenciaDia xs dataa
                                                         | otherwise = contTransferenciaDia xs dataa

-- CONTADOR DE TODOS DEPOSITOS FEITO DIARIAMENTE
contDepositosDia::Depositos->String->Int
contDepositosDia [] dataa= 0
contDepositosDia ((cont,sald,dat,hora):xs) dataa | (dataa==dat) =1+contDepositosDia xs dataa
                                                 | otherwise = contDepositosDia xs dataa
-- CONTADOR DE TODOS LEVANTAMENTOS FEITO DIARIAMENTE
contLevantamentoDia::Levantamentos->String->Int
contLevantamentoDia [] dataa= 0
contLevantamentoDia ((cont,sald,dat,hora):xs) dataa | (dataa==dat) =1+contLevantamentoDia xs dataa
                                                    | otherwise = contLevantamentoDia xs dataa

-- ESTRATO DE TODAS TRANSFERENCIAS EM UM MES
estratoTransferencia::Transferencias->Int->String->String
estratoTransferencia [] cod dataa= ""
estratoTransferencia ((cont,iba,sald,dat,hora):xs) cod dataa | ((drop 5 (take 7(dataa))==  (drop 5 (take 7(dat)))) || (drop 6 (take 7(dataa))== drop 6(take 7 (dat)))) && ((show cont==show cod) || (cont==cod)) = " \t Montante: -"++(show sald)++" \t Data: "++dat++" \t Hora: "++hora++" \n"++estratoTransferencia xs cod dataa
                                                             | otherwise = estratoTransferencia xs cod dataa
-- ESTRATO DE TODAS TRANSFERENCIAS
todosEstratoTransferencia::Transferencias->Int->String
todosEstratoTransferencia [] cod= ""
todosEstratoTransferencia ((cont,iba,sald,dat,hora):xs) cod |  ((show cont==show cod) || (cont==cod)) = " \t Montante: -"++(show sald)++" \t Data: "++dat++" \t Hora: "++hora++" \n"++todosEstratoTransferencia xs cod
                                                            | otherwise = todosEstratoTransferencia xs cod 
-- ESTRATO DE TODOS DEPOSITOS EM UM MES
estratoDeposito::Depositos->Int->String->String
estratoDeposito [] cod dataa= ""
estratoDeposito ((cont,sald,dat,hora):xs) cod dataa | ((drop 5 (take 7(dataa))==  (drop 5 (take 7(dat)))) || (drop 6 (take 7(dataa))== drop 6(take 7 (dat)))) && ((show cont==show cod) || (cont==cod))=" \t Montante: +"++(show sald)++" \t Data: "++dat++" \t Hora: "++hora++"  \n"++estratoDeposito xs cod dataa
                                                    | otherwise = estratoDeposito xs cod dataa
-- ESTRATO DE TODOS DEPOSITOS
todosEstratoDeposito::Depositos->Int->String
todosEstratoDeposito [] cod = ""
todosEstratoDeposito ((cont,sald,dat,hora):xs) cod | ((show cont==show cod) || (cont==cod))= " \t Montante: +"++(show sald)++" \t Data: "++dat++" \t Hora: "++hora++"  \n"++todosEstratoDeposito xs cod
                                                   | otherwise = todosEstratoDeposito xs cod
-- ESTRATO DE TODOS LEVANTAMENTOS
estratoLevantamento::Levantamentos->Int->String->String
estratoLevantamento [] cod dataa= ""
estratoLevantamento ((cont,sald,dat,hora):xs) cod dataa | ((drop 5 (take 7(dataa))==  (drop 5 (take 7(dat)))) || (drop 6 (take 7(dataa))== drop 6(take 7 (dat)))) && ((show cont==show cod) || (cont==cod)) = " \t Montante: -"++(show sald)++" \t Data: "++dat++" \t Hora: "++hora++" \n"++estratoLevantamento xs cod dataa
                                                        | otherwise = estratoLevantamento xs cod dataa
todosEstratoLevantamento::Levantamentos->Int->String
todosEstratoLevantamento [] cod = ""
todosEstratoLevantamento ((cont,sald,dat,hora):xs) cod | ((show cont==show cod) || (cont==cod))= " \t Montante: +"++(show sald)++" \t Data: "++dat++" \t Hora: "++hora++"  \n"++todosEstratoLevantamento xs cod
                                                       | otherwise = todosEstratoLevantamento xs cod

-- *******************************************************************************************************************************
-- *******************************************************************************************************************************
-- ******************************************* LISTAR DADOS ********************************************************

-- *****************************LISTA UM CLIENTE **********************************************
listUmCliente::[(NumeroConta, Nome, Data, BilheteIdentidade,Telefone,Saldo,DataCadastro)]->Int->String
listUmCliente [] _ = ""
listUmCliente ((codigo, nome, dat, bi, tel, salario,dataCadastro):xs) cod |((show cod)==(show codigo) || (cod)==(codigo))= "\tConta: "++show(codigo)++"\tNome: "++(upDateList2 nome)++" \tData: "++dat++"\tBI: "++bi++"\tTelefone: "++(show tel)++"\tSaldo Atual: "++(show(salario))
                                                                          | otherwise = listUmCliente xs cod

-- ******************************** LISTA UM CLIENTE RETIRANDO O VAALOR DO LEVANTAMENTO *************************
tirarValorLevantar::[(NumeroConta, Nome, Data, BilheteIdentidade,Telefone,Saldo,DataCadastro)]->Float->Int->String
tirarValorLevantar [] _ _ = ""
tirarValorLevantar ((codigo, nome, dat, bi, tel, salario,dataCadastro):xs) sal cod |((show cod)==(show codigo))= "\tConta: "++show(codigo)++"    Nome: "++(upDateList2 nome)++"    Data: "++dat++"    BI: "++bi++"    Tel: "++(show tel)++"    Saldo atual: "++(show(salario-sal))++"\n"
                                                                                   | otherwise = tirarValorLevantar xs sal cod


-- ****************** LISTA TODOS CADASTROS EXISTENTE NO FICHEIRO -------------------
listLinhasClientes::[(NumeroConta, Nome, Data, BilheteIdentidade,Telefone,Saldo,DataCadastro)]->String
listLinhasClientes []= ""
listLinhasClientes ((codigo, nome, dat, bi, tel, salario, dataCadastro):xs) | (xs/=[] || (length xs)>=0)= show(codigo)++","++( nome)++","++dat++","++bi++","++(show tel)++","++(show(salario))++","++dataCadastro++"|"++listLinhasClientes xs
                                                                            | otherwise = listLinhasClientes xs
-- ****************** LISTA TODOS CADASTROS EXISTENTE NO FICHEIRO -------------------
todosClientes::[(NumeroConta, Nome, Data, BilheteIdentidade,Telefone,Saldo,DataCadastro)]->String
todosClientes []= ""
todosClientes ((codigo, nome, dat, bi, tel, salario, dataCadastro):xs) | (xs/=[] || (length xs)>=0)="\tConta: "++show(codigo)++"   Nome: "++(upDateList2 nome)++"   Data de Nascimento: "++dat++"    BI: "++bi++"    Tel: "++(show tel)++"    Saldo Atual:"++(show(salario))++"    Data de Cadastro: "++dataCadastro++"\n"++todosClientes xs
                                                                       | otherwise = todosClientes xs

-- ****************** LISTA TODOS CADASTROS DO DIA EXISTENTE NO FICHEIRO -------------------  
clientesDiario::Clientes->String->String                                         
clientesDiario [] dataa= ""
clientesDiario ((codigo, nome, dat, bi, tel, salario, dataCadastro):xs)  dataa | (dataa==dataCadastro) ="\tConta: "++show(codigo)++"   Nome: "++(upDateList2 nome)++"   Data de Nascimento: "++dat++"    BI: "++bi++"    Tel: "++(show tel)++"    Saldo Atual:"++(show(salario))++"    Data de Cadastro: "++dataCadastro++"\n"++clientesDiario xs dataa
                                                                               | otherwise = clientesDiario xs dataa

-- ****************** LISTA TODAS SENHAS DO DIA EXISTENTE NO FICHEIRO -------------------
listarSenas::Senhas->String
listarSenas [] = ""
listarSenas ((letra,codigo):xs) | (xs/=[] || (length xs)>=0) = letra++","++(show codigo)++"|"++listarSenas xs
                                |(xs==[] || (length xs)==0) = letra++","++(show codigo)
                                | otherwise = listarSenas xs

-- ****************** LISTA TODAS TRANSFERÊNCIAS DO DIA EXISTENTE NO FICHEIRO -------------------
transferenciasDiaria::Transferencias->String->String
transferenciasDiaria [] _ = ""
transferenciasDiaria ((cont,iba,sald,dat,hora):xs) dataa | (dataa==dat) = "\tConta: "++(show cont)++"\tIBAN: "++(show iba)++"\tValor: "++(show sald)++"\tData: "++dat++"\tHora: "++hora++"\n"++transferenciasDiaria xs dataa
                                                         | otherwise = transferenciasDiaria xs dataa
-- ****************** LISTA TODAS TRANSFERÊNCIAS EXISTENTE NO FICHEIRO -------------------
todasTRansferencias::[(NumeroConta,Iban,Saldo,Data,Hora)]->String
todasTRansferencias [] = ""
todasTRansferencias ((cont,iba,sald,dat,hora):xs) | (xs/=[]) || (length xs>=0) = "\tConta: "++(show cont)++"\tIBAN: "++(show iba)++"\tValor: "++(show sald)++"\tData: "++dat++"\tHora: "++hora++"\n"++todasTRansferencias xs
                                                  | otherwise = todasTRansferencias xs

-- ****************** LISTA TODOS DEPOSITOS DO DIA EXISTENTE NO FICHEIRO -------------------
depositoDiaria::Depositos->String->String
depositoDiaria [] dat = ""
depositoDiaria ((cont,sald,dat,hora):xs) dataa | ((show dataa)==(show dat))= "\tConta: "++(show cont)++"\tValor: "++(show sald)++"\tData: "++dat++"\tHora: "++hora++"\n"++depositoDiaria xs dataa
                                               | otherwise = depositoDiaria xs dataa
-- ****************** LISTA TODOS DEPOSITOS EXISTENTE NO FICHEIRO -------------------
todosDositos::[(NumeroConta,Saldo,Data,Hora)]->String
todosDositos [] = ""
todosDositos ((cont,sald,dat,hora):xs) | (xs/=[]) || (length xs>=0) = "\tConta: "++(show cont)++"\tValor: "++(show sald)++"\tData: "++dat++"\tHora: "++hora++"\n"++todosDositos xs
                                       | otherwise = todosDositos xs
-- ****************** LISTA TODOS LEVANTAMENTOS DO DIA EXISTENTE NO FICHEIRO -------------------
levantamentosDiaria::Levantamentos->String->String
levantamentosDiaria [] _ = ""
levantamentosDiaria ((cont,sald,dat,hora):xs) dataa | (dataa==dat) = "\tConta: "++(show cont)++"\tValor: "++(show sald)++"\tData: "++dat++"\tHora: "++hora++"\n"++levantamentosDiaria xs dataa
                                                    | otherwise = levantamentosDiaria xs dataa
-- ****************** LISTA TODOS LEVANTAMENTOS EXISTENTE NO FICHEIRO -------------------
todosLevantamentos::[(NumeroConta,Saldo,Data,Hora)]->String
todosLevantamentos [] = ""
todosLevantamentos ((cont,sald,dat,hora):xs) | (xs/=[]) || (length xs>=0) = "\tConta: "++(show cont)++"\tValor: "++(show sald)++"\tData: "++dat++"\tHora: "++hora++"\n"++todosLevantamentos xs
                                             | otherwise = todosLevantamentos xs

-- *******************************************************************************************************************************
-- *******************************************************************************************************************************
-- ******************************************* CRIAR VECTOR DE UM DETERMINADO TIPO ********************************************************

-- ****************** CRIAR VECTOR DE CLIENTES-------------------
clientes::[[String]]->Clientes
clientes [] = []
clientes ([cod,nome,dat,bi,tel,sal,datcad]:xs) = let codigo = (read cod)::NumeroConta
                                                     nom    = (nome)::Nome
                                                     dat1   =(dat)::Data
                                                     bi1    =(bi)::BilheteIdentidade
                                                     tel1   =(read tel)::Telefone
                                                     sal1   =(read sal)::Saldo
                                                     dataCadastro = (datcad)::DataCadastro
                                                  in (codigo,nom,dat1,bi1,tel1,sal1,dataCadastro):(clientes xs)
-- ****************** CRIAR VECTOR DE SENHAS-------------------
senhas :: [[String]] ->Senhas
senhas [] = []
senhas ([letra,cod]:xs) = let letraSenha = (letra)::LetraSenha
                              numeroSenha = (read cod)::NumeroSenha
                          in (letraSenha,numeroSenha):(senhas xs)

-- ****************** CRIAR VECTOR DE TRANSFERÊNCIAS -------------------
tranferencias ::[[String]]->Transferencias
tranferencias [] = [] 
tranferencias ([cont,iba,sald,dat,hora]:xs) = let numeroConta = (read cont)::NumeroConta
                                                  iban = (read iba)::Iban
                                                  saldo = (read sald)::Saldo
                                                  dataa = (dat)::Data
                                                  horas = (hora)::Hora
                                              in (numeroConta,iban,saldo,dataa,horas):(tranferencias xs)
-- ****************** CRIAR VECTOR DE DEPOSITOS-------------------
depositos ::[[String]]->Depositos
depositos [] = [] 
depositos ([conta,sald,dat,hora]:xs) = let numeroConta = (read conta)::NumeroConta
                                           saldo = (read sald)::Saldo
                                           dataa = (dat)::Data
                                           horas = (hora)::Hora
                                        in (numeroConta,saldo,dataa,horas):(depositos xs)
-- ****************** CRIAR VECTOR DE LEVANTAMENTOS-------------------
levantamentos ::[[String]]->Levantamentos
levantamentos [] = [] 
levantamentos ([conta,sald,dat,hora]:xs) = let numeroConta = (read conta)::NumeroConta
                                               saldo = (read sald)::Saldo
                                               dataa = (dat)::Data
                                               horas = (hora)::Hora
                                            in (numeroConta,saldo,dataa,horas):(levantamentos xs)
-- *******************************************************************************************************************************
-- *******************************************************************************************************************************
-- ******************************************* FUÇOÕES PARA ATUALIZAR AS LISTAS ********************************************************

-- REMOVE ALGUNS CARACTERES NUMA STRING FOI USADA PRA PEGAR OS DADOS DOS FICHEIROS
upDateList::String->String
upDateList [] = ""
upDateList (x:xs) |(x==',')=['\t']++upDateList xs
                  |(x=='|')=['\n']++upDateList xs
                  |(x==' ')=['_']++upDateList xs
                  |(x=='\n')=upDateList xs
                  |otherwise =[x]++upDateList xs
-- REMOVE ALGUNS CARACTERES NUMA STRING FOI USADA PRA INSERIR OS DADOS DOS FICHEIROS
upDateList2::String->String
upDateList2 [] = ""
upDateList2 (x:xs) |(x=='[' || x==']' || x=='(' || x==')' )=upDateList2 xs
                   | (x=='_') = [' ']++upDateList2 xs
                   |otherwise =[x]++upDateList2 xs 
validarDiaMesAno::String->Int->Bool
validarDiaMesAno numero cod = if(isNumero numero) then
                                  if((read numero)>0 && (read numero)<13 && cod==2) then True 
                                       else if((read numero)>0 && (read numero)<32 && cod==1) then True 
                                  else if((read numero)>1980 && (read numero)<2021 && cod==3 && (2021-(read numero)>17)) then True else False
                               else False
-- FUNÇÃO QUE PEDE O DIA
getDia::IO String
getDia=do putStr "\tInsira o Dia\n\t==> "
          dia<- getLine
          if(validarDiaMesAno dia 1) 
             then do
                    return (dia)
             else do
                    putStr " \tDIA INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - Sim\t2 - Não\n\t==> "
                    opcao<-getLine
                    if(opcao=="1") then do  getDia else return "0"
-- FUNÇÃO QUE PEDE O MES
getMes::IO String
getMes=do putStr "\tInsira o mes\n\t==> "
          mes<- getLine
          if validarDiaMesAno mes 2
             then do
                    return mes 
             else do
                    putStr " \tMÊS INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - Sim\t2 - Não\n\t==> "
                    opcao<-getLine
                    if(opcao=="1") then do  getMes else return "0"
-- FUNÇÃO QUE PEDE O BILHETE DE IDENTIDADE
getBilhete::Clientes->IO String
getBilhete lista = do putStr "\tInsira o número do Bilhete de Identidade\n\t==> "
                      bi<- getLine
                      if(validarBilhete bi) && (not(existeBilhet lista bi)) then return bi
                         else do
                                putStr "\tNÚMERO DE BILHETE INVALIDO OU JÁ EXISTE!\n\tDESEJA LER NOVAMENTE?\n\t1 - Sim\t2 - Não\n\t==> "
                                opcao<-getLine
                                if(opcao=="1") then do getBilhete lista else return "0"
-- FUNÇÃO QUE PEDE O NUMERO DE TELEFONE
getTelefone::Clientes->IO String
getTelefone lista = do putStr "\tInsira o número de telefone\n\t==> "
                       telefone<- getLine
                       if((length telefone)==9 && (isNumero telefone) && not(existeTelefo lista (read telefone)) ) then return telefone
                          else do
                                  putStr "\tNÚMERO DE TELEFONE INVALIDO Ou JÁ EXISTENTE!\n\tDESEJA LER NOVAMENTE?\n\t1 - Sim\t2 - Não\n\t==> "
                                  opcao<-getLine
                                  if(opcao=="1") then getTelefone lista else return "0"
-- FUNÇÃO QUE PEDE O ANO
getAno::IO String
getAno=do putStr "\tInsira o ano\n\t==> "
          ano<- getLine
          if(validarDiaMesAno ano 3) 
             then return ano
             else do
                     putStr " \tANO INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - Sim\t2 - Não\n\t==> "
                     opcao<-getLine
                     if(opcao=="1") then do  getAno else return "0"
-- FUNÇÃO QUE PEDE O SOBRE NOME
getSobrenome::IO String
getSobrenome =do putStr "\tInsira o Sobrenome\n\t==> "
                 sobrenome<- getLine
                 if((length sobrenome)>1 && (contExistenteNumNaString sobrenome)==0) then return sobrenome
                    else do
                           putStr "\tSOBRENOME INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - Sim\t2 - Não\n\t==> "
                           opcao<-getLine
                           if(opcao=="1") then getSobrenome else return "0"
-- FUNÇÃO QUE PEDE O NOME
getNome::IO String
getNome =do putStr "\tInsira o nome\n\t==> "
            nome<- getLine
            if((length nome)>1 && (contExistenteNumNaString nome)==0) then return nome
               else do
                       putStr "\tNOME INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - Sim\t2 - Não\n\t==> "
                       opcao<-getLine
                       if(opcao=="1") then getNome else return "0"