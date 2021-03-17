module Transferencia where


import Helpers
import Arquivo
import Estruturas
import Data.Time


transferencia :: IO ()
transferencia = do getChar; clear; listCli<- lerClientes;
                   putStr "\n****************************** TRANSFERÊNCIA ******************************\n\n\tInforme o seu número de conta\n\t==> "
                   conta <- getLine
                   putStr "\tInforme o número de conta que deseja tranferir\n\t==> "
                   iban <- getLine
                   valor <- getValorTranferir
                   horasSistema <- getZonedTime
                   if(read valor>0) then tranferir listCli (read conta) (read iban) (read valor) (valor++","++(take 10 (show horasSistema))++","++(take 8( drop 11(show horasSistema)))++"|") else putStrLn ""

-- FUNÇÃO PARA PEDIR O VALOR DO DEPOSITO
getValorTranferir:: IO String
getValorTranferir=do 
                     putStr "\tInforme o valor que deseja tranferir\n\t==> "
                     valor<- getLine
                     if((length valor)>=3 && (isNumero valor) && (read valor)>=100) then return valor
                         else do
                                 putStr "\t\tVALOR  INVALIDO!\n\tSó é possivel TRANSFERIR de 100kz pra cima\n\t\tDESEJA LER NOVAMENTE?\n\t\t1 - Sim\t\t2 - Não\n\t\t==> "
                                 opcao<-getLine
                                 if(opcao=="1") then do getValorTranferir else return "0"


-- FUNCÇAO RESPONSAVEL POR FAZER O DEPOSITO DO VALOR TRANSFERIDO NO IBAN
depositarTransferencia::Clientes->Int->Int->Float->String->IO ()
depositarTransferencia listaArq conta iban valor dados = do 
                                                            if(existeCodigo listaArq conta)
                                                                then do 
                                                                        escreverNoArquivo "clientes.txt" (upDateList2 (listLinhasClientes (inserirValorTransferencia (listaArq) (conta) (iban) (valor))));
                                                                        escrever_arquivo "deposito.txt" ( show (iban)++","++dados) "";
                                                                        escrever_arquivo "levantar.txt" ( show (conta)++","++dados) "";
                                                                        escrever_arquivo "transferencia.txt" (show conta++","++(show iban)++","++dados) ""; 
                                                                        espera "\n\t\tTransferindo.";
                                                                        putStrLn "\n\tTranferência feita com sucesso";
                                                                else putStr "\t\tConta não existe!!"
-- FUNÇAO QUE FAZ O LEVENTAMENTO DOS VALORES PARA TRANSFER NOUTRA CONTA E USA A FUNÇÃO DEPOSITARTRANSFERENCIA PRA FAZER O DEPOSITO NO IBAN
tranferir::Clientes->Int->Int->Float->String->IO ()
tranferir listaArq conta iban valor dados = do if(((existeCodigo listaArq conta) && (existeCodigo (listaArq) (iban))) && (conta/=iban))
                                                  then do
                                                          if((buscaSalario (listaArq) (conta))>=valor) 
                                                            then do
                                                                    espera "\n\t\tVerificando O IBAN .";
                                                                    putStr (("\n\tNome: " ++(upDateList2 (buscaNome (listaArq) (iban))))++("    Conta: "++ show (buscaCodigo (listaArq) (iban)))++ ("    Valor a tranferir: "++(show valor))++"\n\n\t\tConfirmar a Transferencia?\n\t\t\t1 -SIM\n\t\tQualquer tecla para cancelar\n\t==> ");
                                                                    opcao <- getLine;
                                                                    if(opcao=="1") 
                                                                      then do depositarTransferencia listaArq (conta) (iban) (valor) dados
                                                                      else putStrLn "\t\tTranferência Cancelada"
                                                            else do putStrLn ("\tImposivel tirar essa quantia porque o teu saldo atual é de: "++show(buscaSalario (listaArq) (conta)))
                                                  else if(not(existeCodigo (listaArq) conta))  then do putStrLn "\t\tConta não existe!" else putStrLn "\t\tIBAN Invalido!!" 
                                               putStr "\n\tDigite ENTER para continuar\n\t"; getChar; putStrLn ""                 

-- RELATORIO OU ESTATISTICA DE TODAS TRANSFERENCIAS
relactoriosTrasf::Int->IO ()
relactoriosTrasf opcao = do clear;lista <- lerTransfer;
                            if(lista/=[]) 
                              then do
                                    if(opcao ==1) 
                                        then putStr ("************************ LISTAR TODOS TRANSFERÊNCIAS ***********************\n\n"++ (todasTRansferencias lista));
                                        else do
                                                putStr ("************************ LISTAR TODOS TRANSFERÊNCIAS ***********************\n\n\t1 - Hoje\t2 - Qualquer Dia\n\t==>")
                                                opc <- getLine
                                                case opc of 
                                                    "1" -> do
                                                              putStr ("************************ LISTAR TODOS TRANSFERÊNCIAS DE HOJE ***********************\n\n")
                                                              horasSistema <- getZonedTime;
                                                              if((contTransferenciaDia lista (take 10 (show horasSistema)))>0) then putStrLn (transferenciasDiaria lista (take 10 (show horasSistema))) else putStrLn "\t\tLista Vazia"
                                                    "2" -> do
                                                              putStrLn ("************************ LISTAR TODOS TRANSFERÊNCIAS ***********************\n")
                                                              dia <- getDia
                                                              mes <- getMes
                                                              if((contTransferenciaDia lista ("2021-"++(menorQue10 mes)++"-"++(menorQue10 dia)))>0) then putStrLn (transferenciasDiaria lista ("2021-"++(menorQue10 mes)++"-"++(menorQue10 dia))) else putStrLn "\n\t\tLista Vazia"
                                                    otherwise -> relactoriosTrasf 2
                              else putStrLn "Lista Vazia"