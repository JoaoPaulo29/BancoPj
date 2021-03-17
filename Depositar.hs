module Depositar where

import Data.Time
import Helpers
import Estruturas
import Arquivo

-- FUNÇÃO PARA DEPOSITAR
deposito :: IO ()
deposito = do getChar; clear; cli<- lerClientes;
              putStr "\n*************************** DEPOSÍTO *****************************\n\n\tInforme o número da conta para qual deseja depositar\n\t==> "
              conta <- getLine; valor <- getValorDeposito; horasSistema <- getZonedTime --PEGAR A HORA DO SISTEMA 
              if((read (valor))>0) 
               then do
                      if(not(existeCodigo cli (read conta))) 
                          then do
                                  putStr "\n\tConta não existe!!\n\tDESEJA DEPOSITAR NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t==> "
                                  opcao <- getLine
                                  if(opcao=="1") then deposito else saindo
                          else do
                                  escreverNoArquivo "clientes.txt" (upDateList2 (listLinhasClientes (inserirValor (cli) (read conta) (read valor))));
                                  escrever_arquivo "deposito.txt" (conta++","++valor++","++(take 10 (show horasSistema))++","++(take 8( drop 11(show horasSistema)))++"|") "";
                                  espera ("\tDepositando ."); cli2<-lerClientes;
                                  putStrLn ("\n"++(listUmCliente cli2 (read conta))++"\n\tDeposito feito com sucesso");
               else do
                    putStr "\n\tDeposito não efeituado\n\tNão é permitido o deposito de valores a baixa de 100kz\n\tDESEJA DEPOSITAR NOVAMENTE?\t1 - SIM\t2 - NÃO\n\t==> "
                    opcao <- getLine
                    if (opcao=="1") then deposito else saindo
              putStr "\n\tDigite ENTER para continuar\n\t"; getChar; putStrLn ""      


-- FUNÇÃO PARA PEDIR O VALOR DO DEPOSITO
getValorDeposito::IO String
getValorDeposito=do putStr "\tInforme o valor que deseja depositar\n\t==> "
                    valor<- getLine
                    if((length valor)>=3 && (isNumero valor) && (read valor)>=100) then return valor
                      else do
                              putStr "\tVALOR  INVALIDO!\n\tSó é possivel DEPOSITAR de 100kz pra cima\n\tDESEJA LER NOVAMENTE?\n\t1 - Sim\t\t2 - Não\n\t==> "
                              opcao<-getLine
                              if(opcao=="1") then getValorDeposito else return ("0")                    
-- RELATORIO OU ESTATISTICA DE TODOS DEPOSITOS
relactorioDeposi::Int->IO ()
relactorioDeposi opcao = do clear; lista <- lerDeposito;
                            if(lista/=[]) 
                              then do
                                    if(opcao ==1) 
                                        then do
                                                putStrLn ("************************ LISTAR TODOS DEPOSITOS ***********************\n\n"++(todosDositos lista)++"\n");
                                        else do
                                                putStr ("************************ LISTAR TODOS DEPOSITOS ***********************\n\n\t1 - Hoje\t2 - Qualquer Dia\n\t==> ")
                                                opc <- getLine
                                                case opc of 
                                                    "1" -> do
                                                              putStr ("************************ LISTAR TODOS DEPOSITOS DE HOJE ***********************\n\n\t")
                                                              horasSistema <- getZonedTime;
                                                              if((contDepositosDia lista (take 10 (show horasSistema)))>0) then putStrLn (depositoDiaria lista (take 10 (show horasSistema))) else putStrLn "\t\tLista Vazia"
                                                    "2" -> do
                                                              putStr ("************************ LISTAR TODOS DEPOSITOS ***********************\n\n\t")
                                                              dia <- getDia
                                                              mes <- getMes
                                                              if((contDepositosDia lista ("2021-"++(menorQue10 mes)++"-"++(menorQue10 dia)))>0) then putStrLn (depositoDiaria lista ("2021-"++(menorQue10 mes)++"-"++(menorQue10 dia))) else putStrLn "\t\tLista Vazia"
                                                    otherwise -> relactorioDeposi opcao
                              else putStrLn "\n\tLista Vazia\n"