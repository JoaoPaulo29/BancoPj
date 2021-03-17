 module Levantar where

import Data.Time
import Helpers
import Arquivo
import Estruturas

levantamento :: IO ()
levantamento = do list<- lerClientes; getChar; clear;
                  putStr "****************************** LEVANTAMENTO ******************************\n\n\tInforme o seu número de conta\n\t==> "
                  conta <- getLine
                  valor <- getValorLevantar
                  horasSistema <- getZonedTime --PEGAR A HORA DO SISTEMA 
                  if(read valor >0) 
                        then do
                                if(existeCodigo list (read conta)) 
                                  then do
                                          if((buscaSalario (list) (read conta))>=(read valor))
                                            then do
                                                    escreverNoArquivo "clientes.txt" (upDateList2 (listLinhasClientes (inserirValor (list) (read conta) (-(read valor)))));
                                                    escrever_arquivo "levantar.txt" (conta++","++valor++","++(take 10 (show horasSistema))++","++(take 8( drop 11(show horasSistema)))++"|") "";
                                                    espera "\n\t\tLevantando .";
                                                    putStrLn ("\n\t\tlevantamento feito com sucesso!\n\n"++tirarValorLevantar (list) (read valor) (read conta));
                                            else do
                                                    putStr ("\n\tImposivel tirar essa quantia porque o teu saldo atual é de: "++(show (buscaSalario (list) (read conta)))++"\nDESESJA LEVANTAR NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t==>")
                                                    opcao <- getLine
                                                    if(opcao=="1") then levantamento else saindo
                                  else do
                                          putStr ("\t\tConta não existe!!\n\tDESESJA LEVANTAR NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t==> ")
                                          opcao <- getLine
                                          if(opcao=="1") then levantamento else saindo
                          else saindo
                  putStr "\n\tDigite ENTER para continuar\n\t "; getChar; putStrLn ""  
-- FUNÇÃO PARA PEDIR O VALOR DO LEVANTAMENTO
getValorLevantar:: IO String
getValorLevantar=do 
                    putStr "\tInforme o valor que deseja Levantar\n\t==> "
                    valor<- getLine
                    if((length valor)>=4 && (isNumero valor) && (read valor)>=1000) then return valor
                      else do
                              putStr "\t\tVALOR  INVALIDO!\n\t\tSó é possivel LEVANTAR  de 1000kz pra cima\n\t\tDESEJA LER NOVAMENTE?\n\t1 - Sim\t\t2 - Não\n\t\t==> "
                              opcao<-getLine
                              if(opcao=="1") then getValorLevantar else return ("0")
-- RELATORIO OU ESTATISTICA DE TODOS LEVANTAMENTO
relactoriosLevan::Int->IO ()
relactoriosLevan opcao = do clear
                            lista <- lerLevantam;
                            if(lista/=[]) 
                              then do
                                    if(opcao ==1) 
                                        then do
                                                putStr ("************************ LISTAR TODOS LEVANTAMENTOS ***********************\n\n"++(todosLevantamentos lista)++"\n");
                                        else do
                                                putStr ("************************ LISTAR TODOS LEVANTAMENTOS ***********************\n\n\t1 - Hoje\t2 - Qualquer Dia\n\t==> ")
                                                opc <- getLine
                                                case opc of 
                                                    "1" -> do
                                                              putStr ("************************ LISTAR TODOS LEVANTAMENTOS DE HOJE ***********************\n\n")
                                                              horasSistema <- getZonedTime;
                                                              if(contLevantamentoDia lista (take 10 (show horasSistema)))>0 then putStrLn (levantamentosDiaria lista (take 10 (show horasSistema))) else putStrLn "\n\t\tLista Vazia"
                                                    "2" -> do
                                                              putStr ("************************ LISTAR TODOS LEVANTAMENTOS ***********************\n\n")
                                                              dia <- getDia
                                                              mes <- getMes
                                                              if((contLevantamentoDia lista ("2021-"++(menorQue10 mes)++"-"++(menorQue10 dia)))>0) then putStrLn (levantamentosDiaria lista ("2021-"++(menorQue10 mes)++"-"++(menorQue10 dia))) else putStrLn "\n\t\tLista Vazia"
                                                    otherwise -> relactoriosLevan 2
                              else putStrLn "Lista Vazia"