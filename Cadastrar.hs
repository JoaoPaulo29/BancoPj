module Cadastrar where


import Data.Time
import Arquivo
import Estruturas
import Helpers


-- FUNÇÃO PARA CADASTRAR UM CLIENTE
cadastro :: IO ()
cadastro = do getChar;clear; lista <-lerClientes
              putStrLn "******************* CADASTRAMENTO ******************\n"
              nome <- getNome; sobrenome <- getSobrenome; tel <- getTelefone lista; dia <-getDia; mes<-getMes; ano<-getAno; bi <- getBilhete lista
              let codigo = (gerarCodigo lista 1)
              horasSistema <- getZonedTime
              if(dia =="0" || mes=="0" || nome=="0" || sobrenome == "0" || tel =="0" || ano =="0" || bi=="0") 
                  then do
                          putStr "\n\tDados Invalidos\n\tDeseja Preencher Novamente?\n\t1 - SIM\t2 - NÃo\n\t==>"
                          opcao <- getLine
                          if(opcao=="1") then cadastro else saindo
                  else do 
                        let dados=(cadastrarCliente lista (codigo) nome sobrenome dia mes ano bi (read tel) 0.0 (take 10 (show horasSistema)))
                        putStr ("\n\tNOME: "++nome++"    SOBRENOME: "++sobrenome++"    CODIGO: "++(show codigo)++"    DATA DE NASCIMENTO: "++dia++"/"++mes++"/"++ano++"    BI: "++bi++"    TELEFONE: "++tel++"Deseja Salvar?\n\n\t1 - SIM\t\t2 - ALTERAR\n\t\tOutra tecla para terminar...\n\t==> ")
                        opc <- getLine
                        case opc of
                            "1" -> do
                                      espera "\n\t\tCadastrando ."
                                      escreverNoArquivo "clientes.txt" (listLinhasClientes (dados)) 
                                      putStr "\tConta criada com sucesso\n"
                            "2" -> cadastro
                            _ -> saindo
              putStr "\n\tDigite ENTER para continuar\n\t==> "; getChar; putStrLn ""      
-- FUNÇÃO PARA GERAR O CODIGO DO CLIENTE
gerarCodigo :: Clientes->Int->Int
gerarCodigo listCli codigo= if (existeCodigo (listCli) (codigo)) then gerarCodigo (listCli) (codigo +1) else codigo
-- RELATORIO DE TODO CADASTROS 
relactoriosCadas::Int->IO ()
relactoriosCadas opcao = do clear; lista <- lerClientes;
                            if(lista/=[]) 
                                then do
                                      if(opcao ==1) 
                                        then putStrLn ("************************ LISTAR TODOS CADASTROS ***********************\n\n"++(todosClientes lista)++"\n");
                                        else do
                                                putStr ("************************ LISTAR TODOS CADASTROS ***********************\n\n\t1 - Hoje\t2 - Qualquer Dia\n\t==> ")
                                                opc <- getLine
                                                case opc of 
                                                    "1" -> do
                                                              putStr ("************************ LISTAR TODOS CADASTROS DE HOJE ***********************\n\n")
                                                              horasSistema <- getZonedTime;
                                                              if((contCadastroDia lista (take 10 (show horasSistema)))>0) then putStrLn (clientesDiario lista (take 10 (show horasSistema))) else putStrLn "\tLista Vazia"
                                                    "2" -> do
                                                              putStr ("************************ LISTAR TODOS CADASTROS ***********************\n\n\t")
                                                              dia <- getDia
                                                              mes <- getMes
                                                              if((contCadastroDia lista ("2021-"++(menorQue10 mes)++"-"++(menorQue10 dia)))>0) then putStrLn (clientesDiario lista ("2021-"++(menorQue10 mes)++"-"++(menorQue10 dia))) else putStrLn "\t\tLista Vazia"
                                                    otherwise -> relactoriosCadas opcao
                                else putStrLn "************************ LISTAR TODOS CADASTROS ***********************\n\t\tLista Vazia\n"