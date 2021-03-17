module Arquivo where

import System.IO -- WriteMode, AppendMode, ReadMode, hPutStr, hFlush, nClose, openFile, hIsEOF e hGetLine
import System.IO.Error -- ioError, isDoesNotExistError
import Control.Exception -- catch
import Helpers
import Estruturas

escreverNoArquivo::String->String->IO ()
escreverNoArquivo caminho conteudo = do
                                        arquivo <- openFile caminho WriteMode;
                                        hPutStrLn arquivo conteudo;
                                        hFlush arquivo;
                                        hClose arquivo;
escrever_arquivo::String->String->String->IO ()
escrever_arquivo caminho valor mensagem = do
                                             arq <- openFile caminho AppendMode
                                             hPutStr arq valor
                                             putStr mensagem
                                             hFlush arq
                                             hClose arq
lerClientes::IO Clientes
lerClientes = do
                 arquivo <- openFile "clientes.txt" ReadMode;
                 t<- hIsEOF arquivo;
                 if(t)
                    then do
                            hClose arquivo;
                            return [];
                    else do
                           dados <- hGetLine arquivo;
                           hClose arquivo;
                           return (clientes (map words (lines (upDateList (dados)))));
lerTransfer::IO Transferencias
lerTransfer = do
                 arquivo <- openFile "transferencia.txt" ReadMode;
                 t<- hIsEOF arquivo;
                 if(t)
                    then do
                            hClose arquivo;
                            return [];
                    else do
                           dados <- hGetLine arquivo;
                           hClose arquivo;
                           return (tranferencias (map words (lines (upDateList (dados)))));
lerDeposito::IO Depositos
lerDeposito = do
                 arquivo <- openFile "deposito.txt" ReadMode;
                 t<- hIsEOF arquivo;
                 if(t)
                    then do
                            hClose arquivo;
                            return [];
                    else do
                           dados <- hGetLine arquivo;
                           hClose arquivo;
                           return (depositos (map words (lines (upDateList (dados)))));
ler__Senhas::IO Senhas
ler__Senhas = do
                 arquivo <- openFile "senhas.txt" ReadMode;
                 t<- hIsEOF arquivo;
                 if(t)
                    then do
                            hClose arquivo;
                            return [];
                    else do
                           dados <- hGetLine arquivo;
                           hClose arquivo;
                           return (senhas (map words (lines (upDateList (dados)))));
lerSenhaVali::IO Senhas
lerSenhaVali = do
                 arquivo <- openFile "senhasValidar.txt" ReadMode;
                 t<- hIsEOF arquivo;
                 if(t)
                    then do
                            hClose arquivo;
                            return [];
                    else do
                           dados <- hGetLine arquivo;
                           hClose arquivo;
                           return (senhas (map words (lines (upDateList (dados)))));
lerLevantam::IO Levantamentos
lerLevantam = do
                 arquivo <- openFile "levantar.txt" ReadMode;
                 t<- hIsEOF arquivo;
                 if(t)
                    then do
                            hClose arquivo;
                            return [];
                    else do
                           dados <- hGetLine arquivo;
                           hClose arquivo;
                           return (levantamentos (map words (lines (upDateList (dados)))));
-- CARREGA O FICHEIRO DE SENHA SE ELE EXISTIR CASO NÃO EXISTA CRIA O FICHEIRO 
carregarSenhas:: IO ()
carregarSenhas = do
                    {catch(ler_arq) tratar_erro;}
                    where
                          ler_arq = do arquivo<-openFile "senhas.txt" ReadMode; hClose arquivo;  return ();
                          tratar_erro erro = if(isDoesNotExistError erro) 
                                               then do
                                                     {
                                                       arq <- openFile "senhas.txt" WriteMode;
                                                       hPutStrLn arq "";
                                                       hClose arq;
                                                       return ();}
                                               else ioError erro;
carregarClient1:: IO ()
carregarClient1= do
                    {catch(ler_arq) tratar_erro;}
                    where
                          ler_arq = do arquivo<-openFile "clientes.txt" ReadMode; hClose arquivo; return ();
                          tratar_erro erro = if(isDoesNotExistError erro) 
                                               then do
                                                     {
                                                       arq <- openFile "clientes.txt" WriteMode;
                                                       hPutStrLn arq "";
                                                       hClose arq;
                                                       return ();}
                                               else ioError erro;
carregarTransf1::IO ()
carregarTransf1 = do
                    {catch(ler_arq) tratar_erro;}
                    where
                          ler_arq = do arquivo<-openFile "transferencia.txt" ReadMode; hClose arquivo; return ();
                          tratar_erro erro = if(isDoesNotExistError erro) 
                                               then do
                                                     {
                                                       arq <- openFile "transferencia.txt" WriteMode;
                                                       hPutStrLn arq "";
                                                       hClose arq;
                                                       return ();}
                                               else ioError erro;
carregarDeposi1::IO ()
carregarDeposi1 = do
                    {catch(ler_arq) tratar_erro;}
                    where
                          ler_arq = do arquivo<-openFile "deposito.txt" ReadMode; hClose arquivo; return ();
                          tratar_erro erro = if(isDoesNotExistError erro) 
                                               then do
                                                     {
                                                       arq <- openFile "deposito.txt" WriteMode;
                                                       hPutStrLn arq "";
                                                       hClose arq;
                                                       return ();}
                                               else ioError erro;
carregarLevant1::IO ()
carregarLevant1 = do
                    {catch(ler_arq) tratar_erro;}
                    where
                          ler_arq = do arquivo<-openFile "levantar.txt" ReadMode; hClose arquivo; return ();
                          tratar_erro erro = if(isDoesNotExistError erro) 
                                               then do
                                                     {
                                                       arq <- openFile "levantar.txt" WriteMode;
                                                       hPutStrLn arq "";
                                                       hClose arq;
                                                       return ();}
                                               else ioError erro;
-- CARREGA O FICHEIRO DE SENHAvALIDAR SE ELE EXISTIR CASO NÃO EXISTA CTRIA O FICHEIRO 
-- ESSE FICHEIRO É RESPONSAVEL POR ARMAZENAR TODAS SENHAS FEITAS NUM DIA PARA QUE AS SENHAS NÃO SE REPITAM 
carregaSenhas1::IO ()
carregaSenhas1 = do
                    {catch(ler_arq) tratar_erro;}
                    where
                         ler_arq = do arquivo<-openFile "senhasValidar.txt" ReadMode; hClose arquivo;  return ();
                         tratar_erro erro = if(isDoesNotExistError erro) 
                                               then do
                                                      {
                                                       arq <- openFile "senhasValidar.txt" WriteMode;
                                                       hPutStrLn arq "Q,1|";
                                                       hClose arq;
                                                       return ();}
                                               else ioError erro;
