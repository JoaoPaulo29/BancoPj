module FilaDeEspera where

import Estruturas -- MODULO ESTRUTURA ONDE TEM OS NOSSOS ATRIBUTOS OU VARIAVEIS
import Helpers  --NOSSO ARQUIVO ONDE TEM AS FUNÇÕES AUXILHARES CRIADA POR NOS
import Arquivo  --ONDE TEM AS FUNCOES DE ESCRITA NO FICHEIRO


--  MENU FILA DEESPERA ONDE É CADASTRADA SENHA NO FICHEIRO
menuFilaEspera :: Senhas->IO ()
menuFilaEspera senha = do clear; putStr "***************MENU FILA DE ESPERA ******************\n\n"
                          putStrLn "\tA - Criação de conta\n\tB - Depósito\n\tC - Levantamento\n\tD - Transferência\n\tE - Extracto\n\tF - Consulta\n\tG - Sair"
                          putStr "\n****************************************************\n\n\tPor favor, escolha uma opção\n\t==> "
                          opcao <- getLine; letra <- letrasConvert opcao ; codigo <- gerarSenha letra 1 --GERA O ID DA SENHA
                          let vet= insertSenha senha letra codigo
                          clear
                          case (letra>="A" && letra<"G") of
                              True ->do{
                                       putStrLn ("\n\t\tSenha : "++letra++"-"++(show codigo)++"\n\t\tAguarde a chamada\n");
                                       escreverNoArquivo ("senhas.txt") (upDateList2 (listarSenas vet));
                                       escreverNoArquivo ("senhasValidar.txt") (upDateList2 (listarSenas vet));
                                       putStrLn "\tDigite Enter Para Continuar";
                                       getChar;
                                       menuFilaEspera (vet);};
                              False -> saindo

-- **********************************************************************************************
-- FUNÇÃO QUE RETORNA O CODIGO DA SENHA OU SEJA O NUMERO DA SENHA
gerarSenha::String->Int->IO Int
gerarSenha  letra codigo= do senha <-lerSenhaVali;listSenh<- ler__Senhas;
                             if ((existeSenha listSenh letra codigo) || (existeSenha senha letra codigo)) then do gerarSenha (letra) (codigo +1) else do return codigo