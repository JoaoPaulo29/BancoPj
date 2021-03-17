module Main where

import Arquivo
import Menu
import FilaDeEspera


-- FUNÇÃO PRINCIPAL PARA A PARTE DO OPERADOR
main :: IO ()
main = do
          carregarSenhas;
          carregarLevant1;
          carregarDeposi1;
          carregarClient1;
          carregarTransf1;
          menuPrincipal;

-- FUNÇÃO PRINCIPAL PARA A PARTE DO FILA DE ESPERA
fila :: IO ()
fila = do
          carregarSenhas;
          carregaSenhas1;
          senha <-ler__Senhas;
          menuFilaEspera (senha);