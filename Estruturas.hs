module Estruturas where

--Dados Da Conta
type NumeroConta = Int
type Nome = String
type SobreNome = String
type BilheteIdentidade = String
type Telefone = Int
type Dia = Int
type Mes = Int
type Ano = Int
type Saldo = Float
type Data = String
type Iban = Int
type DataCadastro = String
type Hora = String
type Cliente = (NumeroConta ,Nome, Data,BilheteIdentidade, Telefone,Saldo,DataCadastro)
type Clientes = [Cliente]
-- Senhas Das Chamadas
type NumeroSenha = Int
type LetraSenha = String
type Senha = (LetraSenha,NumeroSenha)
type Senhas= [Senha]
-- DADOS DAS TRANFERÊNCIAS
type Transferencia = (NumeroConta,Iban,Saldo,Data,Hora)
type Transferencias = [Transferencia]

-- DADOS DO DEPOSITO
type Deposito = (NumeroConta,Saldo,Data,Hora)
type Depositos = [Deposito]
--
-- DADOS DO LEVANTAMENTO
type Levantamento = (NumeroConta,Saldo,Data,Hora)
type Levantamentos = [Levantamento]