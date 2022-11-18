import Data.List
--import Data.Tree
import GHC.Maybe

type Var    = String
data Op = Nao | SeEntao | E | Ou | NaoOperador deriving(Eq)
data Formula = Formula{operador :: Op, operandoUm:: Maybe Formula,operandoDois:: Maybe Formula,nome::String,valor::Bool} 

data Folha = Folha{valorVerdade::Bool,foiVisitado::Bool,forma::String}
data Tree arvore = EmptyTree | Node arvore (Tree arvore) (Tree arvore) deriving(Read,Eq,Show)

verdadeiro :: Formula  -> Maybe Formula 
verdadeiro f
    | operador f == NaoOperador = Formula{operador=NaoOperador,operandoUm= case operandoUm f of Just a -> operandoUm f,operandoDois= operandoDois f,nome=nome f,valor = True} 
    | operador f == Nao = falsear Formula{operador=NaoOperador,operandoUm= operandoUm f,operandoDois= operandoDois f,nome=nome f,valor =  valor f }

falsear :: Formula -> Maybe Formula 

falsear f 
    | operador f == NaoOperador = Formula{operador=NaoOperador,operandoUm= operandoUm f,operandoDois= operandoDois f,nome=nome f,valor = False} 
    | operador f == Nao = verdadeiro Formula{operador=NaoOperador,operandoUm= operandoUm f,operandoDois= operandoDois f,nome=nome f,valor =  valor f } 
    | operador f == SeEntao = Formula{operador=operador (operandoDois  f),operandoUm= verdadeiro (operandoUm f),operandoDois= falsear(operandoDois f),nome=nome f,valor =  valor f }

