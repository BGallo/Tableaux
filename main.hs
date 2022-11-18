type Var    = String
data Op = Nao | SeEntao | E | Ou | NaoOperador
data Formula = Formula{operador :: Op, operandoUm::Formula,operandoDois::Formula,nome::String,valor::Bool} 

verdadeiro :: formula f ->formula x
verdadeiro f
    | operador f == NaoOperador = Formula{operador=NaoOperador,operandoUm= operandoUm f,operandoDois= operandoDois f,nome=nome f,valor = True} 
    | operador f == Nao = falsificar Formula{operador=NaoOperador,operandoUm= foperandoUm f,operandoDois= operandoDois f,nome=nome f,valor =  valor  }

falsificar :: formula f->formula x

falsificar f 
    | operador f == NaoOperador = Formula{operador=NaoOperador,operandoUm= operandoUm f,operandoDois= operandoDois f,nome=nome f,valor = False} 
    | operador f == Nao = Formula{operador=Nao,operandoUm= foperandoUm f,operandoDois= operandoDois f,nome=nome f,valor = ! valor f } 
    | operador f == SeEntao = 