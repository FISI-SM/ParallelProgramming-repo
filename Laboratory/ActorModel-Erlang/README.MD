# Aproximação Trapezoidal em Erlang

## Implementação

Baixe o arquivo aqui [trap.erl](trap.erl).

Para compilar e rodar faça:

```bash
$ erlc trap.erl
$ erl -noshell -s trap main A B N P
```

Onde `A` e `B` são os valores inicial e final de para `f(x)`,  
`N` é o número de trapézios e `P` o número de processos.
