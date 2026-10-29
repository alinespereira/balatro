# Balatro

Projeto em OCaml, em desenvolvimento, para modelar cartas, baralhos, mãos de poker e efeitos de pontuação do Balatro.

## Preparar o ambiente

Pré-requisitos: Git e opam instalados. O CI utiliza OCaml 5.4.1; o projeto exige Dune 3.24 ou superior.

```sh
git clone git@github.com:alinespereira/balatro.git
cd balatro
opam switch create . 5.4.1
opam install . --deps-only --with-test --yes
```

Se já houver um switch configurado para o projeto, basta instalar as dependências.

## Compilar e executar

```sh
opam exec -- dune build @all
opam exec -- dune exec balatro
```

O executável ainda é um ponto de entrada inicial; o projeto não implementa um jogo completo.

## Rodar os testes

```sh
opam exec -- dune runtest --force
```

O parâmetro `--force` executa os testes novamente mesmo quando os arquivos não mudaram.

## Ativar o pre-commit

O hook em `.githooks/pre-commit` compila o projeto e executa os testes antes de cada commit. Se alguma etapa falhar, o commit é cancelado.

Ative-o na raiz do repositório:

```sh
chmod +x .githooks/pre-commit
git config --local core.hooksPath .githooks
```

A pasta `.githooks/` é versionada, mas a configuração `core.hooksPath` é local. Cada pessoa precisa executar esses comandos em cada novo clone.

Para conferir a configuração:

```sh
git config --local --get core.hooksPath
```

O resultado esperado é `.githooks`.

O hook verifica os arquivos atuais da pasta de trabalho, incluindo alterações que ainda não foram adicionadas ao commit. Ele pode ser ignorado com `git commit --no-verify`, portanto não substitui o CI.

## Integração contínua

O workflow `.github/workflows/ci.yml` roda no GitHub Actions em pushes e pull requests. Também pode ser iniciado manualmente pela aba **Actions**.

Ele prepara OCaml 5.4.1, instala as dependências, compila o projeto e executa os testes. Os resultados ficam disponíveis na aba **Actions** do repositório.

## Estrutura

```text
bin/                Executável
lib/                Cartas, baralho, tabuleiro, mãos de pôquer e pontuação
lib/consumables/    Planetas, tarôs e cartas espectrais
test/               Testes
.githooks/          Hooks locais versionados
.github/workflows/  Configuração do CI
```

## Convenções

Cada módulo reúne um conceito do jogo: o tipo e as funções que trabalham com ele. O `.mli` decide o que fica visível:

- tipos sem regra a proteger (naipes, valores, planetas, `Score.t`) expõem os construtores;
- tipos com uma regra a proteger (`Deck.t`, `Hand.Levels.t`, `Board.t`) são abstratos;
- `private` permite ler um valor de fora sem poder construí-lo (`Hand.Levels.entry`).

Construtores usam PascalCase (`HighCard`, `TheFool`). Módulos que vêm de arquivos seguem o nome do arquivo (`poker/hand.ml` vira `Poker.Hand`).
