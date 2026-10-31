# Jokers

Os jokers ficam no board (até 5) e mudam a pontuação de toda jogada enquanto você os tiver. O código está em `lib/joker.ml`: cada joker é um construtor de `Joker.t`, e o efeito sai de `Joker.changes`.

## Como entram na pontuação

1. A mão dá as fichas e o mult do nível dela (`Play.score`).
2. Cada carta que **pontua** soma as próprias fichas.
3. Os jokers são aplicados **da esquerda para a direita** (`Joker.all_changes`).
4. O resultado é `fichas × mult`.

A ordem importa quando um joker multiplica. Com mult 1:

| Jokers                 | Conta         | Mult |
| ---------------------- | ------------- | ---- |
| `[Joker; Cavendish]`   | (1 + 4) × 3   | 15   |
| `[Cavendish; Joker]`   | 1 × 3 + 4     | 7    |

## Lista

| Construtor        | Nome no jogo       | Efeito                                         | Mudança (`Score.change`)        | Custo | Raridade |
| ----------------- | ------------------ | ---------------------------------------------- | ------------------------------- | ----- | -------- |
| `Joker`           | Coringa            | +4 mult                                        | `AddMult 4.`                    | $2    | Comum    |
| `GreedyJoker`     | Coringa Ganancioso | +3 mult por carta de ♦ que pontua              | `AddMult 3.` por carta          | $5    | Comum    |
| `LustyJoker`      | Coringa Luxurioso  | +3 mult por carta de ♥ que pontua              | `AddMult 3.` por carta          | $5    | Comum    |
| `WrathfulJoker`   | Coringa Irado      | +3 mult por carta de ♠ que pontua              | `AddMult 3.` por carta          | $5    | Comum    |
| `GluttonousJoker` | Coringa Guloso     | +3 mult por carta de ♣ que pontua              | `AddMult 3.` por carta          | $5    | Comum    |
| `JollyJoker`      | Coringa Alegre     | +8 mult se a mão for um par                    | `AddMult 8.` ou nada            | $3    | Comum    |
| `HalfJoker`       | Meio Coringa       | +20 mult se jogar 3 cartas ou menos            | `AddMult 20.` ou nada           | $5    | Comum    |
| `Banner`          | Estandarte         | +30 fichas por descarte restante               | `AddChips (30 × descartes)`     | $5    | Comum    |
| `AbstractJoker`   | Coringa Abstrato   | +3 mult por joker que você tem (inclusive ele) | `AddMult (3 × jokers)`          | $4    | Comum    |
| `Cavendish`       | Cavendish          | ×3 mult                                        | `TimesMult 3.`                  | $4    | Comum    |

Vender um joker devolve metade do custo, arredondada para baixo, e nunca menos de $1 (`Joker.sell_value`).

## O que cada joker enxerga

O efeito depende só do `Joker.context`:

- `play`: a jogada. `Play.hand` dá a categoria, `Play.scoring_cards` as cartas que pontuam e `Play.cards` todas as que foram jogadas.
- `discards_left`: os descartes que sobraram no blind (`Banner`).
- `jokers`: todos os jokers que você tem, em ordem (`AbstractJoker`).

## Diferenças em relação ao Balatro

- No jogo, o `JollyJoker` vale para qualquer mão que *contenha* um par (dois pares, full house...). Aqui ele só vale quando a mão é exatamente um `Pair`.
- No jogo, os jokers de naipe agem carta a carta, enquanto as cartas são contadas. Aqui eles somam tudo no fim. O resultado é o mesmo, porque as cartas e esses jokers só somam, e soma comuta. Isso deixa de valer quando aparecer um joker "por carta" que multiplica. Nesse caso, a pontuação precisa ser separada em duas fases.
- O `Cavendish` aparece direto na loja. No jogo, ele só aparece depois que o Gros Michel se extingue.

## Como adicionar um joker

1. Um construtor novo em `Joker.t`, no `.mli` e no `.ml`.
2. Um caso em `rarity`, `cost` e `changes`. O compilador avisa onde falta (warning 8).
3. Nome e descrição em `Render.joker_name` e `Render.joker_description`.
4. Um caso em `test/test_joker.ml` e uma linha nesta tabela.
