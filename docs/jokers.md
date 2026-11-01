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
| `JollyJoker`      | Coringa Alegre     | +8 mult se a mão tiver um par                  | `AddMult 8.` ou nada            | $3    | Comum    |
| `HalfJoker`       | Meio Coringa       | +20 mult se jogar 3 cartas ou menos            | `AddMult 20.` ou nada           | $5    | Comum    |
| `Banner`          | Estandarte         | +30 fichas por descarte restante               | `AddChips (30 × descartes)`     | $5    | Comum    |
| `AbstractJoker`   | Coringa Abstrato   | +3 mult por joker que você tem (inclusive ele) | `AddMult (3 × jokers)`          | $4    | Comum    |
| `Cavendish`       | Cavendish          | ×3 mult                                        | `TimesMult 3.`                  | $4    | Comum    |

### Jokers com estado

Estes carregam um número que muda durante a partida. O número é o payload do construtor, e a loja vende cada um no estado inicial (`Joker.fresh`).

| Construtor                 | Nome no jogo      | Pontua             | Como o estado muda (`Joker.react`)                                   | Início | Custo |
| -------------------------- | ----------------- | ------------------ | -------------------------------------------------------------------- | ------ | ----- |
| `GreenJoker { mult }`      | Coringa Verde     | `AddMult mult`     | +1 por mão jogada, -1 por descarte, nunca abaixo de 0                | 0      | $4    |
| `RideTheBus { mult }`      | Passeio de Ônibus | `AddMult mult`     | +1 por mão jogada sem figura (J, Q, K) que pontua; senão volta a 0   | 0      | $6    |
| `Runner { chips }`         | Corredor          | `AddChips chips`   | +15 por mão jogada que contém uma sequência                          | 0      | $5    |
| `IceCream { chips }`       | Sorvete           | `AddChips chips`   | -5 por mão jogada; no 0, derrete e sai do board                      | 100    | $5    |

Todos são comuns.

Vender um joker devolve metade do custo, arredondada para baixo, e nunca menos de $1 (`Joker.sell_value`).

## O que cada joker enxerga

O efeito depende só do `Joker.context`:

- `play`: a jogada. `Play.hand` dá a categoria, `Play.scoring_cards` as cartas que pontuam e `Play.cards` todas as que foram jogadas.
- `discards_left`: os descartes que sobraram no blind (`Banner`).
- `jokers`: todos os jokers que você tem, em ordem (`AbstractJoker`).

## Como o estado muda

Depois de cada jogada e de cada descarte, o `Blind` chama `Board.update_jokers (Joker.react evento)`. O `Joker.react` recebe um `Joker.event` (`Played play` ou `Discarded`) e devolve o joker novo, ou `None` quando ele acaba (o Sorvete derretido). O board guarda os jokers novos, então o estado passa de um blind para o outro.

Nada é mutável: cada jogada devolve um board novo com jokers novos, como `{ board with ... }` faz com o resto.

Os jokers reagem **depois** que a jogada pontuou. Sob o chefe que desliga os jokers (`Muzzle`), eles nem pontuam nem reagem.

## Diferenças em relação ao Balatro

- No jogo, o Coringa Verde, o Passeio de Ônibus e o Corredor crescem *antes* de pontuar a mão. Aqui todos reagem depois, então eles ficam uma mão atrás: o Coringa Verde comprado agora dá +0 na primeira mão e +1 na segunda. O Sorvete, que no jogo pontua e depois derrete, funciona igual.
- No jogo, os jokers de naipe agem carta a carta, enquanto as cartas são contadas. Aqui eles somam tudo no fim. O resultado é o mesmo, porque as cartas e esses jokers só somam, e soma comuta. Isso deixa de valer quando aparecer um joker "por carta" que multiplica. Nesse caso, a pontuação precisa ser separada em duas fases.
- O `Cavendish` aparece direto na loja. No jogo, ele só aparece depois que o Gros Michel se extingue.

## Como adicionar um joker

1. Um construtor novo em `Joker.t`, no `.mli` e no `.ml`.
2. Um caso em `rarity`, `cost` e `changes`. O compilador avisa onde falta (warning 8).
3. O joker na lista `Joker.all`, que é escrita à mão. Se ele tiver estado, um caso em `fresh` e outro em `react`.
4. Nome e descrição em `Render.joker_name` e `Render.joker_description`.
5. Um caso em `test/test_joker.ml` e uma linha nesta tabela.
