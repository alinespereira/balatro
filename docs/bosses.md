# Chefes

Todo ante sorteia um chefe quando começa. Ele aparece no status desde o Small Blind, mas só age no **Boss Blind** daquele ante. O código está em `lib/boss.ml`: cada chefe é um construtor de `Boss.t`.

## Como um chefe muda o blind

1. **Antes de começar** (`Boss.limits`): pode mudar as jogadas, os descartes e a meta. O `Blind.create ~boss` aplica isso.
2. **A cada jogada** (`Boss.debuffs`): cartas "enfraquecidas" continuam na jogada (a mão continua sendo um par, por exemplo), mas não pontuam. Elas não somam fichas e os jokers não as veem. Quem tira essas cartas é o `Play.without`.
3. **Os jokers** (`Boss.disables_jokers`): se for verdade, nenhum joker pontua.

## Lista

| Construtor     | Nome no jogo | Efeito                                   | Onde age          |
| -------------- | ------------ | ---------------------------------------- | ----------------- |
| `Drought`      | A Seca       | Sem descartes                            | `limits`          |
| `Hourglass`    | A Ampulheta  | Uma jogada a menos (no mínimo uma)       | `limits`          |
| `Giant`        | O Gigante    | Meta 50% maior                           | `limits`          |
| `Eclipse suit` | O Eclipse    | Cartas do naipe sorteado não pontuam     | `debuffs`         |
| `Fog`          | A Névoa      | Figuras (J, Q, K) não pontuam            | `debuffs`         |
| `Muzzle`       | A Mordaça    | Os jokers não fazem nada                 | `disables_jokers` |

`Eclipse` carrega um naipe (`Eclipse of Card.suit`), então são 4 eclipses diferentes. O `[@@deriving enumerate]` gera `Boss.all` com os 9 chefes sozinho, usando o `Card.all_of_suit`.

## Ideias para depois

- Não repetir o chefe do ante anterior.
- Chefes com estado, como "a mesma mão não pontua duas vezes", que precisariam guardar as mãos jogadas no blind.
