# Cartas de tarô

Os tarôs são consumíveis: você compra na loja por $3, eles ficam num espaço de consumível do board (até 2) e você usa quando quiser, durante o blind ou na loja, com `u`:

```
u 1         usa o consumível 1 (O Eremita não precisa de cartas)
u 1 2 3     usa o consumível 1 nas cartas 2 e 3 da mão
```

O código está em `lib/consumables/tarot.ml`. Cada tarô diz quantas cartas precisa (`Tarot.selection`), o que faz com elas (`Tarot.change_cards`) e com o dinheiro (`Tarot.change_money`). `Board.use_tarot` troca as cartas na mão, e `Run.use_consumable` junta tudo.

As mudanças valem para o resto da partida: a carta mudada volta para o baralho no próximo blind do jeito que ficou, e a carta destruída sai do jogo.

## Lista

| Tarô | Cartas | Efeito |
| ---- | ------ | ------ |
| A Estrela (`TheStar`) | 1 a 3 | viram ♦ |
| A Lua (`TheMoon`) | 1 a 3 | viram ♣ |
| O Sol (`TheSun`) | 1 a 3 | viram ♥ |
| O Mundo (`TheWorld`) | 1 a 3 | viram ♠ |
| A Força (`Strength`) | 1 a 2 | sobem um valor (o Ás vira 2) |
| O Enforcado (`TheHangedMan`) | 1 a 2 | são destruídas |
| A Morte (`Death`) | 2 | a da esquerda vira uma cópia da da direita |
| O Eremita (`TheHermit`) | 0 | dobra o dinheiro, no máximo +$20 |

Os outros tarôs ainda não fazem nada e não aparecem na loja. Os que mudam o aprimoramento das cartas (O Mago, A Imperatriz, O Hierofante...) dependem de a pontuação ler `Decoration`, que fica para depois.
