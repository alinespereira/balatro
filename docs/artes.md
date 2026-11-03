# Desenhando as cartas

As cartas são desenhadas com formas do raylib, sem arquivos de imagem. O código
fica em `gui/art/card_art.ml` e a prancheta para ver o resultado é
`dune exec balatro-art` (a tecla `S` salva `cartas.png`).

## As formas

As coordenadas são em pixels, com `x` para a direita e `y` para baixo.

| Forma | Função |
| --- | --- |
| círculo | `Raylib.draw_circle_v centro raio cor` |
| triângulo | `triangle a b c cor` (de `card_art.ml`, aceita os cantos em qualquer ordem) |
| polígono regular | `Raylib.draw_poly centro lados raio rotacao cor` |
| retângulo arredondado | `rounded ~at ~w ~h cor` (de `card_art.ml`) |
| linha grossa | `Raylib.draw_line_ex inicio fim espessura cor` |

Cores: `Raylib.Color.create r g b a`, com cada número de 0 a 255; `a` é a
opacidade. A paleta fica no topo de `card_art.ml`.

`point centro dx dy` dá o ponto deslocado do centro. Desenhar tudo em função de
`size` faz o mesmo naipe servir para o canto (pequeno) e para o meio (grande).

## O que falta

- Copas, espadas e paus em `draw_suit` (hoje aparecem como um círculo cinza com
  um X). Ouros já está feito, como exemplo: dois triângulos.
- Um desenho para o verso em `draw_back` (hoje é só a cor do baralho).

Ideias de composição: copas são dois círculos e um triângulo para baixo; espadas,
o mesmo de cabeça para baixo e uma haste; paus, três círculos e uma haste.
