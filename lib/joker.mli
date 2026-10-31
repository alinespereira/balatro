type rarity =
  | Common
  | Uncommon
  | Rare
  | Legendary

type t = private {
  name: string;
  description: string;
  rarity: rarity;
  cost: int;
}
