type rarity =
  | Common
  | Uncommon
  | Rare
  | Legendary

type t = {
  name: string;
  description: string;
  rarity: rarity;
  cost: int;
}
