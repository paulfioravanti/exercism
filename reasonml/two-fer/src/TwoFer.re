let twoFer = name => {
  let companion: string =
    name
      ->Belt.Option.getWithDefault("you");

  "One for " ++ companion ++ ", one for me.";
};
