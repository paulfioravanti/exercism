let twoFer = name => {
  let determineName = (name: option(string)): string => {
    switch (name) {
    | None => "you"
    | Some(name) => name
    }
  };

  "One for " ++ determineName(name) ++ ", one for me."
};
