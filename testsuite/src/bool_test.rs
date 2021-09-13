use serde::{Deserialize, Serialize};

#[test]
fn bool1() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Test {
        a: bool,
        b: bool,
    }

    crate::expect_with_toml_rs(&Test { a: true, b: false });
}
