
mod common;
use serde::Deserialize;

#[test]
fn bool1() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Test {
        a: bool,
        b: bool,
    }

    common::expect_output(r#"
a = true
b = false
    "#,
        Test {
            a: true,
            b: false,
        },
    );
}
