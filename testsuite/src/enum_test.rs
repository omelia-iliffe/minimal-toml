use serde::Deserialize;

#[test]
fn enum1() {
    #[derive(Deserialize, PartialEq, Debug)]
    enum E {
        Unit,
        Newtype(u32),
        Tuple(u32, u32),
        Struct { a: u32 },
    }

    crate::expect_output(r#"E = "Unit""#, E::Unit);
    crate::expect_output(r#"E = {"Newtype" = 1}"#, E::Newtype(1));
    crate::expect_output(r#"E = {"Tuple"=[1,2]}"#, E::Tuple(1, 2));
    crate::expect_output(r#"E = {"Struct"={"a"=1}}"#, E::Struct { a: 1 });
}
