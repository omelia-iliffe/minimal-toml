#[cfg(test)]
mod tests {

    use serde::{Deserialize, Serialize};

    #[test]
    fn enum1() {
        #[derive(Serialize, Deserialize, PartialEq, Debug)]
        enum E {
            Unit,
            //We would support these enum kinds, but rust toml doesnt so we dont bother
            //Newtype(u32),
            //Tuple(u32, u32),
            //Struct { a: u32 },
        }

        crate::expect_with_toml_rs(&E::Unit);
    }
}
