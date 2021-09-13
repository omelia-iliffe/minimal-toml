use serde::{Deserialize, Serialize};
use rand::Rng;

#[test]
fn array1() {
    let mut vec: heapless::Vec<u8, 8> = heapless::Vec::new();
    for _ in 0..8 {
        crate::expect_with_toml_rs(&vec);
        vec.push(rand::thread_rng().gen()).unwrap();
    }
    crate::expect_with_toml_rs(&vec);
}

#[test]
fn array2() {
    #[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
    struct Test {
        a: u32,
        b: bool,
        c: heapless::String<12>,
    }
    let mut vec: heapless::Vec<Test, 16> = heapless::Vec::new();
    for _ in 0..16 {
        crate::expect_with_toml_rs(&vec);
        let mut s: heapless::String<12> = heapless::String::new();
        let max = rand::thread_rng().gen_range(0..10);
        for _ in 0..max {
            s.push(rand::thread_rng().gen::<u8>() as char).unwrap();
        }
        let t = Test {
            a: rand::thread_rng().gen(),
            b: rand::thread_rng().gen(),
            c: s,
        };
        vec.push(t).unwrap();
    }
    crate::expect_with_toml_rs(&vec);
}
