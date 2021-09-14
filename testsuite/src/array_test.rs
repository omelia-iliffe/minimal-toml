use rand::Rng;
use serde::{Deserialize, Serialize};

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

    #[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
    struct A {
        stages: heapless::Vec<Test, 16>,
    }
    let mut a = A {
        stages: heapless::Vec::new(),
    };
    for _ in 0..16 {
        crate::expect_with_toml_rs(&a);
        let mut s: heapless::String<12> = heapless::String::new();
        let max = rand::thread_rng().gen_range(0..10);
        for _ in 0..max {
            let u: u8 = rand::thread_rng().gen_range(32..127);
            s.push(u as char).unwrap();
        }
        let t = Test {
            a: rand::thread_rng().gen(),
            b: rand::thread_rng().gen(),
            c: s,
        };
        a.stages.push(t).unwrap();
    }
    crate::expect_with_toml_rs(&a);
}
