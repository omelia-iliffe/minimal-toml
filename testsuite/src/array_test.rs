#[cfg(test)]
mod tests {
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
        for _ in 0..100 {
            let mut a = A {
                stages: heapless::Vec::new(),
            };
            for _ in 0..16 {
                crate::expect_with_toml_rs(&a);
                let mut s: heapless::String<12> = heapless::String::new();
                let max = rand::thread_rng().gen_range(0..10);
                for _ in 0..max {
                    let mut u: u8 = rand::thread_rng().gen_range(32..127);
                    if u == b'\\' || u == b'"' {
                        //Prevent weird escape sequences. We don't support escape sequences anyway
                        u = b'a';
                    }
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
    }

    #[test]
    fn array3() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct A {
            arrays: heapless::Vec<Sub, 16>,
        }

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Sub {
            sub2: heapless::Vec<Test, 4>,
            sub: heapless::Vec<Test, 4>,
        }

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Test {
            a: u32,
            BIG_FLOAT: f64,
        }

        for _ in 0..100 {
            let mut a = A {
                arrays: heapless::Vec::new(),
            };
            for _ in 0..16 {
                crate::expect_with_toml_rs(&a);
                let sub = Sub {
                    sub: heapless::Vec::new(),
                    sub2: heapless::Vec::new(),
                };
                a.arrays.push(sub).unwrap();
                crate::expect_with_toml_rs(&a);
                for _ in 0..4 {
                    let i = a.arrays.len() - 1;
                    let sub = a.arrays.get_mut(i).unwrap();
                    sub.sub
                        .push(Test {
                            a: rand::thread_rng().gen(),
                            BIG_FLOAT: rand::thread_rng().gen(),
                        })
                        .unwrap();
                    crate::expect_with_toml_rs(&a);
                }
            }
        }
    }
}
