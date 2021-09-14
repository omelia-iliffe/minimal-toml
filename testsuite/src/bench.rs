#[cfg(test)]
mod tests {

    use serde::{Deserialize, Serialize};

    //Test of struct used in production by null.black!
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    pub struct WgFlags<'a> {
        //pub address: IpNet,
        pub interface_name: &'a str,
        pub persistent_keep_alive: u16,
        pub dns: &'a str,
        pub f: f32,
        //pub handshake_check_interval: Duration,
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    pub struct ServerFlags<'a> {
        pub gateway_interface_name_override: Option<&'a str>,
        pub endpoint: Option<&'a str>,
        pub wg_listen_port: u16,
        pub disconnect_listen_port: u16,
        pub http_listen_port: u16,
        pub getip_listen_port: u16,
        pub disable_verify_server_pem: Option<bool>,
        pub disable_client_cert: Option<bool>,
        pub enforce_device_limits: bool,
        pub name: &'a str,
        pub flag: &'a str,
        pub is_staff_server: bool,
        pub flight: u8,
        pub torrenting_allowed: bool,
        pub persist_peers: bool,
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    pub struct GCFlags {
        pub initial_allocate_count: u32,
        pub allocate_step: u32,
        pub under_pressure_gc_count: u32,
        pub max_configs: u32,
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    pub struct Flags<'a> {
        //ff: f64,
        #[serde(borrow)]
        pub wg: WgFlags<'a>,
        #[serde(borrow)]
        pub server: ServerFlags<'a>,
        pub gc: GCFlags,
    }

    //The struct with these values is usually around 442 bytes
    const TEST_VALUE: Flags = Flags {
        //ff: 64.0009765625,
        wg: WgFlags {
            interface_name: "es0",
            persistent_keep_alive: 21,
            dns: "1.1.1.1",
            f: -75.8125,
        },
        server: ServerFlags {
            gateway_interface_name_override: None,
            endpoint: None,
            wg_listen_port: 51820,
            disconnect_listen_port: 6976,
            http_listen_port: 25566,
            getip_listen_port: 6977,
            disable_verify_server_pem: None,
            disable_client_cert: None,
            enforce_device_limits: true,
            name: "Frankfurt",
            flag: "DE",
            is_staff_server: false,
            flight: 0,
            torrenting_allowed: false,
            persist_peers: true,
        },
        gc: GCFlags {
            initial_allocate_count: 100,
            allocate_step: 25,
            under_pressure_gc_count: 10,
            max_configs: 1000,
        },
    };

    fn get_start_text() -> String {
        toml::to_string(&TEST_VALUE).expect("Failed to serialize Flags")
    }

    #[bench]
    fn other_toml_de(b: &mut test::Bencher) {
        let src = get_start_text();
        b.iter(|| {
            let f: Flags = toml::from_str(src.as_str()).unwrap();
            f
        });
    }

    #[bench]
    fn our_toml(b: &mut test::Bencher) {
        let src = get_start_text();
        b.iter(|| {
            let f: Flags = minimal_toml::from_str(src.as_str()).unwrap();
            f
        });
    }
}
