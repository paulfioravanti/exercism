#[derive(Debug)]
pub struct Duration {
    seconds: u64,
}

impl From<u64> for Duration {
    fn from(seconds: u64) -> Self {
        Self { seconds }
    }
}

pub trait Planet {
    const ORBITAL_FACTOR: f64;

    fn years_during(Duration { seconds }: &Duration) -> f64 {
        *seconds as f64 / Earth::ORBITAL_PERIOD / Self::ORBITAL_FACTOR
    }
}

macro_rules! planets {
    ( $( $planet:ident; $orbital_factor:expr ),* ) => {
        $(
            pub struct $planet;
            impl Planet for $planet {
                const ORBITAL_FACTOR: f64 = $orbital_factor;
            }
        )*
    }
}

planets! [
    Mercury; 0.240_846_7,
    Venus; 0.615_197_26,
    Earth; 1.0,
    Mars; 1.880_815_8,
    Jupiter; 11.862_615,
    Saturn; 29.447_498,
    Uranus; 84.016_846,
    Neptune; 164.79132
];

impl Earth {
    const ORBITAL_PERIOD: f64 = 31_557_600.0;
}
