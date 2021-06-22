#[macro_export]
macro_rules! declare_consts {
    ($tp:ident, $name:ident = $value:expr $(,)* ) => {
        pub const $name: $tp = $value;
    };

    ($tp:ident, $name:ident = $value:expr, $($names:ident),+ $(,)*) => {
        declare_consts!($tp, $name = $value);
        declare_consts!($tp, $($names = $name + 1),+);
    };

    ($tp:ident, $name:ident = $value:expr, $($names:ident = $values:expr),+ $(,)*) => {
        declare_consts!($tp, $name = $value);
        declare_consts!($tp, $($names = $name + 1),+);
    };
}
