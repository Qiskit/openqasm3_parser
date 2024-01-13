// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

/// Return the name of the type of a value.
pub fn type_name_of<T>(_: T) -> &'static str {
    std::any::type_name::<T>()
}

/// Like dbg!, but print to stdout
#[macro_export]
macro_rules! dbg_stdout {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `println!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `println!`
    // will be malformed.
    () => {
        ::std::println!("[{}:{}]", ::std::file!(), ::std::line!())
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                ::std::println!("[{}:{}] {} = {:#?}",
                    ::std::file!(), ::std::line!(), ::std::stringify!($val), &tmp);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($(::std::dbg_stdout!($val)),+,)
    };
}
