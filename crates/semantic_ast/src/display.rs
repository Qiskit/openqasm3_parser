// Unused at the moment.
// This intent here is to print something like source code.
// But this involves a ton of boiler plate.

use std::fmt;
use crate::ast::*;

impl fmt::Display for Include {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "include \"{}\"",  self.file_path())
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Include(include) => {
                write!(f, "{}", include);
            }
            _ => ()
        }
        write!(f, ";")
    }
}
