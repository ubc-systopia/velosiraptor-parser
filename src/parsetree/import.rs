// VelosiParser -- a parser for the Velosiraptor Language
//
//
// MIT License
//
// Copyright (c) 2021, 2022 Systopia Lab, Computer Science, University of British Columbia
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

//! # VelosiParser -- Parse Tree
//!
//! This module defines the parse tree for the Velosiraptor language.

// used standard library functionality
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

// use crate functionality
use crate::parsetree::VelosiParseTreeIdentifier;
use crate::VelosiTokenStream;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Imports
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Import clause in the root context
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeImport(pub VelosiParseTreeIdentifier);

impl VelosiParseTreeImport {
    /// obtains the name of the imported file
    pub fn name(&self) -> &str {
        &self.0.name
    }

    /// obtains the location of the
    pub fn loc(&self) -> &VelosiTokenStream {
        &self.0.loc
    }
}

/// Implementation of [Display] for [VelosiParseTreeImport]
impl Display for VelosiParseTreeImport {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "import {};", self.0.name)
    }
}

/// Implementation of [Debug] for [VelosiParseTreeImport]
impl Debug for VelosiParseTreeImport {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(self, f)
    }
}
