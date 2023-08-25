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

//! # VelosiParser -- Parse Tree Parameters
//!
//! This module defines the parameter nodes for the velosiraptor parse tree.

// used standard library functionality
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

// use crate functionality
use super::{VelosiParseTreeIdentifier, VelosiParseTreeType};
use crate::VelosiTokenStream;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Parameters
///////////////////////////////////////////////////////////////////////////////////////////////////

/// A parameter definition within the methods, unit, or quantifier context
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeParam {
    /// the name of the parameter
    pub name: VelosiParseTreeIdentifier,
    /// the type of the param
    pub ptype: VelosiParseTreeType,
    /// the location of the entire import clause
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeParam {
    /// constructs a new parameter node
    pub fn with_loc(
        name: VelosiParseTreeIdentifier,
        ptype: VelosiParseTreeType,
        loc: VelosiTokenStream,
    ) -> Self {
        VelosiParseTreeParam { name, ptype, loc }
    }

    /// constructs a new parameter node with default location
    pub fn new(name: VelosiParseTreeIdentifier, ptype: VelosiParseTreeType) -> Self {
        Self::with_loc(name, ptype, VelosiTokenStream::default())
    }
}

/// Implementation of the [Display] trait for the [VelosiParseTreeParam] struct
impl Display for VelosiParseTreeParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}: {}", self.name, self.ptype)
    }
}

/// Implementation of the [Debug] trait for the [VelosiParseTreeParam] struct
impl Debug for VelosiParseTreeParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(self, f)
    }
}
