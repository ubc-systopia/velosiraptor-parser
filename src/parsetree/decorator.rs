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

//! # VelosiParser -- Parse Tree Decorators / Properties
//!
//! Parse tree nodes for method definitions

// used standard library functionality
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

// used parsetree nodes
use crate::parsetree::VelosiParseTreeIdentifier;
use crate::VelosiTokenStream;

/// type defining a method property
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeProperty {
    /// identifier of the property
    pub ident: VelosiParseTreeIdentifier,
    /// parameters of the properties
    pub params: Vec<VelosiParseTreeIdentifier>,
    /// location of the property in the source file
    pub loc: VelosiTokenStream,
}

/// Implement the [Display] trait for the [VelosiParseTreeProperty] struct
impl Display for VelosiParseTreeProperty {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.ident)?;
        if !self.params.is_empty() {
            write!(f, "(")?;
            for (i, p) in self.params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", p)?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeProperty]
impl Debug for VelosiParseTreeProperty {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self, f)
    }
}
