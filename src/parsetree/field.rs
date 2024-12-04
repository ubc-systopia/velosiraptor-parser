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

//! # VelosiParser -- Parse Tree Fields
//!
//! This module defines the field nodes of the VelosiRaptor parse tree.

// used standard library functionality
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

// used crate functionality
use crate::parsetree::VelosiParseTreeIdentifier;
use crate::VelosiTokenStream;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Field Bitslices
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a bitslice in a field
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeFieldSlice {
    /// the start bit
    pub start: u64,
    // the end bit
    pub end: u64,
    /// name of the bitslice
    pub name: VelosiParseTreeIdentifier,
    /// location of the bit slice in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeFieldSlice {
    /// constructs a new bitslice of a field
    pub fn with_loc(
        start: u64,
        end: u64,
        name: VelosiParseTreeIdentifier,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            start,
            end,
            name,
            loc,
        }
    }

    /// constructs a new bitslice of a field with default location
    pub fn new(start: u64, end: u64, name: VelosiParseTreeIdentifier) -> Self {
        Self::with_loc(start, end, name, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeFieldSlice]
impl Display for VelosiParseTreeFieldSlice {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}..{} {}", self.start, self.end, self.name)
    }
}

/// Implementation of [Debug] for [VelosiParseTreeFieldSlice]
impl Debug for VelosiParseTreeFieldSlice {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Field
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents represents a field in the state
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeField {
    /// name of the field
    pub name: VelosiParseTreeIdentifier,
    /// offset information into the parameter
    pub offset: Option<(VelosiParseTreeIdentifier, u64)>,
    /// size of the field
    pub size: u64,
    /// bitslice layout
    pub layout: Vec<VelosiParseTreeFieldSlice>,
    /// location of the field in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeField {
    /// constructs a new field
    pub fn with_loc(
        name: VelosiParseTreeIdentifier,
        offset: Option<(VelosiParseTreeIdentifier, u64)>,
        size: u64,
        layout: Vec<VelosiParseTreeFieldSlice>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            name,
            offset,
            size,
            layout,
            loc,
        }
    }

    /// constructs a new field with default location
    pub fn new(
        name: VelosiParseTreeIdentifier,
        offset: Option<(VelosiParseTreeIdentifier, u64)>,
        size: u64,
        layout: Vec<VelosiParseTreeFieldSlice>,
    ) -> Self {
        Self::with_loc(name, offset, size, layout, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeField]
impl Display for VelosiParseTreeField {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{} [", self.name)?;
        if let Some((name, num)) = &self.offset {
            write!(f, "{name}, {num}, ")?;
        }
        write!(f, "{}", self.size)?;
        writeln!(f, "] {{")?;
        for slice in &self.layout {
            let formatted = format!("{slice}");
            for l in formatted.lines() {
                writeln!(f, "  {l},")?;
            }
        }
        write!(f, "    }}")
    }
}

/// Implementation of [Debug] for [VelosiParseTreeField]
impl Debug for VelosiParseTreeField {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}
