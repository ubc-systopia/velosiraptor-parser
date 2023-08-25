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

//! # VelosiParser -- Parse Tree Map
//!
//! This module defines the StaticMap of a Unit
//!

// used standard library functionality
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

// use crate functionality
use crate::parsetree::{
    VelosiParseTreeExpr, VelosiParseTreeFnCallExpr, VelosiParseTreeIdentifier,
    VelosiParseTreeMethodProperty, VelosiParseTreeRangeExpr,
};
use crate::VelosiTokenStream;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Static Map Definitions
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a possible map definitions
#[derive(PartialEq, Eq, Clone)]
pub enum VelosiParseTreeMap {
    /// map defined as a list comprehension
    ListComp(Box<VelosiParseTreeMapListComp>),
    /// map defined by explicitly listing all nodes
    Explicit(Box<VelosiParseTreeMapExplicit>),
}

impl VelosiParseTreeMap {
    /// Returns the position of the node in the source code
    pub fn loc(&self) -> &VelosiTokenStream {
        match self {
            VelosiParseTreeMap::ListComp(node) => &node.loc,
            VelosiParseTreeMap::Explicit(node) => &node.loc,
        }
    }
}
/// Implementation of [Display] for [VelosiParseTreeMap]
impl Display for VelosiParseTreeMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            VelosiParseTreeMap::ListComp(map) => Display::fmt(map, f),
            VelosiParseTreeMap::Explicit(map) => Display::fmt(map, f),
        }
    }
}

/// Implementation of [Debug] for [VelosiParseTreeMap]
impl Debug for VelosiParseTreeMap {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Explicit Map
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents possible nodes in the unit definitions
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeMapExplicit {
    /// list of entries of the map
    pub entries: Vec<VelosiParseTreeMapElement>,
    /// location of the explicit map in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeMapExplicit {
    /// creates a new explicit list map
    pub fn with_loc(entries: Vec<VelosiParseTreeMapElement>, loc: VelosiTokenStream) -> Self {
        Self { entries, loc }
    }

    /// creates a new explicit list map with default location
    pub fn new(entries: Vec<VelosiParseTreeMapElement>) -> Self {
        Self::with_loc(entries, VelosiTokenStream::default())
    }
}
/// Implementation of [Display] for [VelosiParseTreeMapExplicit]
impl Display for VelosiParseTreeMapExplicit {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "maps [ ")?;
        for (i, e) in self.entries.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{e}")?;
        }

        write!(f, " ]")
    }
}

/// Implementation of [Debug] for [VelosiParseTreeMapExplicit]
impl Debug for VelosiParseTreeMapExplicit {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// List Comprehension Map
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a list comprehension map
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeMapListComp {
    /// dscription of the element
    pub elm: VelosiParseTreeMapElement,
    /// variable to evaluate
    pub var: VelosiParseTreeIdentifier,
    /// range defining the entries
    pub range: VelosiParseTreeRangeExpr,
    /// properties of the map
    pub properties: Vec<VelosiParseTreeMethodProperty>,
    /// location of the list comprehension map in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeMapListComp {
    /// constructs a new list comprehension map
    pub fn with_loc(
        elm: VelosiParseTreeMapElement,
        var: VelosiParseTreeIdentifier,
        range: VelosiParseTreeRangeExpr,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            elm,
            var,
            range,
            loc,
            properties: Vec::new(),
        }
    }

    /// constructs a new list comprehension map with default location
    pub fn new(
        elm: VelosiParseTreeMapElement,
        var: VelosiParseTreeIdentifier,
        range: VelosiParseTreeRangeExpr,
    ) -> Self {
        Self::with_loc(elm, var, range, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeMapListComp]
impl Display for VelosiParseTreeMapListComp {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(
            f,
            "maps [ {} for {} in {} ]",
            self.elm, self.var, self.range
        )
    }
}

/// Implementation of [Debug] for [VelosiParseTreeMapListComp]
impl Debug for VelosiParseTreeMapListComp {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Map Element
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an element in the static map definition
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeMapElement {
    /// expression defining the source address range
    pub src: Option<VelosiParseTreeRangeExpr>,
    /// destinaton unit of the map with parameters
    pub dst: VelosiParseTreeFnCallExpr,
    /// offset into the destination unit
    pub offset: Option<VelosiParseTreeExpr>,
    /// location of the element in the source tree
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeMapElement {
    /// constructs a new static map element
    pub fn wiht_loc(
        src: Option<VelosiParseTreeRangeExpr>,
        dst: VelosiParseTreeFnCallExpr,
        offset: Option<VelosiParseTreeExpr>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            src,
            dst,
            offset,
            loc,
        }
    }

    /// constructs a new static map element with default location
    pub fn new(
        src: Option<VelosiParseTreeRangeExpr>,
        dst: VelosiParseTreeFnCallExpr,
        offset: Option<VelosiParseTreeExpr>,
    ) -> Self {
        Self::wiht_loc(src, dst, offset, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeMapElement]
impl Display for VelosiParseTreeMapElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if let Some(src) = &self.src {
            write!(f, "{src} => ")?;
        }
        write!(f, "{}", self.dst)?;
        if let Some(offset) = &self.offset {
            write!(f, " @ {offset}")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeMapElement]
impl Debug for VelosiParseTreeMapElement {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}
