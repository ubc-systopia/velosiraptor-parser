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

//! # VelosiParser -- Parse Tree Nodes for State
//!
//! Parse tree nodes for state information

// used standard library functionality
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

// used crate functionality
use crate::parsetree::{
    VelosiParseTreeFieldSlice, VelosiParseTreeIdentifier, VelosiParseTreeParam,
};
use crate::VelosiTokenStream;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Memory State Fields
////////////////////////////////////////////////////////////////////////////////////////////////////

/// represents a memory state field
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeStateFieldMemory {
    /// identifier of the field
    pub name: VelosiParseTreeIdentifier,
    /// memory region the field is located in
    pub base: VelosiParseTreeIdentifier,
    /// offset of the memory field within the memory region
    pub offset: u64,
    /// size of the field in bytes
    pub size: u64,
    /// bit slice layout of the memory field
    pub layout: Vec<VelosiParseTreeFieldSlice>,
    /// location of the state field in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeStateFieldMemory {
    /// constructs a new memory state filed
    pub fn with_loc(
        name: VelosiParseTreeIdentifier,
        base: VelosiParseTreeIdentifier,
        offset: u64,
        size: u64,
        layout: Vec<VelosiParseTreeFieldSlice>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            name,
            base,
            offset,
            size,
            layout,
            loc,
        }
    }

    /// constructs a new memory state filed with the default location
    pub fn new(
        name: VelosiParseTreeIdentifier,
        base: VelosiParseTreeIdentifier,
        offset: u64,
        size: u64,
        layout: Vec<VelosiParseTreeFieldSlice>,
    ) -> Self {
        Self::with_loc(
            name,
            base,
            offset,
            size,
            layout,
            VelosiTokenStream::default(),
        )
    }
}

/// Implementation of [Display] for [VelosiParseTreeStateFieldMemory]
impl Display for VelosiParseTreeStateFieldMemory {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "mem {} [ ", self.name)?;
        write!(f, "{}, {}, ", self.base, self.offset)?;
        write!(f, "{} ]", self.size)?;
        if !self.layout.is_empty() {
            writeln!(f, " {{")?;
            for slice in &self.layout {
                write!(f, "  ")?;
                Display::fmt(slice, f)?;
                writeln!(f, ",")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeStateFieldMemory]
impl Debug for VelosiParseTreeStateFieldMemory {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Register State Fields
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a register state field
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeStateFieldRegister {
    /// identifier of the state field
    pub name: VelosiParseTreeIdentifier,
    /// size of the state field in bytes
    pub size: u64,
    /// bit field layout of the state field
    pub layout: Vec<VelosiParseTreeFieldSlice>,
    /// location of the register state field in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeStateFieldRegister {
    /// creates a new register state field
    pub fn with_loc(
        name: VelosiParseTreeIdentifier,
        size: u64,
        layout: Vec<VelosiParseTreeFieldSlice>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            name,
            size,
            layout,
            loc,
        }
    }

    /// creates a new register state field with default location
    pub fn new(
        name: VelosiParseTreeIdentifier,
        size: u64,
        layout: Vec<VelosiParseTreeFieldSlice>,
    ) -> Self {
        Self::with_loc(name, size, layout, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeStateFieldRegister]
impl Display for VelosiParseTreeStateFieldRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "reg {} [", self.name)?;
        write!(f, " {} ]", self.size)?;
        if !self.layout.is_empty() {
            writeln!(f, " {{")?;
            for slice in &self.layout {
                write!(f, "  ")?;
                Display::fmt(slice, f)?;
                writeln!(f, ",")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeStateFieldRegister]
impl Debug for VelosiParseTreeStateFieldRegister {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// State Fields
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a component of the state, either a register or a memory location
#[derive(PartialEq, Eq, Clone)]
pub enum VelosiParseTreeStateField {
    Memory(VelosiParseTreeStateFieldMemory),
    Register(VelosiParseTreeStateFieldRegister),
}

/// Implementation of [Display] for [VelosiParseTreeStateField]
impl Display for VelosiParseTreeStateField {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use VelosiParseTreeStateField::*;
        match self {
            Memory(field) => Display::fmt(field, f),
            Register(field) => Display::fmt(field, f),
        }
    }
}

/// Implementation of [Debug] for [VelosiParseTreeState]
impl Debug for VelosiParseTreeStateField {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self, f)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// State
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents the parsed state description
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeState {
    /// parameters of the state
    pub params: Vec<VelosiParseTreeParam>,
    /// the fields defined in teh state
    pub fields: Vec<VelosiParseTreeStateField>,
    /// the position in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeState {
    /// Create a new state definition
    pub fn with_loc(
        params: Vec<VelosiParseTreeParam>,
        fields: Vec<VelosiParseTreeStateField>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            params,
            fields,
            loc,
        }
    }

    /// Create a new state definition with default location
    pub fn new(params: Vec<VelosiParseTreeParam>, fields: Vec<VelosiParseTreeStateField>) -> Self {
        Self::with_loc(params, fields, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeState]
impl Display for VelosiParseTreeState {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "state(")?;
        for param in &self.params {
            Display::fmt(param, f)?;
        }
        writeln!(f, ") {{")?;
        for field in &self.fields {
            let formatted = format!("{}", field);
            for (i, line) in formatted.lines().enumerate() {
                if i > 0 {
                    writeln!(f)?;
                }
                write!(f, "  {line}")?;
            }
            writeln!(f, ",")?;
        }
        writeln!(f, "}}")
    }
}

/// Implementation of [Debug] for [VelosiParseTreeState]
impl Debug for VelosiParseTreeState {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}
