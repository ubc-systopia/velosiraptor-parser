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

//! # VelosiParser -- Parse Tree Interface
//!
//! This module defines the interface nodes of the VelosiRaptor parse tree.

// used standard library functionality
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

// used crate functionality
use super::{
    VelosiParseTreeExpr, VelosiParseTreeFieldSlice, VelosiParseTreeIdentifier, VelosiParseTreeParam,
};
use crate::VelosiTokenStream;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Interface Actions
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an action on an interface field
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeInterfaceAction {
    /// source expression of the data movement
    pub src: VelosiParseTreeExpr,
    /// destination expression of the data movement
    pub dst: VelosiParseTreeExpr,
    /// location of the action in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeInterfaceAction {
    pub fn with_loc(
        src: VelosiParseTreeExpr,
        dst: VelosiParseTreeExpr,
        loc: VelosiTokenStream,
    ) -> Self {
        Self { src, dst, loc }
    }

    pub fn new(src: VelosiParseTreeExpr, dst: VelosiParseTreeExpr) -> Self {
        Self::with_loc(src, dst, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeInterfaceAction]
impl Display for VelosiParseTreeInterfaceAction {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{} -> {}", self.src, self.dst)
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterfaceAction]
impl Debug for VelosiParseTreeInterfaceAction {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

/// Represents a set of actions for an interface field
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeInterfaceActions {
    /// actions of the field
    pub actions: Vec<VelosiParseTreeInterfaceAction>,
    /// location of the field actions in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeInterfaceActions {
    /// constructs a new interface actions block
    pub fn with_loc(actions: Vec<VelosiParseTreeInterfaceAction>, loc: VelosiTokenStream) -> Self {
        Self { actions, loc }
    }

    /// constructs a new interface actions block with default location
    pub fn new(actions: Vec<VelosiParseTreeInterfaceAction>) -> Self {
        Self::with_loc(actions, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeInterfaceActions]
impl Display for VelosiParseTreeInterfaceActions {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        for a in &self.actions {
            writeln!(f, "{a};")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterfaceActions]
impl Debug for VelosiParseTreeInterfaceActions {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Field Layout
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents the layout of the field
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeInterfaceLayout {
    /// slices of the bit field layout
    pub slices: Vec<VelosiParseTreeFieldSlice>,
    /// location of the layout in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeInterfaceLayout {
    /// constructs a new interface field layout
    pub fn with_loc(slices: Vec<VelosiParseTreeFieldSlice>, loc: VelosiTokenStream) -> Self {
        Self { slices, loc }
    }

    /// constructs a new interface field layout with default location
    pub fn new(actions: Vec<VelosiParseTreeFieldSlice>) -> Self {
        Self::with_loc(actions, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeInterfaceLayout]
impl Display for VelosiParseTreeInterfaceLayout {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        for a in &self.slices {
            writeln!(f, "{a},")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterfaceLayout]
impl Debug for VelosiParseTreeInterfaceLayout {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Interface Field Node
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a node in the interface field definition
#[derive(PartialEq, Eq, Clone)]
pub enum VelosiParseTreeInterfaceFieldNode {
    Layout(VelosiParseTreeInterfaceLayout),
    ReadActions(VelosiParseTreeInterfaceActions),
    WriteActions(VelosiParseTreeInterfaceActions),
    // ReadWriteActions(VelosiParseTreeInterfaceActions),
}

impl Display for VelosiParseTreeInterfaceFieldNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            VelosiParseTreeInterfaceFieldNode::Layout(s) => {
                writeln!(f, "Layout {{")?;
                let formatted = format!("{s}");
                for l in formatted.lines() {
                    writeln!(f, "  {l}")?;
                }
                write!(f, "}}")?;
            }
            VelosiParseTreeInterfaceFieldNode::ReadActions(a) => {
                writeln!(f, "ReadActions {{")?;
                let formatted = format!("{a}");
                for l in formatted.lines() {
                    writeln!(f, "  {l}")?;
                }
                write!(f, "}}")?;
            }
            VelosiParseTreeInterfaceFieldNode::WriteActions(a) => {
                writeln!(f, "WriteActions {{")?;
                let formatted = format!("{a}");
                for l in formatted.lines() {
                    writeln!(f, "  {l}")?;
                }
                write!(f, "}}")?;
            }
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterfaceFieldNode]
impl Debug for VelosiParseTreeInterfaceFieldNode {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            VelosiParseTreeInterfaceFieldNode::Layout(s) => {
                writeln!(f, "// {}", s.loc)?;
            }
            VelosiParseTreeInterfaceFieldNode::ReadActions(a) => {
                writeln!(f, "// {}", a.loc)?;
            }
            VelosiParseTreeInterfaceFieldNode::WriteActions(a) => {
                writeln!(f, "// {}", a.loc)?;
            }
        }
        Display::fmt(&self, f)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Interface Memory Field
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an memory interface field
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeInterfaceFieldMemory {
    /// identifier of the field
    pub name: VelosiParseTreeIdentifier,
    /// identifer of the base of the memory region
    pub base: VelosiParseTreeIdentifier,
    /// offset into the memory region
    pub offset: u64,
    /// size of the field in bytes
    pub size: u64,
    /// nodes of the interface field
    pub nodes: Vec<VelosiParseTreeInterfaceFieldNode>,
    /// location of the interface field
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeInterfaceFieldMemory {
    /// constructs a new interface memory field
    pub fn with_loc(
        name: VelosiParseTreeIdentifier,
        base: VelosiParseTreeIdentifier,
        offset: u64,
        size: u64,
        nodes: Vec<VelosiParseTreeInterfaceFieldNode>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            name,
            base,
            offset,
            size,
            nodes,
            loc,
        }
    }

    /// constructs a new interface memory field with default location
    pub fn new(
        name: VelosiParseTreeIdentifier,
        base: VelosiParseTreeIdentifier,
        offset: u64,
        size: u64,
        nodes: Vec<VelosiParseTreeInterfaceFieldNode>,
    ) -> Self {
        Self::with_loc(
            name,
            base,
            offset,
            size,
            nodes,
            VelosiTokenStream::default(),
        )
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterfaceFieldMemory]
impl Display for VelosiParseTreeInterfaceFieldMemory {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "mem {} [ ", self.name)?;
        write!(f, "{}, {}, ", self.base, self.offset)?;
        write!(f, "{} ]", self.size)?;
        if !self.nodes.is_empty() {
            writeln!(f, " {{")?;
            for node in &self.nodes {
                let formatted = format!("{node}");
                for (i, l) in formatted.lines().enumerate() {
                    if i > 0 {
                        writeln!(f)?;
                    }
                    write!(f, "  {l}")?;
                }
                writeln!(f, ",")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterfaceFieldMemory]
impl Debug for VelosiParseTreeInterfaceFieldMemory {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Interface MMIO Field
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an interface mmio field
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeInterfaceFieldMmio {
    /// identifer of the field
    pub name: VelosiParseTreeIdentifier,
    /// address of the mmio region
    pub base: VelosiParseTreeIdentifier,
    /// offset of the mmio field into the base region
    pub offset: u64,
    /// size of the interface field in bytes
    pub size: u64,
    /// nodes of the interface filed
    pub nodes: Vec<VelosiParseTreeInterfaceFieldNode>,
    /// location of the mmio field in memory
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeInterfaceFieldMmio {
    /// constructs a new mmio field
    pub fn with_loc(
        name: VelosiParseTreeIdentifier,
        base: VelosiParseTreeIdentifier,
        offset: u64,
        size: u64,
        nodes: Vec<VelosiParseTreeInterfaceFieldNode>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            name,
            base,
            offset,
            size,
            nodes,
            loc,
        }
    }

    /// constructs a new mmio field with default location
    pub fn new(
        name: VelosiParseTreeIdentifier,
        base: VelosiParseTreeIdentifier,
        offset: u64,
        size: u64,
        nodes: Vec<VelosiParseTreeInterfaceFieldNode>,
    ) -> Self {
        Self::with_loc(
            name,
            base,
            offset,
            size,
            nodes,
            VelosiTokenStream::default(),
        )
    }
}

/// Implementation of [Display] for [VelosiParseTreeInterfaceFieldMmio]
impl Display for VelosiParseTreeInterfaceFieldMmio {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "mmio {} [ ", self.name)?;
        write!(f, "{}, {}, ", self.base, self.offset)?;
        write!(f, "{} ]", self.size)?;
        if !self.nodes.is_empty() {
            writeln!(f, " {{")?;
            for node in &self.nodes {
                let formatted = format!("{node}");
                for (i, l) in formatted.lines().enumerate() {
                    if i > 0 {
                        writeln!(f)?;
                    }
                    write!(f, "  {l}")?;
                }
                writeln!(f, ",")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterfaceFieldMmio]
impl Debug for VelosiParseTreeInterfaceFieldMmio {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Interface Register Field
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a register interface field
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeInterfaceFieldRegister {
    /// the identifer of the field
    pub name: VelosiParseTreeIdentifier,
    /// the size of the field in bytes
    pub size: u64,
    /// nodes of the field
    pub nodes: Vec<VelosiParseTreeInterfaceFieldNode>,
    /// location of the field in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeInterfaceFieldRegister {
    /// constructs a new register interface field
    pub fn with_loc(
        name: VelosiParseTreeIdentifier,
        size: u64,
        nodes: Vec<VelosiParseTreeInterfaceFieldNode>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            name,
            size,
            nodes,
            loc,
        }
    }

    /// constructs a new register interface field with default location
    pub fn new(
        name: VelosiParseTreeIdentifier,
        size: u64,
        nodes: Vec<VelosiParseTreeInterfaceFieldNode>,
    ) -> Self {
        Self::with_loc(name, size, nodes, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeInterfaceFieldRegister]
impl Display for VelosiParseTreeInterfaceFieldRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "reg {} [ ", self.name)?;
        write!(f, "{} ]", self.size)?;
        if !self.nodes.is_empty() {
            writeln!(f, " {{")?;
            for node in &self.nodes {
                let formatted = format!("{node}");
                for (i, l) in formatted.lines().enumerate() {
                    if i > 0 {
                        writeln!(f)?;
                    }
                    write!(f, "  {l}")?;
                }
                writeln!(f, ",")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterfaceFieldRegister]
impl Debug for VelosiParseTreeInterfaceFieldRegister {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Interface Fields
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a component of the interface, either a register or a memory location
#[derive(PartialEq, Eq, Clone)]
pub enum VelosiParseTreeInterfaceField {
    Memory(VelosiParseTreeInterfaceFieldMemory),
    Mmio(VelosiParseTreeInterfaceFieldMmio),
    Register(VelosiParseTreeInterfaceFieldRegister),
}

/// Implementation of [Display] for [VelosiParseTreeInterfaceField]
impl Display for VelosiParseTreeInterfaceField {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use VelosiParseTreeInterfaceField::*;
        match self {
            Memory(field) => Display::fmt(field, f),
            Mmio(field) => Display::fmt(field, f),
            Register(field) => Display::fmt(field, f),
        }
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterfaceField]
impl Debug for VelosiParseTreeInterfaceField {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Interface
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents the parsed interfaced escription
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeInterface {
    /// parameters of the interface
    pub params: Vec<VelosiParseTreeParam>,
    /// the fields defined in the interface
    pub fields: Vec<VelosiParseTreeInterfaceField>,
    /// the position in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeInterface {
    /// Create a new state definition
    pub fn with_loc(
        params: Vec<VelosiParseTreeParam>,
        fields: Vec<VelosiParseTreeInterfaceField>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            params,
            fields,
            loc,
        }
    }

    /// Create a new state definition with default location
    pub fn new(
        params: Vec<VelosiParseTreeParam>,
        fields: Vec<VelosiParseTreeInterfaceField>,
    ) -> Self {
        Self::with_loc(params, fields, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeInterface]
impl Display for VelosiParseTreeInterface {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "interface(")?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
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
        write!(f, "}}")
    }
}

/// Implementation of [Debug] for [VelosiParseTreeInterface]
impl Debug for VelosiParseTreeInterface {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}
