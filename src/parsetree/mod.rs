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

/// parse tree modules
mod consts;
mod decorator;
mod expr;
mod field;
mod identifier;
mod import;
mod interface;
mod map;
mod method;
mod params;
mod state;
mod types;
mod unit;

/// export of the parse tree nodes
pub use consts::VelosiParseTreeConstDef;
pub use decorator::VelosiParseTreeProperty;
pub use expr::{
    VelosiParseTreeBinOp, VelosiParseTreeBinOpExpr, VelosiParseTreeBoolLiteral,
    VelosiParseTreeElementExpr, VelosiParseTreeExpr, VelosiParseTreeFnCallExpr,
    VelosiParseTreeIdentifierLiteral, VelosiParseTreeIfElseExpr, VelosiParseTreeNumLiteral,
    VelosiParseTreeQuantifier, VelosiParseTreeQuantifierExpr, VelosiParseTreeRangeExpr,
    VelosiParseTreeSliceExpr, VelosiParseTreeUnOp, VelosiParseTreeUnOpExpr,
};
pub use field::{VelosiParseTreeField, VelosiParseTreeFieldSlice};
pub use identifier::VelosiParseTreeIdentifier;
pub use import::VelosiParseTreeImport;
pub use interface::{
    VelosiParseTreeInterface, VelosiParseTreeInterfaceAction, VelosiParseTreeInterfaceActions,
    VelosiParseTreeInterfaceField, VelosiParseTreeInterfaceFieldMemory,
    VelosiParseTreeInterfaceFieldMmio, VelosiParseTreeInterfaceFieldNode,
    VelosiParseTreeInterfaceFieldRegister, VelosiParseTreeInterfaceLayout,
};
pub use map::{
    VelosiParseTreeMap, VelosiParseTreeMapElement, VelosiParseTreeMapExplicit,
    VelosiParseTreeMapListComp,
};
pub use method::VelosiParseTreeMethod;
pub use params::VelosiParseTreeParam;
pub use state::{
    VelosiParseTreeState, VelosiParseTreeStateField, VelosiParseTreeStateFieldMemory,
    VelosiParseTreeStateFieldRegister,
};
pub use types::{VelosiParseTreeExternType, VelosiParseTreeType, VelosiParseTreeTypeInfo};
pub use unit::{
    VelosiParseTreeEnum, VelosiParseTreeFlags, VelosiParseTreeUnit, VelosiParseTreeUnitDef,
    VelosiParseTreeUnitNode,
};

///////////////////////////////////////////////////////////////////////////////////////////////////
// Parse Tree Nodes
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents parse tree nodes
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VelosiParseTreeContextNode {
    Const(VelosiParseTreeConstDef),
    Import(VelosiParseTreeImport),
    Unit(VelosiParseTreeUnit),
    Flags(VelosiParseTreeFlags),
    Type(VelosiParseTreeExternType),
}

/// Implement [Display] for [VelosiParseTree]
impl Display for VelosiParseTreeContextNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use VelosiParseTreeContextNode::*;
        match self {
            Const(s) => Display::fmt(&s, f),
            Import(s) => Display::fmt(&s, f),
            Unit(s) => Display::fmt(&s, f),
            Flags(s) => Display::fmt(&s, f),
            Type(s) => Display::fmt(&s, f),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Parse Tree Root Node
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents the parse tree root node
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct VelosiParseTree {
    /// List of nodes in the current parse tree context
    pub nodes: Vec<VelosiParseTreeContextNode>,
    /// The current node context
    pub context: Option<String>,
}

impl VelosiParseTree {
    /// creates a new parse tree from the parsed nodes
    pub fn new(nodes: Vec<VelosiParseTreeContextNode>) -> Self {
        Self {
            nodes,
            context: None,
        }
    }

    /// creates a new empty tree
    pub fn empty() -> Self {
        Self {
            nodes: Vec::new(),
            context: None,
        }
    }

    /// merges two parset rees together
    pub fn merge(&mut self, other: Self) {
        self.nodes.extend(other.nodes);
    }

    /// filters the nodes to remove imports
    pub fn filter_imports(&mut self) {
        self.nodes
            .retain(|n| !matches!(n, VelosiParseTreeContextNode::Import(_)));
    }

    /// sets the context string of the tree
    pub fn set_context(&mut self, c: String) {
        self.context = Some(c);
    }

    /// obtains an interator to the import nodes
    pub fn imports(&self) -> impl Iterator<Item = &VelosiParseTreeImport> {
        self.nodes.iter().filter_map(|n| match n {
            VelosiParseTreeContextNode::Import(i) => Some(i),
            _ => None,
        })
    }
}

/// Implement [Display] for [VelosiParseTree]
impl Display for VelosiParseTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if let Some(c) = &self.context {
            writeln!(f, "VelosiParseTree({c})")?;
        } else {
            writeln!(f, "VelosiParseTree($buf)")?;
        }

        writeln!(f, "---------------------------------------------")?;
        for n in &self.nodes {
            writeln!(f, "{n}\n")?;
        }
        writeln!(f, "---------------------------------------------")?;
        Ok(())
    }
}
