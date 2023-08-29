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

//! # VelosiParser -- Parse Tree Methods
//!
//! Parse tree nodes for method definitions

// used standard library functionality
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

// used parsetree nodes
use crate::parsetree::{
    VelosiParseTreeExpr, VelosiParseTreeIdentifier, VelosiParseTreeParam, VelosiParseTreeType,
};
use crate::VelosiTokenStream;

/// type defining a method property
pub type VelosiParseTreeMethodProperty =
    (VelosiParseTreeIdentifier, Option<VelosiParseTreeIdentifier>);

/// Represents a method node
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeMethod {
    /// the name of the unit (identifier)
    pub name: VelosiParseTreeIdentifier,
    /// properties of the method
    pub properties: Vec<VelosiParseTreeMethodProperty>,
    /// whether this is an abstract method
    pub is_abstract: bool,
    /// whether this is a method to be synthesized
    pub is_synth: bool,
    /// whether the method is externally defined
    pub is_extern: bool,
    /// the unit parameters
    pub params: Vec<VelosiParseTreeParam>,
    /// the name of the derrived unit
    pub rettype: Option<VelosiParseTreeType>,
    /// the nodes defined in the parse tree
    pub requires: Vec<VelosiParseTreeExpr>,
    /// the nodes defined in the parse tree
    pub ensures: Vec<VelosiParseTreeExpr>,
    /// the body of the method
    pub body: Option<VelosiParseTreeExpr>,
    /// the position in the source tree where this unit is defined
    pub loc: VelosiTokenStream,
}

/// Implement the [Display] trait for the [VelosiParseTreeMethod] struct
impl Display for VelosiParseTreeMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if !self.properties.is_empty() {
            write!(f, "#[")?;
            for (i, prop) in self.properties.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }

                write!(f, "{}", prop.0.name)?;
                if let Some(param) = &prop.1 {
                    write!(f, "({})", param.name)?;
                }
            }
            writeln!(f, "]")?;
        }

        if self.is_extern {
            write!(f, "extern ")?;
        }

        if self.is_abstract {
            write!(f, "abstract ")?;
        }
        if self.is_synth {
            write!(f, "synth ")?;
        }
        write!(f, "fn {}(", self.name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{param}")?;
        }
        if let Some(rtype) = &self.rettype {
            write!(f, ") -> {rtype}")?;
        } else {
            write!(f, ")")?;
        }

        for require in self.requires.iter() {
            write!(f, "\n  requires {require}")?;
        }

        for ensure in self.ensures.iter() {
            write!(f, "\n  ensures {ensure}")?;
        }

        if let Some(body) = &self.body {
            writeln!(f, "\n{{\n  {body}\n}}")?;
        } else {
            writeln!(f)?;
        }

        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeMethod]
impl Debug for VelosiParseTreeMethod {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "// {}", self.loc)?;
        Display::fmt(&self, f)
    }
}
