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

//! # VelosiParser -- Parse Tree Expressions
//!
//! This module defines the expression nodes for the parse tree

// used standard library functionality
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

// used crate functionality
use crate::VelosiTokenStream;

use crate::parsetree::{VelosiParseTreeIdentifier, VelosiParseTreeParam};

///////////////////////////////////////////////////////////////////////////////////////////////////
// Binary Operation Expressions
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an operator for a binary expression
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum VelosiParseTreeBinOp {
    // arithmetic operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    LShift,
    RShift,
    And,
    Xor,
    Or,
    // boolean operators
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Land,
    Lor,
    Implies,
}

/// Implementation of [Display] for [VelosiParseTreeBinOp]
impl Display for VelosiParseTreeBinOp {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        use VelosiParseTreeBinOp::*;
        match self {
            Plus => write!(format, "+"),
            Minus => write!(format, "-"),
            Multiply => write!(format, "*"),
            Divide => write!(format, "/"),
            Modulo => write!(format, "%"),
            LShift => write!(format, "<<"),
            RShift => write!(format, ">>"),
            And => write!(format, "&"),
            Xor => write!(format, "^"),
            Or => write!(format, "|"),
            Eq => write!(format, "=="),
            Ne => write!(format, "!="),
            Lt => write!(format, "<"),
            Gt => write!(format, ">"),
            Le => write!(format, "<="),
            Ge => write!(format, ">="),
            Land => write!(format, "&&"),
            Lor => write!(format, "||"),
            Implies => write!(format, "==>"),
        }
    }
}

/// Implementation of the [Display] trait for the [VelosiParseTreeConstDef] struct
impl Debug for VelosiParseTreeBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(&self, f)
    }
}

/// Represents a binary operation `expr <op> expr`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeBinOpExpr {
    /// the left hand side of the expression
    pub lhs: Box<VelosiParseTreeExpr>,
    /// the operator of the binary expression
    pub op: VelosiParseTreeBinOp,
    /// the right hand side of the expression
    pub rhs: Box<VelosiParseTreeExpr>,
    /// the source location covering the binary expression
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeBinOpExpr {
    /// constructs a new binary operation expression with the given operators
    pub fn with_loc(
        lhs: VelosiParseTreeExpr,
        op: VelosiParseTreeBinOp,
        rhs: VelosiParseTreeExpr,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
            loc,
        }
    }

    /// constructs a new binary operation expression with the given operators and default location
    pub fn new(
        lhs: VelosiParseTreeExpr,
        op: VelosiParseTreeBinOp,
        rhs: VelosiParseTreeExpr,
    ) -> Self {
        Self::with_loc(lhs, op, rhs, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeBinOpExpr]
impl Display for VelosiParseTreeBinOpExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        if self.lhs.needs_paren() {
            write!(format, "({})", self.lhs)?;
        } else {
            write!(format, "{}", self.lhs)?;
        }
        write!(format, " {} ", self.op)?;

        if self.rhs.needs_paren() {
            write!(format, "({})", self.rhs)
        } else {
            write!(format, "{}", self.rhs)
        }
    }
}

/// Implementation of [Debug] for [VelosiParseTreeBinOpExpr]
impl Debug for VelosiParseTreeBinOpExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Unary Operation Expressions
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an operator for a unary expression
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum VelosiParseTreeUnOp {
    // arithmetic operators
    Not,
    // boolean operator
    LNot,
}

/// Implementation of [Display] for [VelosiParseTreeUnOp]
impl Display for VelosiParseTreeUnOp {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        use VelosiParseTreeUnOp::*;
        match self {
            Not => write!(format, "~"),
            LNot => write!(format, "!"),
        }
    }
}

/// Implementation of the [Debug] trait for the [VelosiParseTreeUnOp] struct
impl Debug for VelosiParseTreeUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(&self, f)
    }
}

/// Represents an unary operation `!a`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeUnOpExpr {
    /// operator of the unary expression
    pub op: VelosiParseTreeUnOp,
    /// the expression the unary operator is applied to
    pub expr: Box<VelosiParseTreeExpr>,
    /// source location covering the unary expression
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeUnOpExpr {
    /// Constructs a new unary operator parse tree node
    pub fn with_loc(
        op: VelosiParseTreeUnOp,
        expr: VelosiParseTreeExpr,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            op,
            expr: Box::new(expr),
            loc,
        }
    }
    /// Constructs a new unary operator parse tree node with default location
    pub fn new(op: VelosiParseTreeUnOp, expr: VelosiParseTreeExpr) -> Self {
        Self::with_loc(op, expr, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeUnOpExpr]
impl Display for VelosiParseTreeUnOpExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        write!(format, "{}({})", self.op, self.expr)
    }
}

/// Implementation of [Debug] for [VelosiParseTreeUnOpExpr]
impl Debug for VelosiParseTreeUnOpExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Quantifier Expression
///////////////////////////////////////////////////////////////////////////////////////////////////

/// representation of a quantifier
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum VelosiParseTreeQuantifier {
    Forall,
    Exists,
}

/// Implementation of [Display] for [VelosiParseTreeQuantifier]
impl Display for VelosiParseTreeQuantifier {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        use VelosiParseTreeQuantifier::*;
        match self {
            Forall => write!(format, "forall"),
            Exists => write!(format, "exists"),
        }
    }
}

/// Implementation of the [Display] trait for the [VelosiParseTreeQuantifier] struct
impl Debug for VelosiParseTreeQuantifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(&self, f)
    }
}

/// Represents a quantifier expression `forall x : int :: x < 0`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeQuantifierExpr {
    /// quantifier kind
    pub quant: VelosiParseTreeQuantifier,
    /// list of quantified parameteres
    pub params: Vec<VelosiParseTreeParam>,
    /// expression to evaluate with the quantified parameters
    pub expr: Box<VelosiParseTreeExpr>,
    /// source location of the quantifier expression
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeQuantifierExpr {
    /// constructs a new quantifier expression node
    pub fn with_loc(
        quant: VelosiParseTreeQuantifier,
        params: Vec<VelosiParseTreeParam>,
        expr: VelosiParseTreeExpr,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            quant,
            params,
            expr: Box::new(expr),
            loc,
        }
    }

    /// constructs a new quantifier expression node with default location
    pub fn new(
        quant: VelosiParseTreeQuantifier,
        params: Vec<VelosiParseTreeParam>,
        expr: VelosiParseTreeExpr,
    ) -> Self {
        Self::with_loc(quant, params, expr, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeQuantifierExpr]
impl Display for VelosiParseTreeQuantifierExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        write!(format, "{} ", self.quant)?;
        for (i, p) in self.params.iter().enumerate() {
            if i != 0 {
                write!(format, ", ")?;
            }
            write!(format, "{p}")?;
        }
        write!(format, " :: {}", self.expr)
    }
}

/// Implementation of [Debug] for [VelosiParseTreeQuantifierExpr]
impl Debug for VelosiParseTreeQuantifierExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Literal Expressions
///////////////////////////////////////////////////////////////////////////////////////////////////

/// reprsents a identifier literal expression `foo`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeIdentifierLiteral {
    /// path of the identifier literal
    pub path: Vec<VelosiParseTreeIdentifier>,
    /// location of the identifier literal in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeIdentifierLiteral {
    /// constructs a new identifier literal expression
    pub fn with_loc(path: Vec<VelosiParseTreeIdentifier>, loc: VelosiTokenStream) -> Self {
        Self { path, loc }
    }

    /// constructs a new identifier literal expression with default location
    pub fn new(path: Vec<VelosiParseTreeIdentifier>) -> Self {
        Self::with_loc(path, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeIdentifierLiteral]
impl Display for VelosiParseTreeIdentifierLiteral {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        for (i, p) in self.path.iter().enumerate() {
            if i != 0 {
                write!(format, ".")?;
            }
            write!(format, "{p}")?;
        }
        Ok(())
    }
}

/// Implementation of [Debug] for [VelosiParseTreeIdentifierLiteral]
impl Debug for VelosiParseTreeIdentifierLiteral {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

/// Represents a numeric literal exprssion `123`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeNumLiteral {
    /// value of the numeric literal
    pub value: u64,
    /// location of the identifier literal in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeNumLiteral {
    /// constructs a new numeric literal expression
    pub fn with_loc(value: u64, loc: VelosiTokenStream) -> Self {
        Self { value, loc }
    }

    /// constructs a new numeric literal expression with default location
    pub fn new(value: u64) -> Self {
        Self::with_loc(value, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeNumLiteral]
impl Display for VelosiParseTreeNumLiteral {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        write!(format, "{}", self.value)
    }
}

/// Implementation of [Debug] for [VelosiParseTreeNumLiteral]
impl Debug for VelosiParseTreeNumLiteral {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

/// Represents a boolean literal expression `true`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeBoolLiteral {
    /// value of the boolean literal
    pub value: bool,
    /// location of the boolean literal in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeBoolLiteral {
    /// constructs a new numeric literal expression
    pub fn with_loc(value: bool, loc: VelosiTokenStream) -> Self {
        Self { value, loc }
    }

    /// constructs a new numeric literal expression with default location
    pub fn new(value: bool) -> Self {
        Self::with_loc(value, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeBoolLiteral]
impl Display for VelosiParseTreeBoolLiteral {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        write!(format, "{}", self.value)
    }
}

/// Implementation of [Debug] for [VelosiParseTreeBoolLiteral]
impl Debug for VelosiParseTreeBoolLiteral {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Function Call Expression
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a function call expression `f(43)`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeFnCallExpr {
    /// identifier of the function to be called
    pub name: VelosiParseTreeIdentifier,
    /// arguments passed to the function call
    pub args: Vec<VelosiParseTreeExpr>,
    /// location of the function call expression in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeFnCallExpr {
    /// constructs a new function call expression
    pub fn with_loc(
        name: VelosiParseTreeIdentifier,
        args: Vec<VelosiParseTreeExpr>,
        loc: VelosiTokenStream,
    ) -> Self {
        Self { name, args, loc }
    }

    /// constructs a new function call expression with default location
    pub fn new(name: VelosiParseTreeIdentifier, args: Vec<VelosiParseTreeExpr>) -> Self {
        Self::with_loc(name, args, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeFnCallExpr]
impl Display for VelosiParseTreeFnCallExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        write!(format, "{}(", self.name)?;
        for (i, p) in self.args.iter().enumerate() {
            if i != 0 {
                write!(format, ", ")?;
            }
            write!(format, "{p}")?;
        }
        write!(format, ")")
    }
}

/// Implementation of [Debug] for [VelosiParseTreeFnCallExpr]
impl Debug for VelosiParseTreeFnCallExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// IF Else Expression
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an if-then-else expression `if a < b { 1 } else { 3 }`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeIfElseExpr {
    /// condition expression
    pub cond: Box<VelosiParseTreeExpr>,
    /// expression of the then-branch
    pub then: Box<VelosiParseTreeExpr>,
    /// expression of the else-branch
    pub other: Box<VelosiParseTreeExpr>,
    /// location of the if-then-else expression in the source file
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeIfElseExpr {
    /// constructs a new if-then-else expression
    pub fn with_loc(
        cond: VelosiParseTreeExpr,
        then: VelosiParseTreeExpr,
        other: VelosiParseTreeExpr,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            cond: Box::new(cond),
            then: Box::new(then),
            other: Box::new(other),
            loc,
        }
    }

    /// constructs a new if-then-else expression with default location
    pub fn new(
        cond: VelosiParseTreeExpr,
        then: VelosiParseTreeExpr,
        other: VelosiParseTreeExpr,
    ) -> Self {
        Self::with_loc(cond, then, other, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeIfElseExpr]
impl Display for VelosiParseTreeIfElseExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        write!(
            format,
            "if {} {{ {} }} else {{ {} }}",
            self.cond, self.then, self.other
        )
    }
}

/// Implementation of [Debug] for [VelosiParseTreeIfElseExpr]
impl Debug for VelosiParseTreeIfElseExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Range Expression
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an range expression `0..1`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeRangeExpr {
    /// start of the range (including)
    pub start: Box<VelosiParseTreeExpr>,
    /// end value of the range (not including)
    pub end: Box<VelosiParseTreeExpr>,
    /// location of the range expression in the source tree
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeRangeExpr {
    /// constructs a new range expression with default location
    pub fn with_loc(
        start: VelosiParseTreeExpr,
        end: VelosiParseTreeExpr,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            start: Box::new(start),
            end: Box::new(end),
            loc,
        }
    }

    /// constructs a new range expression with default location
    pub fn new(start: VelosiParseTreeExpr, end: VelosiParseTreeExpr) -> Self {
        Self::with_loc(start, end, VelosiTokenStream::default())
    }

    pub fn new_fixed(start: u64, end: u64) -> Self {
        Self::with_loc(
            VelosiParseTreeExpr::NumLiteral(VelosiParseTreeNumLiteral::new(start)),
            VelosiParseTreeExpr::NumLiteral(VelosiParseTreeNumLiteral::new(end)),
            VelosiTokenStream::default(),
        )
    }
}

/// Implementation of [Display] for [VelosiParseTreeRangeExpr]
impl Display for VelosiParseTreeRangeExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        write!(format, "{}..{}", self.start, self.end)
    }
}

/// Implementation of [Debug] for [VelosiParseTreeRangeExpr]
impl Debug for VelosiParseTreeRangeExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Slice Expression
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an slice expression `e[0..1]`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeSliceExpr {
    /// expresson producing an array-like structure
    pub name: Box<VelosiParseTreeExpr>,
    /// range of the slice
    pub range: VelosiParseTreeRangeExpr,
    /// location of the slice expression in the source tree
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeSliceExpr {
    /// constructs a new slice expression
    pub fn with_loc(
        name: VelosiParseTreeExpr,
        range: VelosiParseTreeRangeExpr,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            name: Box::new(name),
            range,
            loc,
        }
    }

    /// constructs a new slice expression with default location
    pub fn new(name: VelosiParseTreeExpr, range: VelosiParseTreeRangeExpr) -> Self {
        Self::with_loc(name, range, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeSliceExpr]
impl Display for VelosiParseTreeSliceExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        write!(
            format,
            "{}[{}..{}]",
            self.name, self.range.start, self.range.end
        )
    }
}

/// Implementation of [Debug] for [VelosiParseTreeSliceExpr]
impl Debug for VelosiParseTreeSliceExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Element Expression
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an element access expression `e[0]`
#[derive(PartialEq, Eq, Clone)]
pub struct VelosiParseTreeElementExpr {
    /// expresson producing an array-like structure
    pub name: Box<VelosiParseTreeExpr>,
    /// index into the expression
    pub idx: Box<VelosiParseTreeExpr>,
    /// location of the element expression in the source tree
    pub loc: VelosiTokenStream,
}

impl VelosiParseTreeElementExpr {
    pub fn with_loc(
        name: VelosiParseTreeExpr,
        idx: VelosiParseTreeExpr,
        loc: VelosiTokenStream,
    ) -> Self {
        Self {
            name: Box::new(name),
            idx: Box::new(idx),
            loc,
        }
    }

    pub fn new(name: VelosiParseTreeExpr, idx: VelosiParseTreeExpr) -> Self {
        Self::with_loc(name, idx, VelosiTokenStream::default())
    }
}

/// Implementation of [Display] for [VelosiParseTreeElementExpr]
impl Display for VelosiParseTreeElementExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        write!(format, "{}[{}]", self.name, self.idx)
    }
}

/// Implementation of [Debug] for [VelosiParseTreeElementExpr]
impl Debug for VelosiParseTreeElementExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Expressions
///////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents an expression in the parse tree
#[derive(PartialEq, Eq, Clone)]
pub enum VelosiParseTreeExpr {
    Identifier(VelosiParseTreeIdentifierLiteral),
    NumLiteral(VelosiParseTreeNumLiteral),
    BoolLiteral(VelosiParseTreeBoolLiteral),
    BinOp(VelosiParseTreeBinOpExpr),
    UnOp(VelosiParseTreeUnOpExpr),
    Quantifier(VelosiParseTreeQuantifierExpr),
    FnCall(VelosiParseTreeFnCallExpr),
    IfElse(VelosiParseTreeIfElseExpr),
    Slice(VelosiParseTreeSliceExpr),
    Range(VelosiParseTreeRangeExpr),
    Element(VelosiParseTreeElementExpr),
}

impl VelosiParseTreeExpr {
    /// returns the location of the expression
    pub fn loc(&self) -> &VelosiTokenStream {
        use VelosiParseTreeExpr::*;
        match self {
            Identifier(i) => &i.loc,
            NumLiteral(i) => &i.loc,
            BoolLiteral(i) => &i.loc,
            BinOp(i) => &i.loc,
            UnOp(i) => &i.loc,
            Quantifier(i) => &i.loc,
            FnCall(i) => &i.loc,
            IfElse(i) => &i.loc,
            Slice(i) => &i.loc,
            Range(i) => &i.loc,
            Element(i) => &i.loc,
        }
    }

    /// evaluates whether we need to add parenthesis for printing
    fn needs_paren(&self) -> bool {
        matches!(
            self,
            VelosiParseTreeExpr::BinOp(_) | VelosiParseTreeExpr::Quantifier(_)
        )
    }
}

/// Implementation of [Display] for [VelosiParseTreeExpr]
impl Display for VelosiParseTreeExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        use VelosiParseTreeExpr::*;
        match self {
            Identifier(i) => Display::fmt(&i, format),
            NumLiteral(i) => Display::fmt(&i, format),
            BoolLiteral(i) => Display::fmt(&i, format),
            BinOp(i) => Display::fmt(&i, format),
            UnOp(i) => Display::fmt(&i, format),
            Quantifier(i) => Display::fmt(&i, format),
            FnCall(i) => Display::fmt(&i, format),
            IfElse(i) => Display::fmt(&i, format),
            Slice(i) => Display::fmt(&i, format),
            Range(i) => Display::fmt(&i, format),
            Element(i) => Display::fmt(&i, format),
        }
    }
}

/// Implementation of [Debug] for [VelosiParseTreeExpr]
impl Debug for VelosiParseTreeExpr {
    fn fmt(&self, format: &mut Formatter) -> FmtResult {
        Display::fmt(&self, format)
    }
}
