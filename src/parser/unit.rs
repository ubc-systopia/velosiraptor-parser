// VelosiParser -- Parser for the VelosiRaptor specification language
//
//
// MIT License
//
// Copyright (c) 2021 Systopia Lab, Computer Science, University of British Columbia
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

//! # VelosiParser -- Unit Definitions
//!
//! This module parses a unit definition with its state, interface and methods.

// external dependencies
use nom::{
    branch::alt,
    combinator::{cut, map, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, tuple},
};

// the used library-internal functionaltity
use crate::error::IResult;
use crate::parser::flags;
use crate::parser::{
    constdef,
    expr::expr,
    interface::interface,
    map::staticmap,
    method::method,
    parameter::param_list,
    state::state,
    terminals::{
        assign, colon, comma, ident, kw_abstract, kw_enum, kw_inbitwidth, kw_osspec,
        kw_outbitwidth, kw_segment, kw_staticmap, lbrace, lparen, rbrace, rparen, semicolon,
    },
    types,
};
use crate::parsetree::{
    VelosiParseTreeConstDef, VelosiParseTreeEnum, VelosiParseTreeExternType, VelosiParseTreeFlags,
    VelosiParseTreeIdentifier, VelosiParseTreeInterface, VelosiParseTreeMethod,
    VelosiParseTreeParam, VelosiParseTreeState, VelosiParseTreeUnit, VelosiParseTreeUnitDef,
    VelosiParseTreeUnitNode,
};
use crate::VelosiTokenStream;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Unit Definitions
////////////////////////////////////////////////////////////////////////////////////////////////////

/// parses and consumes a unit definition with its state, interface etc.
///
/// # Arguments
///
/// * `input` - input token stream to be parsed
///
/// # Results
///
/// * Ok:  The parser succeeded. The return value is a tuple of the remaining input and the
///        recognized unit definition as a parse tree node.
/// * Err: The parser did not succeed. The return value indicates whether this is:
///
///    * Error: a recoverable error indicating that the parser did not recognize the input but
///             another parser might, or
///    * Failure: a fatal failure indicating the parser recognized the input but failed to parse it
///               and that another parser would fail.
///
/// # Grammar
///
/// UNIT := UNIT_SEGMENT | UNIT_STATICMAP | UNIT_ENUM | UNIT_OSSPEC
///
/// # Example
///
/// * `segment Foo (bar: baz, foo: bar) : FooBar { ... }`
/// * `staticmap Bar(foo: baz, bar: foo) : FooBar { ... }`
///
pub fn unit(input: VelosiTokenStream) -> IResult<VelosiTokenStream, VelosiParseTreeUnit> {
    alt((unit_segment, unit_staticmap, unit_enum, unit_osspec))(input)
}

/// parses the derived clause of a unit
///
/// # Arguments
///
///  * `input`  - token stream representing the current input position
///
/// # Return Value
///
/// Result type wrapping a [String] and the remaining [VelosiTokenStream] if the parser succeeded,
/// or an error wrapping the input position if the parser failed.
///
/// # Grammar
///
/// `DERIVED_CLAUSE := COLON IDENTIFIER`
///
/// # Example
///
///  * `: FooBar`
///
/// # Notes
///
///  * None
fn derived_clause(
    input: VelosiTokenStream,
) -> IResult<VelosiTokenStream, VelosiParseTreeIdentifier> {
    preceded(colon, cut(ident))(input)
}

/// type definition for the unit header parser
type UnitHeader = (
    VelosiParseTreeIdentifier,
    Vec<VelosiParseTreeParam>,
    Option<VelosiParseTreeIdentifier>,
);

/// parses the unit header
///
/// # Arguments
///
///  * `input`  - token stream representing the current input position
///
/// # Return Value
///
/// Result type wrapping a [UnitHeader] and the remaining [VelosiTokenStream] if the parser succeeded,
/// or an error wrapping the input position if the parser failed.
///
/// # Grammar
///
/// `UNIT_HEADER := IDENTIFIER (LPAREN PARAM_CLAUSE RPAREN)? DERIVED_CLAUSE?`
///
/// # Example
///
///  * `Foo (bar: baz, foo: bar) : FooBar`
fn unit_header(input: VelosiTokenStream) -> IResult<VelosiTokenStream, UnitHeader> {
    tuple((cut(ident), param_list, opt(derived_clause)))(input)
}

/// parses the input bitwidth clause of the unit
///
/// # Arguments
///
///  * `input`  - token stream representing the current input position
///
/// # Return Value
///
/// Result type wrapping a [u64] and the remaining [VelosiTokenStream] if the parser succeeded,
/// or an error wrapping the input position if the parser failed.
///
/// # Grammar
///
/// `INBITWIDTH_CLAUSE := KW_INBITWIDTH ASSIGN CONST_EXPR SEMICOLON`
///
/// # Example
///
///  * `inbitwidth = 32;`
fn inbitwidth_clause(
    input: VelosiTokenStream,
) -> IResult<VelosiTokenStream, VelosiParseTreeUnitNode> {
    let mut loc = input.clone();
    let (i1, _) = kw_inbitwidth(input)?;
    let (i2, e) = cut(delimited(assign, expr, semicolon))(i1)?;

    loc.span_until_start(&i2);
    Ok((i2, VelosiParseTreeUnitNode::InBitWidth(e, loc)))
}

/// parses the output bitwidth clause of the unit
///
/// # Arguments
///
///  * `input`  - token stream representing the current input position
///
/// # Return Value
///
/// Result type wrapping a [u64] and the remaining [VelosiTokenStream] if the parser succeeded,
/// or an error wrapping the input position if the parser failed.
///
/// # Grammar
///
/// `OUTBITWIDTH_CLAUSE := KW_OUTBITWIDTH ASSIGN CONST_EXPR SEMICOLON`
///
/// # Example
///
///  * `outbitwidth = 32;`
///
fn outbitwidth_clause(
    input: VelosiTokenStream,
) -> IResult<VelosiTokenStream, VelosiParseTreeUnitNode> {
    let mut loc = input.clone();
    let (i1, _) = kw_outbitwidth(input)?;
    let (i2, e) = cut(delimited(assign, expr, semicolon))(i1)?;

    loc.span_until_start(&i2);
    Ok((i2, VelosiParseTreeUnitNode::OutBitWidth(e, loc)))
}

/// parses the unit body
///
/// # Arguments
///
/// # Return Value
///
/// # Grammar
///
/// `UNIT_BODY := (STATE_CLAUSE | INTERFACE_CLAUSE | METHOD_CLAUSE | CONSTDEF_CLAUSE |
///                FLAGS_CLAUSE | STATICMAP_CLAUSE)*`
///
fn unit_body(input: VelosiTokenStream) -> IResult<VelosiTokenStream, Vec<VelosiParseTreeUnitNode>> {
    many0(alt((
        inbitwidth_clause,
        outbitwidth_clause,
        map(types::extern_type, |s: VelosiParseTreeExternType| {
            VelosiParseTreeUnitNode::Type(s)
        }),
        map(method, |s: VelosiParseTreeMethod| {
            VelosiParseTreeUnitNode::Method(s)
        }),
        map(state, |s: VelosiParseTreeState| {
            VelosiParseTreeUnitNode::State(s)
        }),
        map(interface, |s: VelosiParseTreeInterface| {
            VelosiParseTreeUnitNode::Interface(s)
        }),
        map(flags::flags_unit, |s: VelosiParseTreeFlags| {
            VelosiParseTreeUnitNode::Flags(s)
        }),
        staticmap,
        map(constdef::constdef, |s: VelosiParseTreeConstDef| {
            VelosiParseTreeUnitNode::Const(s)
        }),
    )))(input)
}

/// parses the enum list
///
/// # Arguments
///
/// # Return Value
///
/// # Grammar
///
/// `ENUM_LIST := LIST(COMMA, IDENTIFIER LPAREN LIST(COMMA, IDENTIFIER) RPAREN)
///
fn enum_list(
    input: VelosiTokenStream,
) -> IResult<VelosiTokenStream, Vec<(VelosiParseTreeIdentifier, Vec<VelosiParseTreeIdentifier>)>> {
    separated_list0(
        comma,
        tuple((
            ident,
            cut(delimited(lparen, separated_list0(comma, ident), rparen)),
        )),
    )(input)
}

/// parses and consumes a segment unit declaration
///
/// # Arguments
///
///  * `input`  - token stream representing the current input position
///
/// # Return Value
///
/// Result type wrapping a [VelosiParseTreeUnit] and the remaining [VelosiTokenStream] if the
/// parser succeeded, or an error wrapping the input position if the parser failed.
///
/// # Grammar
///
/// UNIT_STATICMAP := KW_ABSTRACT? KW_SEGMENT UNIT_HEADER LBRACE UNIT_BODY RBRACE
///
fn unit_segment(input: VelosiTokenStream) -> IResult<VelosiTokenStream, VelosiParseTreeUnit> {
    let mut loc = input.clone();

    // try to match the segment keyword, if there is no match, return early.
    let (i1, (abs, _)) = tuple((opt(kw_abstract), kw_segment))(input)?;

    // we've seen the `staticmap` keyword, next there needs to be the unit identifier,
    // followed bby some optional parameters and the derived clause.
    let (i2, (unitname, params, derived)) = cut(unit_header)(i1)?;

    // parse the body within the curly brances
    let (i3, body) = cut(delimited(lbrace, unit_body, rbrace))(i2)?;

    loc.span_until_start(&i3);

    let unitdef =
        VelosiParseTreeUnitDef::with_loc(unitname, params, abs.is_some(), derived, body, loc);
    Ok((i3, VelosiParseTreeUnit::Segment(unitdef)))
}

/// parses and consumes a staticmap unit declaration
///
/// # Arguments
///
///  * `input`  - token stream representing the current input position
///
/// # Return Value
///
/// Result type wrapping a [VelosiParseTreeUnit] and the remaining [VelosiTokenStream] if the
/// parser succeeded, or an error wrapping the input position if the parser failed.
///
/// # Grammar
///
/// UNIT_STATICMAP := KW_ABSTRACT? KW_STATICMAP UNIT_HEADER LBRACE UNIT_BODY RBRACE
///
fn unit_staticmap(input: VelosiTokenStream) -> IResult<VelosiTokenStream, VelosiParseTreeUnit> {
    let mut loc = input.clone();

    // try to match the staticmap keyword, if there is no match, return early.
    let (i1, (abs, _)) = tuple((opt(kw_abstract), kw_staticmap))(input)?;

    // we've seen the `staticmap` keyword, next there needs to be the unit identifier,
    // followed bby some optional parameters and the derived clause.
    let (i2, (unitname, params, derived)) = cut(unit_header)(i1)?;

    // parse the body within the curly brances
    let (i3, body) = cut(delimited(lbrace, unit_body, rbrace))(i2)?;

    loc.span_until_start(&i3);

    let unitdef =
        VelosiParseTreeUnitDef::with_loc(unitname, params, abs.is_some(), derived, body, loc);
    Ok((i3, VelosiParseTreeUnit::StaticMap(unitdef)))
}

/// parses and consumes a enum unit declaration
///
/// # Arguments
///
///  * `input`  - token stream representing the current input position
///
/// # Return Value
///
/// Result type wrapping a [VelosiParseTreeUnit] and the remaining [VelosiTokenStream] if the
/// parser succeeded, or an error wrapping the input position if the parser failed.
///
/// # Grammar
///
/// UNIT_ENUM := KW_ENUM UNIT_HEADER LBRACE UNIT_BODY RBRACE
///
fn unit_enum(input: VelosiTokenStream) -> IResult<VelosiTokenStream, VelosiParseTreeUnit> {
    let mut loc = input.clone();

    // try to match the staticmap keyword, if there is no match, return early.
    let (i1, _) = kw_enum(input)?;

    // we've seen the `enum` keyword, next there needs to be the unit identifier,
    // followed by some optional parameters and the derived clause.
    let (i2, (unitname, params, derived)) = cut(unit_header)(i1)?;

    // parse the body within the curly brances
    let (i3, body) = cut(delimited(lbrace, enum_list, rbrace))(i2)?;

    let body = body
        .into_iter()
        .map(|(name, values)| {
            let mut loc = name.loc.clone();
            if let Some(last) = values.last() {
                loc.span_until_start(&last.loc);
            };
            VelosiParseTreeUnitNode::EnumEntry(VelosiParseTreeEnum::with_loc(name, values, loc))
        })
        .collect();

    loc.span_until_start(&i3);

    let unitdef = VelosiParseTreeUnitDef::with_loc(unitname, params, false, derived, body, loc);
    Ok((i3, VelosiParseTreeUnit::Enum(unitdef)))
}

/// parses and consumes a osspec unit declaration
///
/// # Arguments
///
///  * `input`  - token stream representing the current input position
///
/// # Return Value
///
/// Result type wrapping a [VelosiParseTreeUnit] and the remaining [VelosiTokenStream] if the
/// parser succeeded, or an error wrapping the input position if the parser failed.
///
/// # Grammar
///
/// UNIT_OSSPEC := KW_OSSPEC UNIT_HEADER LBRACE UNIT_BODY RBRACE
///
fn unit_osspec(input: VelosiTokenStream) -> IResult<VelosiTokenStream, VelosiParseTreeUnit> {
    let mut loc = input.clone();

    // try to match the staticmap keyword, if there is no match, return early.
    let (i1, _) = kw_osspec(input)?;

    // we've seen the `osspec` keyword, next there needs to be the unit identifier,
    // followed by some optional parameters and the derived clause.
    let (i2, (unitname, params, derived)) = cut(unit_header)(i1)?;

    // parse the body within the curly brances
    let (i3, body) = cut(delimited(lbrace, unit_body, rbrace))(i2)?;

    loc.span_until_start(&i3);

    let unitdef = VelosiParseTreeUnitDef::with_loc(unitname, params, false, derived, body, loc);
    Ok((i3, VelosiParseTreeUnit::OSSpec(unitdef)))
}
