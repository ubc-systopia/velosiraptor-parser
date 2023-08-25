// VelosiParser -- Parser for the VelosiRaptor specification language
//
//
// MIT License
//
// Copyright (c) 2022,2023 Systopia Lab, Computer Science, University of British Columbia
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

//! # VelosiParser - State Definition

// external dependencies
use nom::{
    combinator::{cut, opt},
    multi::separated_list0,
    sequence::{delimited, tuple},
};

// lexer, parser terminals and ast
use crate::error::IResult;
use crate::parser::{parameter::param_list, terminals::*};
use crate::parsetree::VelosiParseTreeState;
use crate::VelosiTokenStream;

// submodules
mod fields;

////////////////////////////////////////////////////////////////////////////////////////////////////
// State Parsing
////////////////////////////////////////////////////////////////////////////////////////////////////

/// parses and consumes a state definition of a unit
///
/// # Arguments
///
/// * `input` - input token stream to be parsed
///
/// # Results
///
/// * Ok:  The parser succeeded. The return value is a tuple of the remaining input and the
///        recognized state definition as a parse tree node.
/// * Err: The parser did not succed. The return value indicates whether this is:
///
///    * Error: a recoverable error indicating that the parser did not recognize the input but
///             another parser might, or
///    * Failure: a fatal failure indicating the parser recognized the input but failed to parse it
///               and that another parser would fail.
///
/// # Grammar
///
/// `STATE := KW_STATE STATEPARAMS LBRACE STATEFIELDS RBRACE`
///
/// # Examples
///
/// * `state() {}`
/// * `state(base: int) { mem field [ base, 0, 0 ] }`
pub fn state(input: VelosiTokenStream) -> IResult<VelosiTokenStream, VelosiParseTreeState> {
    let mut loc = input.clone();

    let (i1, _) = kw_state(input)?;
    let (i2, bases) = param_list(i1)?;
    let (i3, fields) = cut(delimited(
        lbrace,
        separated_list0(comma, fields::statefield),
        tuple((opt(comma), rbrace)),
    ))(i2)?;

    loc.span_until_start(&i3);

    Ok((i3, VelosiParseTreeState::with_loc(bases, fields, loc)))
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
use velosilexer::VelosiLexer;

#[cfg(test)]
use crate::{test_parse_and_check_fail, test_parse_and_compare_ok};

#[test]
fn test_state_def_ok() {
    test_parse_and_compare_ok!("state(){}", state, "state() {\n}\n");
    test_parse_and_compare_ok!("state(base: foo){}", state, "state(base: foo) {\n}\n");
    // with a register
    test_parse_and_compare_ok!(
        "state(base: foo){ reg foo [ 8 ] }",
        state,
        "state(base: foo) {\n  reg foo [ 8 ],\n}\n"
    );
    // trailing comma
    test_parse_and_compare_ok!(
        "state(base: foo){ reg foo [ 8 ], }",
        state,
        "state(base: foo) {\n  reg foo [ 8 ],\n}\n"
    );
    // with two registers
    test_parse_and_compare_ok!(
        "state(base: foo){ reg foo [ 8 ], reg foo [ 8 ] }",
        state,
        "state(base: foo) {\n  reg foo [ 8 ],\n  reg foo [ 8 ],\n}\n"
    );
}

#[test]
fn test_iface_def_fail() {
    // no separator
    test_parse_and_check_fail!("state(base: foo){ reg foo [ 8 ]\n reg foo [ 8 ] }", state);
    // wrong separator
    test_parse_and_check_fail!("state(base: foo){ reg foo [ 8 ]; reg foo [ 8 ] }", state);
}
