// VelosiParser -- Parser for the VelosiRaptor specification language
//
//
// MIT License
//
// Copyright (c) 2021-2023 Systopia Lab, Computer Science, University of British Columbia
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

//! # VelosiParser: Methods
//!
//! This module contains the parser for method definitions within the unit contexts.

// external dependencies
use nom::{
    combinator::{cut, opt},
    multi::separated_list1,
    sequence::delimited,
};

// used crate functionality
use crate::error::IResult;

use crate::parser::terminals::{comma, hashtag, ident, lbrack, lparen, rbrack, rparen};
use crate::parsetree::VelosiParseTreeProperty;
use crate::VelosiTokenStream;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Decorator / Property Parsing
////////////////////////////////////////////////////////////////////////////////////////////////////

/// parses a decorator/property list of a method
///
/// This function parses a function decorator/property
///
/// # Arguments
///
/// * `input` - input token stream to be parsed
///
/// # Results
///
/// * Ok:  The parser succeeded. The return value is a tuple of the remaining input and the
///        recognized decorator list as a parse tree node.
/// * Err: The parser did not succeed. The return value indicates whether this is:
///
///    * Error: a recoverable error indicating that the parser did not recognize the input but
///             another parser might, or
///    * Failure: a fatal failure indicating the parser recognized the input but failed to parse it
///               and that another parser would fail.
///
/// # Grammar
///
/// `DECORATOR := KW_HASHTAG RBRAK DECORATOR_ELEMENT RBRAK`
///
/// # Example
///
/// * `#[foo]`
///
pub fn decorator_list(
    input: VelosiTokenStream,
) -> IResult<VelosiTokenStream, Vec<VelosiParseTreeProperty>> {
    let (i1, _) = hashtag(input)?;

    // [ ident ]
    let (i2, decorators) = cut(delimited(
        lbrack,
        separated_list1(comma, decorator_element),
        rbrack,
    ))(i1)?;

    Ok((i2, decorators))
}

/// parses a decorator/property element
///
/// # Arguments
///
/// * `input` - input token stream to be parsed
///
/// # Results
///
/// * Ok:  The parser succeeded. The return value is a tuple of the remaining input and the
///        recognized decorator/property as a parse tree node.
/// * Err: The parser did not succeed. The return value indicates whether this is:
///
///    * Error: a recoverable error indicating that the parser did not recognize the input but
///             another parser might, or
///    * Failure: a fatal failure indicating the parser recognized the input but failed to parse it
///               and that another parser would fail.
///
/// # Grammar
///
/// `DECORATOR_ELEMENT := IDENT (LPAREN IDENT RPAREN)?`
///
/// # Examples
///
///  * `foo`
///  * `bar(a)`
///
///
fn decorator_element(
    input: VelosiTokenStream,
) -> IResult<VelosiTokenStream, VelosiParseTreeProperty> {
    let mut loc = input.clone();
    let (i1, name) = ident(input)?;
    let (i2, arg) = opt(delimited(lparen, cut(ident), cut(rparen)))(i1)?;

    let params = if let Some(arg) = arg {
        vec![arg]
    } else {
        Vec::new()
    };

    loc.expand_until_start(&i2);

    let res = VelosiParseTreeProperty {
        ident: name,
        params,
        loc,
    };

    Ok((i2, res))
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
use velosilexer::VelosiLexer;

#[cfg(test)]
use crate::{test_parse_and_check_fail, test_parse_and_check_ok};

#[test]
fn test_decorator_ok() {
    test_parse_and_check_ok!("foo", decorator_element);
    test_parse_and_check_ok!("foo(bar)", decorator_element);
}

#[test]
fn test_decorator_fail() {
    test_parse_and_check_fail!("foo(bar, baz)", decorator_element);
    test_parse_and_check_fail!("foo[bar, baz]", decorator_element);
}

#[test]
fn test_decorator_list_ok() {
    test_parse_and_check_ok!("#[foo]", decorator_list);
    test_parse_and_check_ok!("#[foo(bar)]", decorator_list);
    test_parse_and_check_ok!("#[foo(bar), baz]", decorator_list);
}

#[test]
fn test_decorator_list_fail() {
    test_parse_and_check_fail!("#[]", decorator_list);
    test_parse_and_check_fail!("#[foo(bar),]", decorator_list);
    test_parse_and_check_fail!("#[foo(bar) baz]", decorator_list);
}
