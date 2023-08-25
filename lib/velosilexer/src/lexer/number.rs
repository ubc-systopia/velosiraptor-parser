// Velosiraptor Compiler
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

//! VelosiLexer - Number Parsing

// used external dependencies
use nom::{
    branch::alt,
    bytes::complete::{is_a, tag},
    character::complete::{alphanumeric1, digit1, hex_digit1, oct_digit1},
    combinator::recognize,
    multi::many0,
    sequence::pair,
    Err, InputLength, Slice,
};
use tokstream::{SrcSpan, Tok};

// used crate dependencies
use crate::error::{IResult, VelosiLexerErrBuilder};
use crate::{VelosiToken, VelosiTokenKind};

////////////////////////////////////////////////////////////////////////////////////////////////////
// Number Parsing
////////////////////////////////////////////////////////////////////////////////////////////////////

/// parses a number in base 2, 8, 16, 10
///
/// Numbers can be separated by `_` to improve readabililty.
///
/// The parsed numbers must fit in 64-bits.
///
/// # Arguments
///
/// * `input` - the input source span to be lexed
///
/// # Returns
///
/// Returns a tuple of the remaining input and the parsed number token on success.
/// Otherwise, the an [crate::error::VelosiLexerErr] error is returned.
///
/// # Grammar
///
/// `NUMBER := BASE16 | BASE8 | BASE2 | BASE10`
///
pub fn number(input: SrcSpan) -> IResult<SrcSpan, VelosiToken> {
    alt((base16, base8, base2, base10))(input)
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Number Parsing: Base 10
////////////////////////////////////////////////////////////////////////////////////////////////////

/// parses a number in base 10
///
/// Numbers can be separated by `_` to improve readabililty.
///
/// The parsed numbers must fit in 64-bits.
///
/// # Arguments
///
/// * `input` - the input source span to be lexed
///
/// # Returns
///
/// Returns a tuple of the remaining input and the parsed number token on success.
/// Otherwise, the an [crate::error::VelosiLexerErr] error is returned.
///
/// # Grammar
///
/// `BASE10 := DIGIT [DIGIT, '_']*`
///
fn base10(input: SrcSpan) -> IResult<SrcSpan, VelosiToken> {
    // match a digit followed by alphanumeric characters and the `_`
    // this is needed to recognize patterns of: 1234asdf
    let otherchar = alt((alphanumeric1, tag("_")));
    let (rem, numsp) = recognize(pair(digit1, many0(otherchar)))(input)?;

    // allow groups of digits 1234_5678
    let otherdigits = alt((digit1, tag("_")));

    // parse the numsp again, with restricted tokens
    let (rem1, _) = recognize(pair(digit1, many0(otherdigits)))(numsp.clone())?;

    // if it's not empty there will be some junk at the end of the number
    if !rem1.is_empty() {
        let (sp, hint) = if rem1.as_str().chars().all(|x| x.is_ascii_hexdigit()) {
            let hint = format!("Change this number to hex: `0x{}`", numsp.as_str());
            (numsp, hint)
        } else {
            (
                rem1,
                "remove unsupported characters from this number.".to_string(),
            )
        };
        let errmsg = "unsupported digit in number encountered.";
        let err = VelosiLexerErrBuilder::new(errmsg.to_string())
            .add_hint(hint)
            .add_location(sp)
            .build();
        return Err(Err::Failure(err));
    }

    // now convert the string to to a number
    let numstr = String::from(numsp.as_str()).replace('_', "");
    let num = match numstr.parse::<u64>() {
        Ok(i) => i,
        Err(_) => {
            let errmsg = "number too large to be stored as a 64-bit number.";
            let err = VelosiLexerErrBuilder::new(errmsg.to_string())
                .add_hint("reduce this number to fit within 64-bits.".to_string())
                .add_location(rem1)
                .build();
            return Err(Err::Failure(err));
        }
    };
    Ok((rem, Tok::new(VelosiTokenKind::NumLiteral(num), numsp)))
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Number Parsing: Base 2, 8, 16
////////////////////////////////////////////////////////////////////////////////////////////////////

// Macro to generate the parser function
macro_rules! namedbase (
    ($name:ident, $radix:expr, $tag:expr, $pattern:expr) => (
        fn $name(input: SrcSpan) -> IResult<SrcSpan, VelosiToken> {
            // check if it's the right tag `0x`, otherwise return
            let prefix = tag($tag);
            let (i1, _) = prefix(input.clone())?;

            // match alphanumeric groups separated by `_`
            let otherchar = alt((alphanumeric1, tag("_")));
            let (rem, numsp) = recognize(pair(alphanumeric1, many0(otherchar)))(i1.clone())?;

            // allow groups of digits 1234_abcd
            let otherdigits = alt(($pattern, tag("_")));

            // parse the numsp again, with restricted tokens
            let (rem1, _) = recognize(pair($pattern, many0(otherdigits)))(numsp.clone())?;

            // if it's not empty there will be some junk at the end of the number
            if ! rem1.is_empty() {
                let errmsg = "unsupported digit in number encountered.";
                let err = VelosiLexerErrBuilder::new(errmsg.to_string())
                    .add_hint("remove unsupported characters from this number.".to_string())
                    .add_location(rem1)
                    .build();
                return Err(Err::Failure(err));
            }

            // now convert the string to to a number
            let numstr = String::from(numsp.as_str()).replace("_", "");
            match u64::from_str_radix(&numstr, $radix) {
                Ok(i) => Ok((rem, Tok::new(VelosiTokenKind::NumLiteral(i), input.slice(0..numsp.input_len() + 2)))),
                Err(_) => {
                    let errmsg = "number too large to be stored as a 64-bit number.";
                    let err = VelosiLexerErrBuilder::new(errmsg.to_string())
                        .add_hint("reduce this number to fit within 64-bits.".to_string())
                        .add_location(rem1)
                        .build();
                    return Err(Err::Failure(err));
                }
            }
        }
    )
);

// parses a number in base 16
//
// Numbers can be separated by `_` to improve readabililty.
//
// The parsed numbers must fit in 64-bits.
//
// # Arguments
//
// * `input` - the input source span to be lexed
//
// # Returns
//
// Returns a tuple of the remaining input and the parsed number token on success.
// Otherwise, the an [crate::error::VelosiLexerErr] error is returned.
//
// # Grammar
//
// `BASE16 := '0x' HEX_DIGIT [HEX_DIGIT, '_']*`
//
namedbase!(base16, 16, "0x", hex_digit1);

// parses a number in base 8
//
// Numbers can be separated by `_` to improve readabililty.
//
// The parsed numbers must fit in 64-bits.
//
// # Arguments
//
// * `input` - the input source span to be lexed
//
// # Returns
//
// Returns a tuple of the remaining input and the parsed number token on success.
// Otherwise, the an [crate::error::VelosiLexerErr] error is returned.
//
// # Grammar
//
// `BASE16 := '0o' OCT_DIGIT [OCT_DIGIT, '_']*`
//
namedbase!(base8, 8, "0o", oct_digit1);

// parses a number in base 2
//
// Numbers can be separated by `_` to improve readabililty.
//
// The parsed numbers must fit in 64-bits.
//
// # Arguments
//
// * `input` - the input source span to be lexed
//
// # Returns
//
// Returns a tuple of the remaining input and the parsed number token on success.
// Otherwise, the an [crate::error::VelosiLexerErr] error is returned.
//
// # Grammar
//
// `BASE16 := '0b' BIN_DIGIT [BIN_DIGIT, '_']*`
//
namedbase!(base2, 2, "0b", is_a("01"));

////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////

#[test]
fn decimal_test() {
    let sp = SrcSpan::new("12312".to_string());
    let rem = sp.slice(5..5);
    let num = sp.slice(0..5);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(12312), num)
        ))
    );

    let sp = SrcSpan::new("12312213489654".to_string());
    let rem = sp.slice(14..14);
    let num = sp.slice(0..14);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(12312213489654), num)
        ))
    );
}

#[test]
fn hexadecimal_test() {
    let sp = SrcSpan::new("0xABCD".to_string());
    let rem = sp.slice(6..6);
    let num = sp.slice(0..6);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(0xABCD), num)
        ))
    );

    let sp = SrcSpan::new("0xabcd".to_string());
    let rem = sp.slice(6..6);
    let num = sp.slice(0..6);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(0xABCD), num)
        ))
    );
}

#[test]
fn octal_test() {
    let sp = SrcSpan::new("0o1234".to_string());
    let rem = sp.slice(6..6);
    let num = sp.slice(0..6);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(0o1234), num)
        ))
    );
}

#[test]
fn binary_test() {
    let sp = SrcSpan::new("0b1000".to_string());
    let rem = sp.slice(6..6);
    let num = sp.slice(0..6);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(0b1000), num)
        ))
    );
}

#[test]
fn separator_test() {
    let sp = SrcSpan::new("0b1111_0000".to_string());
    let rem = sp.slice(11..11);
    let num = sp.slice(0..11);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(0b11110000), num)
        ))
    );

    let sp = SrcSpan::new("0o4567_1234".to_string());
    let rem = sp.slice(11..11);
    let num = sp.slice(0..11);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(0o45671234), num)
        ))
    );

    let sp = SrcSpan::new("0xabcd_1234".to_string());
    let rem = sp.slice(11..11);
    let num = sp.slice(0..11);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(0xabcd1234), num)
        ))
    );

    let sp = SrcSpan::new("1_000_000".to_string());
    let rem = sp.slice(9..9);
    let num = sp.slice(0..9);
    assert_eq!(
        number(sp),
        Ok((
            rem,
            VelosiToken::new(VelosiTokenKind::NumLiteral(1000000), num)
        ))
    );

    let sp = SrcSpan::new("0o11223344 asdf".to_string());
    assert!(number(sp).is_ok());

    let sp = SrcSpan::new("0b11001100 asdf".to_string());
    assert!(number(sp).is_ok());
}

#[test]
fn not_a_number() {
    let sp = SrcSpan::new("12354asdf".to_string());
    assert!(number(sp).is_err());

    let sp = SrcSpan::new("0x1234oiweu".to_string());
    assert!(number(sp).is_err());

    let sp = SrcSpan::new("0o123lajks".to_string());
    assert!(number(sp).is_err());

    let sp = SrcSpan::new("0b11123".to_string());
    assert!(number(sp).is_err());

    let sp = SrcSpan::new("0b".to_string());
    assert!(number(sp).is_err());

    let sp = SrcSpan::new("0x".to_string());
    assert!(number(sp).is_err());

    let sp = SrcSpan::new("0o".to_string());
    assert!(number(sp).is_err());
}

#[test]
fn out_of_range_test() {
    let sp = SrcSpan::new("0x10000000000000000".to_string());
    assert!(number(sp).is_err());
}
