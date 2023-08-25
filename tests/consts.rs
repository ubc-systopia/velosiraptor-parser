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

//! # VelosiParser Tests: Example Specs

// std includes
use std::path::Path;

mod utils;

/// Test basic constant definitions
#[test]
fn const_defs() {
    let vrs = Path::new("tests/vrs/consts/const_00_defs.vrs");
    let exp = Path::new("tests/vrs/consts/const_00_defs_expected.txt");
    utils::check_parse_ok(vrs, None);
    utils::check_parse_ok(vrs, Some(exp));
}

/// Test imported constants
#[test]
fn const_imported() {
    let vrs = Path::new("tests/vrs/consts/const_01_imported.vrs");
    let exp = Path::new("tests/vrs/consts/const_01_imported_expected.txt");
    utils::check_parse_ok(vrs, None);
    utils::check_parse_ok(vrs, Some(exp));
}

/// Test constants within unit context
#[test]
fn const_unit() {
    let vrs = Path::new("tests/vrs/consts/const_02_unit.vrs");
    let exp = Path::new("tests/vrs/consts/const_02_unit_expected.txt");
    utils::check_parse_ok(vrs, None);
    utils::check_parse_ok(vrs, Some(exp));
}

/// Test expressions with constants
#[test]
fn const_expr() {
    let vrs = Path::new("tests/vrs/consts/const_03_expr.vrs");
    let exp = Path::new("tests/vrs/consts/const_03_expr_expected.txt");
    utils::check_parse_ok(vrs, None);
    utils::check_parse_ok(vrs, Some(exp));
}

/// Test missing semicolon
#[test]
fn const_missing_semicolon() {
    let vrs = Path::new("tests/vrs/consts/const_04_missing_semicolon.vrs");
    let exp = Path::new("tests/vrs/consts/const_04_missing_semicolon_expected.txt");
    utils::check_parse_fail(vrs, None);
    utils::check_parse_fail(vrs, Some(exp));
}

/// Test missing semicolon
#[test]
fn const_missing_type() {
    let vrs = Path::new("tests/vrs/consts/const_05_missing_type.vrs");
    let exp = Path::new("tests/vrs/consts/const_05_missing_type_expected.txt");
    utils::check_parse_fail(vrs, None);
    utils::check_parse_fail(vrs, Some(exp));
}
