use crate::rust::{PathSegment, PathSegmentName, Token};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::complete::{char, multispace0};
use nom::combinator::{all_consuming, map, opt, recognize};
use nom::multi::{many0, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Parser};
use std::borrow::Cow;

fn is_identifier_start(character: char) -> bool {
    character == '_' || character.is_ascii_alphabetic()
}

fn is_identifier_continue(character: char) -> bool {
    character == '_' || character == '#' || character.is_ascii_alphanumeric()
}

fn is_synthetic_continue(character: char) -> bool {
    character == '_' || character == '#' || character == ':' || character.is_ascii_alphanumeric()
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        take_while1(is_identifier_start),
        take_while(is_identifier_continue),
    ))
    .parse(input)
}

fn parse_synthetic_name(input: &str) -> IResult<&str, &str> {
    delimited(char('{'), take_while1(is_synthetic_continue), char('}')).parse(input)
}

fn parse_path_segment_name(input: &str) -> IResult<&str, PathSegmentName<'_>> {
    alt((
        map(parse_synthetic_name, |x| {
            PathSegmentName::Synthetic(Cow::Borrowed(x))
        }),
        map(parse_identifier, |x| {
            PathSegmentName::Identifier(Cow::Borrowed(x))
        }),
    ))
    .parse(input)
}

fn parse_associated_type_binding(input: &str) -> IResult<&str, Token<'_>> {
    map(
        pair(
            parse_identifier,
            preceded(ws(char('=')), parse_type_expression),
        ),
        |(name, value)| Token::AssociatedTypeBinding {
            name,
            value: Box::new(value),
        },
    )
    .parse(input)
}

fn parse_generic_argument(input: &str) -> IResult<&str, Token<'_>> {
    alt((parse_associated_type_binding, parse_type_expression)).parse(input)
}

fn parse_generic_arguments(input: &str) -> IResult<&str, Vec<Token<'_>>> {
    delimited(
        ws(char('<')),
        separated_list0(ws(char(',')), parse_generic_argument),
        ws(char('>')),
    )
    .parse(input)
}

fn parse_segment_generic_arguments(input: &str) -> IResult<&str, Vec<Token<'_>>> {
    alt((
        parse_generic_arguments,
        preceded(ws(tag("::")), parse_generic_arguments),
    ))
    .parse(input)
}

fn parse_path_segment(input: &str) -> IResult<&str, PathSegment<'_>> {
    let (remainder, name) = parse_path_segment_name(input)?;
    let (remainder, generic_args) = opt(parse_segment_generic_arguments).parse(remainder)?;

    Ok((
        remainder,
        PathSegment {
            name,
            generics: generic_args,
        },
    ))
}

fn parse_unit_or_tuple_type(input: &str) -> IResult<&str, Token<'_>> {
    map(
        delimited(
            ws(char('(')),
            separated_list0(ws(char(',')), parse_type_expression),
            ws(char(')')),
        ),
        |elements| {
            if elements.is_empty() {
                Token::Unit
            } else {
                Token::Tuple { elements }
            }
        },
    )
    .parse(input)
}

fn parse_slice_type(input: &str) -> IResult<&str, Token<'_>> {
    map(
        delimited(ws(char('[')), parse_type_expression, ws(char(']'))),
        |element| Token::Slice {
            element: Box::new(element),
        },
    )
    .parse(input)
}

fn parse_reference_type(input: &str) -> IResult<&str, Token<'_>> {
    map(
        preceded(
            ws(char('&')),
            pair(
                opt(preceded(multispace0, tag("mut"))),
                parse_type_expression,
            ),
        ),
        |(mutable_keyword, inner)| Token::Reference {
            mutable: mutable_keyword.is_some(),
            inner: Box::new(inner),
        },
    )
    .parse(input)
}

fn parse_dynamic_trait_object(input: &str) -> IResult<&str, Token<'_>> {
    map(
        preceded(
            ws(tag("dyn")),
            pair(
                parse_regular_path,
                many0(preceded(ws(char('+')), parse_regular_path)),
            ),
        ),
        |(principal_trait, additional_traits)| Token::DynamicTraitObject {
            principal_trait: Box::new(principal_trait),
            additional_traits,
        },
    )
    .parse(input)
}

fn parse_regular_path(input: &str) -> IResult<&str, Token<'_>> {
    map(
        separated_list1(ws(tag("::")), parse_path_segment),
        |segments| Token::Path { segments },
    )
    .parse(input)
}

fn parse_qualified_path(input: &str) -> IResult<&str, Token<'_>> {
    let (remainder, _) = ws(char('<')).parse(input)?;
    let (remainder, qualified_type) = parse_type_expression(remainder)?;
    let (remainder, trait_path) =
        opt(preceded(ws(tag("as")), parse_type_expression)).parse(remainder)?;
    let (remainder, _) = ws(char('>')).parse(remainder)?;
    let (remainder, associated_segments) =
        many0(preceded(ws(tag("::")), parse_path_segment)).parse(remainder)?;

    Ok((
        remainder,
        Token::QualifiedPath {
            qualified_type: Box::new(qualified_type),
            trait_path: trait_path.map(Box::new),
            associated_segments,
        },
    ))
}

fn parse_type_expression(input: &str) -> IResult<&str, Token<'_>> {
    ws(alt((
        parse_reference_type,
        parse_qualified_path,
        parse_dynamic_trait_object,
        parse_unit_or_tuple_type,
        parse_slice_type,
        parse_regular_path,
    )))
    .parse(input)
}

fn ws<'a, O, P>(inner: P) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>
where
    P: Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    delimited(multispace0, inner, multispace0)
}

pub fn parse(input: &str) -> IResult<&str, Token<'_>> {
    all_consuming(ws(parse_type_expression)).parse(input)
}
