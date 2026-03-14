use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, multispace0},
    combinator::{all_consuming, map, opt, recognize},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded},
};
use std::borrow::Cow;
use std::fmt;

macro_rules! ps {
    ($name:pat) => {
        PathSegment {
            name: PathSegmentName::Identifier(Cow::Borrowed($name)),
            generic_args: None,
        }
    };
    ($name:pat, $generic_args:pat) => {
        PathSegment {
            name: PathSegmentName::Identifier(Cow::Borrowed($name)),
            generic_args: Some($generic_args),
        }
    };
}

macro_rules! sps {
    ($name:literal) => {
        PathSegment {
            name: PathSegmentName::Synthetic(Cow::Borrowed($name)),
            generic_args: None,
        }
    };
    ($name:literal, $generic_args:pat) => {
        PathSegment {
            name: PathSegmentName::Synthetic(Cow::Borrowed($name)),
            generic_args: Some($generic_args),
        }
    };
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token<'a> {
    Path {
        segments: Vec<PathSegment<'a>>,
    },

    QualifiedPath {
        qualified_type: Box<Token<'a>>,
        trait_path: Option<Box<Token<'a>>>,
        associated_segments: Vec<PathSegment<'a>>,
    },

    Tuple {
        elements: Vec<Token<'a>>,
    },

    Slice {
        element: Box<Token<'a>>,
    },

    Reference {
        mutable: bool,
        inner: Box<Token<'a>>,
    },

    DynamicTraitObject {
        principal_trait: Box<Token<'a>>,
        additional_traits: Vec<Token<'a>>,
    },

    AssociatedTypeBinding {
        name: &'a str,
        value: Box<Token<'a>>,
    },

    Unit,
}

impl Token<'_> {
    fn visit(&mut self, fun: &mut impl FnMut(&mut Self) -> usize) -> usize {
        let mut count = 0;
        match self {
            Token::Path { segments } => {
                for segment in segments {
                    if let Some(generic_args) = &mut segment.generic_args {
                        for arg in generic_args {
                            count += arg.visit(fun);
                        }
                    }
                }
            }

            Token::QualifiedPath {
                qualified_type,
                trait_path,
                associated_segments,
            } => {
                count += qualified_type.visit(fun);

                if let Some(trait_path) = trait_path {
                    count += trait_path.visit(fun);
                }

                for segment in associated_segments {
                    if let Some(generic_args) = &mut segment.generic_args {
                        for arg in generic_args {
                            count += arg.visit(fun);
                        }
                    }
                }
            }

            Token::Tuple { elements } => {
                for element in elements {
                    count += element.visit(fun);
                }
            }

            Token::Slice { element } => {
                count += element.visit(fun);
            }

            Token::Reference { inner, .. } => {
                count += inner.visit(fun);
            }

            Token::DynamicTraitObject {
                principal_trait,
                additional_traits,
            } => {
                count += principal_trait.visit(fun);
                for additional_trait in additional_traits {
                    count += additional_trait.visit(fun);
                }
            }

            Token::AssociatedTypeBinding { value, .. } => {
                count += value.visit(fun);
            }

            Token::Unit => {}
        }

        count + fun(self)
    }

    fn visit_segments(
        &mut self,
        fun: &mut impl FnMut(&mut Vec<PathSegment<'_>>) -> usize,
    ) -> usize {
        fn apply_segment_generics(
            segments: &mut Vec<PathSegment<'_>>,
            fun: &mut impl FnMut(&mut Vec<PathSegment<'_>>) -> usize,
        ) -> usize {
            let mut count = 0;

            for segment in segments.iter_mut() {
                if let Some(generic_args) = &mut segment.generic_args {
                    for arg in generic_args {
                        count += arg.visit_segments(fun);
                    }
                }
            }

            count + fun(segments)
        }

        match self {
            Token::Path { segments } => apply_segment_generics(segments, fun),
            Token::QualifiedPath {
                qualified_type,
                trait_path,
                associated_segments,
            } => {
                let mut count = 0;
                count += qualified_type.visit_segments(fun);

                if let Some(trait_path) = trait_path {
                    count += trait_path.visit_segments(fun);
                }

                count + apply_segment_generics(associated_segments, fun)
            }
            Token::Tuple { elements } => {
                let mut count = 0;
                for element in elements {
                    count += element.visit_segments(fun);
                }
                count
            }
            Token::Slice { element } => element.visit_segments(fun),
            Token::Reference { inner, .. } => inner.visit_segments(fun),
            Token::DynamicTraitObject {
                principal_trait,
                additional_traits,
            } => {
                let mut count = 0;
                count += principal_trait.visit_segments(fun);
                for additional_trait in additional_traits {
                    count += additional_trait.visit_segments(fun);
                }
                count
            }
            Token::AssociatedTypeBinding { value, .. } => value.visit_segments(fun),
            Token::Unit => 0,
        }
    }

    pub fn erase_trait_names(&mut self) -> usize {
        self.visit(&mut |token| {
            if let Token::QualifiedPath { trait_path, .. } = token {
                trait_path.take();
                1
            } else {
                0
            }
        })
    }

    pub fn downgrade_qpath(&mut self) -> usize {
        self.visit(&mut |token| {
            let Token::QualifiedPath {
                trait_path,
                qualified_type,
                associated_segments,
            } = token
            else {
                return 0;
            };

            if trait_path.is_some() {
                return 0;
            }

            let Token::Path { segments } = qualified_type.as_mut() else {
                return 0;
            };

            segments.append(associated_segments);

            let segments = std::mem::take(segments);
            *token = Token::Path { segments };

            1
        })
    }

    pub fn unwrap_result(&mut self) -> usize {
        self.visit(&mut |token| {
            let Token::Path { segments } = token else {
                return 0;
            };

            let [ps!("core"), ps!("result"), ps!("Result", generic_args)] = segments.as_mut_slice()
            else {
                return 0;
            };

            let [ok, _err] = generic_args.as_mut_slice() else {
                return 0;
            };

            *token = std::mem::replace(ok, Token::Unit);
            1
        })
    }

    pub fn unwrap_option(&mut self) -> usize {
        self.visit(&mut |token| {
            let Token::Path { segments } = token else {
                return 0;
            };

            let [ps!("core"), ps!("option"), ps!("Option", generic_args)] = segments.as_mut_slice()
            else {
                return 0;
            };

            let [inner] = generic_args.as_mut_slice() else {
                return 0;
            };

            *token = std::mem::replace(inner, Token::Unit);
            1
        })
    }

    pub fn unwrap_pin(&mut self) -> usize {
        self.visit(&mut |token| {
            let Token::Path { segments } = token else {
                return 0;
            };

            let [ps!("core" | "std"), ps!("pin"), ps!("Pin", generic_args)] =
                segments.as_mut_slice()
            else {
                return 0;
            };

            let [inner] = generic_args.as_mut_slice() else {
                return 0;
            };

            *token = std::mem::replace(inner, Token::Unit);
            1
        })
    }

    pub fn collapse_closures(&mut self) -> usize {
        fn idx<'a>(segment: &'a PathSegment<'a>) -> Option<&'a str> {
            let PathSegmentName::Synthetic(name) = &segment.name else {
                return None;
            };

            name.strip_prefix("closure#")
        }

        self.visit_segments(&mut |segments| {
            let mut changed = 0;
            let mut left = 0;

            while left < segments.len() {
                let Some(first_index) = idx(&segments[left]) else {
                    left += 1;
                    continue;
                };

                let mut right = left + 1;
                let mut indices = vec![first_index];

                while right < segments.len() {
                    let Some(index) = idx(&segments[right]) else {
                        break;
                    };
                    indices.push(index);
                    right += 1;
                }

                if indices.len() < 2 {
                    left += 1;
                    continue;
                }

                let merged = format!("closures#{{{}}}", indices.join(","));

                segments.splice(
                    left..right,
                    [PathSegment {
                        name: PathSegmentName::Synthetic(Cow::Owned(merged)),
                        generic_args: None,
                    }],
                );

                changed += 1;
                left += 1;
            }

            changed
        })
    }

    fn is_marker_trait(&self) -> bool {
        let Token::Path { segments } = self else {
            return false;
        };

        let [prefix, marker, trait_name] = segments.as_slice() else {
            return false;
        };

        if !matches!(prefix.ident(), Some("core" | "std")) {
            return false;
        }

        if !marker.is_ident("marker") {
            return false;
        }

        trait_name.generic_args.is_none()
    }

    pub fn erase_marker_traits(&mut self) -> usize {
        self.visit(&mut |token| {
            let Token::DynamicTraitObject {
                principal_trait,
                additional_traits,
            } = token
            else {
                return 0;
            };

            let mut changed = 0;

            additional_traits.retain(|t| {
                let keep = !t.is_marker_trait();
                changed += usize::from(!keep);
                keep
            });

            if principal_trait.is_marker_trait() {
                changed += 1;

                if additional_traits.is_empty() {
                    *token = Token::Unit;
                    return changed;
                }

                let new_principal = additional_traits.remove(0);
                *principal_trait = Box::new(new_principal);
            }

            changed
        })
    }

    pub fn erase_trait_objects(&mut self) -> usize {
        self.visit(&mut |token| {
            let Token::DynamicTraitObject {
                principal_trait, ..
            } = token
            else {
                return 0;
            };

            *token = std::mem::replace(principal_trait.as_mut(), Token::Unit);
            1
        })
    }

    pub fn erase_single_type_binding(&mut self) -> usize {
        self.visit(&mut |token| {
            let Token::Path { segments } = token else {
                return 0;
            };

            let mut count = 0;
            for segment in segments {
                let Some(generic_args) = &mut segment.generic_args else {
                    continue;
                };

                let [Token::AssociatedTypeBinding { value, .. }] = generic_args.as_mut_slice()
                else {
                    continue;
                };

                segment.generic_args = Some(vec![std::mem::replace(value.as_mut(), Token::Unit)]);
                count += 1
            }

            count
        })
    }

    fn erase_path_prefix(segments: &mut Vec<PathSegment<'_>>) -> bool {
        fn matches_path(segments: &[PathSegment<'_>], expected: &[&str]) -> bool {
            segments.len() == expected.len()
                && segments
                    .iter()
                    .zip(expected)
                    .all(|(segment, expected)| segment.is_ident(expected))
        }

        fn strip_prefix(segments: &mut Vec<PathSegment<'_>>) -> bool {
            if segments.len() <= 1 {
                return false;
            }

            segments.drain(..segments.len() - 1);
            true
        }

        let Some(last) = segments.last().and_then(PathSegment::ident) else {
            return false;
        };

        let matched = match last {
            "Vec" => {
                matches_path(segments, &["std", "vec", "Vec"])
                    || matches_path(segments, &["alloc", "vec", "Vec"])
            }

            "String" => {
                matches_path(segments, &["std", "string", "String"])
                    || matches_path(segments, &["alloc", "string", "String"])
            }

            "str" => matches_path(segments, &["core", "primitive", "str"]),

            "Option" => {
                matches_path(segments, &["std", "option", "Option"])
                    || matches_path(segments, &["core", "option", "Option"])
            }

            "Result" => {
                matches_path(segments, &["std", "result", "Result"])
                    || matches_path(segments, &["core", "result", "Result"])
            }

            "Box" => {
                matches_path(segments, &["std", "boxed", "Box"])
                    || matches_path(segments, &["alloc", "boxed", "Box"])
            }

            "HashMap" => matches_path(segments, &["std", "collections", "HashMap"]),
            "HashSet" => matches_path(segments, &["std", "collections", "HashSet"]),
            "BTreeMap" => matches_path(segments, &["std", "collections", "BTreeMap"]),
            "BTreeSet" => matches_path(segments, &["std", "collections", "BTreeSet"]),
            "VecDeque" => matches_path(segments, &["std", "collections", "VecDeque"]),
            "BinaryHeap" => matches_path(segments, &["std", "collections", "BinaryHeap"]),
            "LinkedList" => matches_path(segments, &["std", "collections", "LinkedList"]),

            "Rc" => matches_path(segments, &["std", "rc", "Rc"]),
            "Arc" => matches_path(segments, &["std", "sync", "Arc"]),

            "Cell" => matches_path(segments, &["std", "cell", "Cell"]),
            "RefCell" => matches_path(segments, &["std", "cell", "RefCell"]),
            "UnsafeCell" => matches_path(segments, &["std", "cell", "UnsafeCell"]),

            "Mutex" => matches_path(segments, &["std", "sync", "Mutex"]),
            "RwLock" => matches_path(segments, &["std", "sync", "RwLock"]),
            "OnceLock" => matches_path(segments, &["std", "sync", "OnceLock"]),
            "LazyLock" => matches_path(segments, &["std", "sync", "LazyLock"]),

            "Cow" => {
                matches_path(segments, &["std", "borrow", "Cow"])
                    || matches_path(segments, &["alloc", "borrow", "Cow"])
            }

            "Path" => matches_path(segments, &["std", "path", "Path"]),
            "PathBuf" => matches_path(segments, &["std", "path", "PathBuf"]),

            "OsStr" => matches_path(segments, &["std", "ffi", "OsStr"]),
            "OsString" => matches_path(segments, &["std", "ffi", "OsString"]),
            "CStr" => matches_path(segments, &["std", "ffi", "CStr"]),
            "CString" => matches_path(segments, &["std", "ffi", "CString"]),

            "Pin" => {
                matches_path(segments, &["std", "pin", "Pin"])
                    || matches_path(segments, &["core", "pin", "Pin"])
            }

            "Duration" => matches_path(segments, &["std", "time", "Duration"]),
            "Instant" => matches_path(segments, &["std", "time", "Instant"]),
            "SystemTime" => matches_path(segments, &["std", "time", "SystemTime"]),

            "Range" => matches_path(segments, &["std", "ops", "Range"]),
            "RangeInclusive" => matches_path(segments, &["std", "ops", "RangeInclusive"]),
            "Bound" => matches_path(segments, &["std", "ops", "Bound"]),

            "IpAddr" => matches_path(segments, &["std", "net", "IpAddr"]),
            "Ipv4Addr" => matches_path(segments, &["std", "net", "Ipv4Addr"]),
            "Ipv6Addr" => matches_path(segments, &["std", "net", "Ipv6Addr"]),
            "SocketAddr" => matches_path(segments, &["std", "net", "SocketAddr"]),

            "NonZeroUsize" => matches_path(segments, &["std", "num", "NonZeroUsize"]),
            "NonZeroU64" => matches_path(segments, &["std", "num", "NonZeroU64"]),

            "NonNull" => matches_path(segments, &["std", "ptr", "NonNull"]),
            "PhantomData" => matches_path(segments, &["std", "marker", "PhantomData"]),
            "MaybeUninit" => matches_path(segments, &["std", "mem", "MaybeUninit"]),

            "Poll" => {
                matches_path(segments, &["std", "task", "Poll"])
                    || matches_path(segments, &["core", "task", "Poll"])
            }

            "Context" => {
                matches_path(segments, &["std", "task", "Context"])
                    || matches_path(segments, &["core", "task", "Context"])
            }

            "Waker" => {
                matches_path(segments, &["std", "task", "Waker"])
                    || matches_path(segments, &["core", "task", "Waker"])
            }

            "Future" => {
                matches_path(segments, &["std", "future", "Future"])
                    || matches_path(segments, &["core", "future", "future", "Future"])
            }

            "Sender" => matches_path(segments, &["std", "sync", "mpsc", "Sender"]),
            "Receiver" => matches_path(segments, &["std", "sync", "mpsc", "Receiver"]),

            _ => false,
        };

        matched && strip_prefix(segments)
    }

    pub fn erase_well_known_paths(&mut self) -> usize {
        self.visit_segments(&mut |segments| usize::from(Self::erase_path_prefix(segments)))
    }

    fn is_placeholder_type(&self) -> bool {
        let Token::Path { segments } = self else {
            return false;
        };

        matches!(segments.as_slice(), [segment] if segment.is_ident("_") && segment.generic_args.is_none())
    }

    pub fn erase_placeholder_generics(&mut self) -> usize {
        self.visit(&mut |token| {
            let Token::Path { segments } = token else {
                return 0;
            };

            let mut changed = 0;

            for segment in segments {
                let Some(generic_args) = &segment.generic_args else {
                    continue;
                };

                if matches!(generic_args.as_slice(), [arg] if arg.is_placeholder_type()) {
                    segment.generic_args = None;
                    changed += 1;
                }
            }

            changed
        })
    }

    pub fn erase_all(&mut self) -> usize {
        self.erase_trait_names()
            + self.downgrade_qpath()
            + self.unwrap_result()
            + self.unwrap_option()
            + self.collapse_closures()
            + self.erase_marker_traits()
            + self.erase_trait_objects()
            + self.erase_single_type_binding()
            + self.unwrap_pin()
            + self.erase_well_known_paths()
            + self.erase_placeholder_generics()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PathSegment<'a> {
    pub name: PathSegmentName<'a>,
    pub generic_args: Option<Vec<Token<'a>>>,
}

impl<'a> PathSegment<'a> {
    pub fn ident(&'a self) -> Option<&'a str> {
        match &self.name {
            PathSegmentName::Identifier(name) => Some(name),
            PathSegmentName::Synthetic(_) => None,
        }
    }

    pub fn is_ident(&self, expected: &str) -> bool {
        self.ident() == Some(expected)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PathSegmentName<'a> {
    Identifier(Cow<'a, str>),
    Synthetic(Cow<'a, str>),
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Path { segments } => {
                for (index, segment) in segments.iter().enumerate() {
                    if index > 0 {
                        write!(formatter, "::")?;
                    }
                    write!(formatter, "{segment}")?;
                }
                Ok(())
            }

            Token::QualifiedPath {
                qualified_type,
                trait_path,
                associated_segments,
            } => {
                write!(formatter, "<{}", qualified_type)?;

                if let Some(trait_path) = trait_path {
                    write!(formatter, " as {}", trait_path)?;
                }

                write!(formatter, ">")?;

                for segment in associated_segments {
                    write!(formatter, "::{}", segment)?;
                }

                Ok(())
            }

            Token::Tuple { elements } => {
                write!(formatter, "(")?;
                for (index, element) in elements.iter().enumerate() {
                    if index > 0 {
                        write!(formatter, ", ")?;
                    }
                    write!(formatter, "{element}")?;
                }
                write!(formatter, ")")
            }

            Token::Slice { element } => {
                write!(formatter, "[{}]", element)
            }

            Token::Reference { mutable, inner } => {
                if *mutable {
                    write!(formatter, "&mut {}", inner)
                } else {
                    write!(formatter, "&{}", inner)
                }
            }

            Token::DynamicTraitObject {
                principal_trait,
                additional_traits,
            } => {
                write!(formatter, "dyn {}", principal_trait)?;
                for additional_trait in additional_traits {
                    write!(formatter, " + {}", additional_trait)?;
                }
                Ok(())
            }

            Token::AssociatedTypeBinding { name, value } => {
                write!(formatter, "{} = {}", name, value)
            }

            Token::Unit => write!(formatter, "()"),
        }
    }
}

impl fmt::Display for PathSegment<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}", self.name)?;

        if let Some(generic_args) = &self.generic_args {
            write!(formatter, "<")?;
            for (index, arg) in generic_args.iter().enumerate() {
                if index > 0 {
                    write!(formatter, ", ")?;
                }
                write!(formatter, "{arg}")?;
            }
            write!(formatter, ">")?;
        }

        Ok(())
    }
}

impl fmt::Display for PathSegmentName<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathSegmentName::Identifier(name) => write!(formatter, "{name}"),
            PathSegmentName::Synthetic(name) => write!(formatter, "{{{name}}}"),
        }
    }
}

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

    Ok((remainder, PathSegment { name, generic_args }))
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

pub fn parse(input: &str) -> IResult<&str, Token<'_>> {
    all_consuming(ws(parse_type_expression)).parse(input)
}

fn ws<'a, O, P>(inner: P) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>
where
    P: Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    delimited(multispace0, inner, multispace0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use testresult::TestResult;

    fn roundtrip(s: &str) -> TestResult {
        let (_, token) = parse(s)?;
        let repr = token.to_string();
        println!("{token:#?}");
        let token2 = parse(repr.as_str())?.1;
        assert_eq!(token, token2, "Not equal: `{:?}` vs `{:?}`", token, token2);
        Ok(())
    }

    #[test]
    fn turbofish() -> TestResult {
        let t = r#"
            <gsym::writer::Writer>::insert_file::<&str, &str>
        "#;

        roundtrip(t)?;

        let (_, mut token) = parse(t)?;
        token.erase_all();

        assert_eq!(
            format!("{token}"),
            "gsym::writer::Writer::insert_file<&str, &str>"
        );

        Ok(())
    }

    #[test]
    fn t1() -> TestResult {
        let t = r#"
<
    std::
    thread::
    lifecycle::
    spawn_unchecked<
        <
            rayon_core::
            registry::
            DefaultSpawn as rayon_core::registry::ThreadSpawn
        >::spawn::{closure#0},
        ()
    >::{closure#1} as core::ops::function::FnOnce<
        ()
    >
>
::call_once
::{shim:vtable#0}
        "#;

        roundtrip(t)
    }

    #[test]
    fn t3() -> TestResult {
        let t = r#"
            <indexmap::map::IndexMap<gsym::types::FileInfo, ()>>::insert_full
        "#;
        roundtrip(t)?;

        let (_, mut token) = parse(t)?;
        token.erase_trait_names();
        token.downgrade_qpath();

        assert_eq!(
            format!("{token}"),
            "indexmap::map::IndexMap<gsym::types::FileInfo, ()>::insert_full"
        );

        Ok(())
    }

    #[test]
    fn t4() -> TestResult {
        let t = r#"
<
    gimli::read::unit::EntriesCursor<
        gimli::read::endian_slice::EndianSlice<
            gimli::endianity::LittleEndian
        >
    >
>
::next_entry
        "#;

        roundtrip(t)
    }

    #[test]
    fn t5() -> TestResult {
        let t = r#"
            <symblib::symbconv::dwarf::Extractor as symblib::symbconv::RangeExtractor>::extract
        "#;

        roundtrip(t)?;

        let (_, mut token) = parse(t)?;
        token.erase_trait_names();
        token.downgrade_qpath();

        assert_eq!(
            format!("{token}"),
            "symblib::symbconv::dwarf::Extractor::extract"
        );

        Ok(())
    }

    #[test]
    fn t6() -> TestResult {
        let t = r#"
core
::slice
::sort
::unstable
::quicksort
::quicksort
::<
    intervaltree::Node<
        u64,
         (
            symblib::dwarf::SourceFile,
             core::option::Option<
                core::num::nonzero::NonZero<
                    u64
                >
            >
        )
    >,
     <
        [
            intervaltree::Node<
                u64,
                 (
                    symblib::dwarf::SourceFile,
                     core::option::Option<
                        core::num::nonzero::NonZero<
                            u64
                        >
                    >
                )
            >
        ]
    >::sort_unstable_by<
        <
            intervaltree::IntervalTree<
                u64,
                 (
                    symblib::dwarf::SourceFile,
                     core::option::Option<
                        core::num::nonzero::NonZero<
                            u64
                        >
                    >
                )
            > as core::iter::traits::collect::FromIterator<
                intervaltree::Element<
                    u64,
                     (
                        symblib::dwarf::SourceFile,
                         core::option::Option<
                            core::num::nonzero::NonZero<
                                u64
                            >
                        >
                    )
                >
            >
        >::from_iter<
            alloc::vec::Vec<
                intervaltree::Element<
                    u64,
                     (
                        symblib::dwarf::SourceFile,
                         core::option::Option<
                            core::num::nonzero::NonZero<
                                u64
                            >
                        >
                    )
                >
            >
        >::{closure#2}
    >::{closure#0}
>
        "#;

        roundtrip(t)
    }

    #[test]
    fn t7() -> TestResult {
        let t = r#"
<
    core::iter::adapters::GenericShunt<
        fallible_iterator::Iterator<
            fallible_iterator::Map<
                fallible_iterator::Filter<
                    symblib::dwarf::LineIter,
                     symblib::symbconv::dwarf::process_unit<
                        &mut <
                            symblib::symbconv::dwarf::Extractor as symblib::symbconv::RangeExtractor
                        >::extract::{closure#0}
                    >::{closure#0}
                >,
                 symblib::symbconv::dwarf::process_unit<
                    &mut <
                        symblib::symbconv::dwarf::Extractor as symblib::symbconv::RangeExtractor
                    >::extract::{closure#0}
                >::{closure#1}
            >
        >,
         core::result::Result<
            core::convert::Infallible,
             symblib::dwarf::Error
        >
    > as core::iter::traits::iterator::Iterator
>
::next
        "#;

        roundtrip(t)
    }

    #[test]
    fn t8() -> TestResult {
        let t = r#"
rayon_core
::scope
::scope
::<
    <
        svclib::taskutil::ThreadPool
    >::spawn<
        <
            symdb::symdb::SymDb
        >::fetch_from_debuginfod::{closure#0}::{closure#0}::{closure#0}::{closure#0},
         gsym::writer::Writer,
         anyhow::Error
    >::{closure#0}::{closure#0}::{closure#0},
     core::result::Result<
        gsym::writer::Writer,
         anyhow::Error
    >
>
::{closure#0}
        "#;

        roundtrip(t)?;

        let (_, mut token) = parse(t)?;
        token.erase_trait_names();
        token.downgrade_qpath();
        token.unwrap_result();
        token.collapse_closures();

        assert_eq!(
            format!("{token}"),
            "rayon_core::scope::scope<\
                svclib::taskutil::ThreadPool::spawn<\
                    symdb::symdb::SymDb::fetch_from_debuginfod::{closures#{0,0,0,0}}, \
                    gsym::writer::Writer, \
                    anyhow::Error\
                >::{closures#{0,0,0}}, gsym::writer::Writer\
            >::{closure#0}"
        );

        Ok(())
    }

    #[test]
    fn t9() -> TestResult {
        let t = r#"
core
::iter
::adapters
::try_process
::<
    fallible_iterator::Iterator<
        fallible_iterator::Map<
            fallible_iterator::Filter<
                symblib::dwarf::LineIter,
                 symblib::symbconv::dwarf::process_unit<
                    &mut <
                        symblib::symbconv::dwarf::Extractor as symblib::symbconv::RangeExtractor
                    >::extract::{closure#0}
                >::{closure#0}
            >,
             symblib::symbconv::dwarf::process_unit<
                &mut <
                    symblib::symbconv::dwarf::Extractor as symblib::symbconv::RangeExtractor
                >::extract::{closure#0}
            >::{closure#1}
        >
    >,
     intervaltree::Element<
        u64,
         (
            symblib::dwarf::SourceFile,
             core::option::Option<
                core::num::nonzero::NonZero<
                    u64
                >
            >
        )
    >,
     core::result::Result<
        core::convert::Infallible,
         symblib::dwarf::Error
    >,
     <
        core::result::Result<
            alloc::vec::Vec<
                intervaltree::Element<
                    u64,
                     (
                        symblib::dwarf::SourceFile,
                         core::option::Option<
                            core::num::nonzero::NonZero<
                                u64
                            >
                        >
                    )
                >
            >,
             symblib::dwarf::Error
        > as core::iter::traits::collect::FromIterator<
            core::result::Result<
                intervaltree::Element<
                    u64,
                     (
                        symblib::dwarf::SourceFile,
                         core::option::Option<
                            core::num::nonzero::NonZero<
                                u64
                            >
                        >
                    )
                >,
                 symblib::dwarf::Error
            >
        >
    >::from_iter<
        fallible_iterator::Iterator<
            fallible_iterator::Map<
                fallible_iterator::Filter<
                    symblib::dwarf::LineIter,
                     symblib::symbconv::dwarf::process_unit<
                        &mut <
                            symblib::symbconv::dwarf::Extractor as symblib::symbconv::RangeExtractor
                        >::extract::{closure#0}
                    >::{closure#0}
                >,
                 symblib::symbconv::dwarf::process_unit<
                    &mut <
                        symblib::symbconv::dwarf::Extractor as symblib::symbconv::RangeExtractor
                    >::extract::{closure#0}
                >::{closure#1}
            >
        >
    >::{closure#0},
     alloc::vec::Vec<
        intervaltree::Element<
            u64,
             (
                symblib::dwarf::SourceFile,
                 core::option::Option<
                    core::num::nonzero::NonZero<
                        u64
                    >
                >
            )
        >
    >
>
        "#;

        roundtrip(t)
    }

    #[test]
    fn dyn_example() -> TestResult {
        let t = r#"
<
    tonic::transport::server::SvcFuture<
        tonic::service::recover_error::ResponseFuture<
            tower::util::either::EitherResponseFuture<
                tower::load_shed::future::ResponseFuture<
                    tower::util::either::EitherResponseFuture<
                        tower::limit::concurrency::future::ResponseFuture<
                            tonic::transport::service::grpc_timeout::ResponseFuture<
                                tower::limit::concurrency::future::ResponseFuture<
                                    core::pin::Pin<
                                        alloc::boxed::Box<
                                            dyn core::future::future::Future<
                                                Output = core::result::Result<
                                                    http::response::Response<
                                                        tonic::body::Body
                                                    >,
                                                     core::convert::Infallible
                                                >
                                            > + core::marker::Send
                                        >
                                    >
                                >
                            >
                        >,
                         tonic::transport::service::grpc_timeout::ResponseFuture<
                            tower::limit::concurrency::future::ResponseFuture<
                                core::pin::Pin<
                                    alloc::boxed::Box<
                                        dyn core::future::future::Future<
                                            Output = core::result::Result<
                                                http::response::Response<
                                                    tonic::body::Body
                                                >,
                                                 core::convert::Infallible
                                            >
                                        > + core::marker::Send
                                    >
                                >
                            >
                        >
                    >
                >,
                 tower::util::either::EitherResponseFuture<
                    tower::limit::concurrency::future::ResponseFuture<
                        tonic::transport::service::grpc_timeout::ResponseFuture<
                            tower::limit::concurrency::future::ResponseFuture<
                                core::pin::Pin<
                                    alloc::boxed::Box<
                                        dyn core::future::future::Future<
                                            Output = core::result::Result<
                                                http::response::Response<
                                                    tonic::body::Body
                                                >,
                                                 core::convert::Infallible
                                            >
                                        > + core::marker::Send
                                    >
                                >
                            >
                        >
                    >,
                     tonic::transport::service::grpc_timeout::ResponseFuture<
                        tower::limit::concurrency::future::ResponseFuture<
                            core::pin::Pin<
                                alloc::boxed::Box<
                                    dyn core::future::future::Future<
                                        Output = core::result::Result<
                                            http::response::Response<
                                                tonic::body::Body
                                            >,
                                             core::convert::Infallible
                                        >
                                    > + core::marker::Send
                                >
                            >
                        >
                    >
                >
            >
        >
    > as core::future::future::Future
>
::poll
        "#;

        roundtrip(t)?;

        let (_, mut token) = parse(t)?;
        token.erase_all();

        println!("{token}");

        Ok(())
    }

    #[test]
    fn erase_option_type_test() -> TestResult {
        let t = r#"
            core::option::Option<core::num::nonzero::NonZero<u64>>
        "#;

        let (_, mut token) = parse(t)?;
        token.unwrap_option();

        assert_eq!(format!("{token}"), "core::num::nonzero::NonZero<u64>");
        Ok(())
    }

    #[test]
    fn erase_marker() -> TestResult {
        let t = r#"
<
    tracing::instrument::Instrumented<
        <
            alloc::boxed::Box<
                dyn objstore::Bucket + core::marker::Sync + core::marker::Send
            > as objstore::BucketExt
        >::get_object_into_tempfile::{closure#0}::{closure#0}
    > as core::future::future::Future
>
::poll
        "#;

        let (_, mut token) = parse(t)?;
        token.erase_all();

        assert_eq!(
            format!("{token}"),
            "tracing::instrument::Instrumented<\
                alloc::boxed::Box<\
                    objstore::Bucket\
                >::get_object_into_tempfile::{closures#{0,0}}\
            >::poll"
        );

        Ok(())
    }

    #[test]
    fn unwrap_future() -> TestResult {
        let t = r#"
        <
axum::util::MapIntoResponseFuture<
        core::pin::Pin<
            alloc::boxed::Box<
                dyn core::future::future::Future<
                    Output = core::result::Result<
                        http::response::Response<
                            tonic::body::Body
                        >,
                         core::convert::Infallible
                    >
                > + core::marker::Send
            >
        >
    > as core::future::future::Future
>
::poll
        "#;

        let (_, mut token) = parse(t)?;
        token.erase_all();

        assert_eq!(
            format!("{token}"),
            "axum::util::MapIntoResponseFuture<Box<Future<http::response::Response<tonic::body::Body>>>>::poll"
        );

        Ok(())
    }

    #[test]
    fn other_sample() -> TestResult {
        let t = r#"
<
    <
        zt::symdb::v1::sym_db_server::SymDbServer<
            _
        > as tower_service::Service<
            http::request::Request<
                _
            >
        >
    >::call::FetchGlobalBinariesSvc<
        symdb::grpc::ServiceImpl
    > as tonic::server::service::UnaryService<
        zt::symdb::v1::FetchGlobalBinariesRequest
    >
>
::call
::{closure#0}
        "#;

        let (_, mut token) = parse(t)?;
        token.erase_all();

        assert_eq!(
            format!("{token}"),
            "zt::symdb::v1::sym_db_server::SymDbServer::call::FetchGlobalBinariesSvc<symdb::grpc::ServiceImpl>::call::{closure#0}"
        );

        Ok(())
    }
}
