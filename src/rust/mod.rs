mod parse;

use std::borrow::Cow;
use std::fmt;

macro_rules! ps {
    ($name:pat) => {
        PathSegment {
            name: PathSegmentName::Identifier(Cow::Borrowed($name)),
            generics: None,
        }
    };
    ($name:pat, $generic_args:pat) => {
        PathSegment {
            name: PathSegmentName::Identifier(Cow::Borrowed($name)),
            generics: Some($generic_args),
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

impl<'a> TryFrom<&'a str> for Token<'a> {
    type Error = nom::Err<nom::error::Error<&'a str>>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let (_, res) = parse::parse(value)?;
        Ok(res)
    }
}

impl<'a> Token<'a> {
    fn visit(&mut self, fun: &mut impl FnMut(&mut Self) -> usize) -> usize {
        let mut count = 0;
        match self {
            Token::Path { segments } => {
                for segment in segments {
                    if let Some(generic_args) = &mut segment.generics {
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
                    if let Some(generic_args) = &mut segment.generics {
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
                if let Some(generic_args) = &mut segment.generics {
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
                        generics: None,
                    }],
                );

                changed += 1;
                left += 1;
            }

            changed
        })
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
                let Some(generic_args) = &mut segment.generics else {
                    continue;
                };

                let [Token::AssociatedTypeBinding { value, .. }] = generic_args.as_mut_slice()
                else {
                    continue;
                };

                segment.generics = Some(vec![std::mem::replace(value.as_mut(), Token::Unit)]);
                count += 1
            }

            count
        })
    }

    pub fn erase_placeholder_generics(&mut self) -> usize {
        self.visit(&mut |token| {
            let Token::Path { segments } = token else {
                return 0;
            };

            let mut changed = 0;

            for segment in segments {
                let Some(generic_args) = &segment.generics else {
                    continue;
                };

                if matches!(generic_args.as_slice(), [arg] if arg.is_placeholder_type()) {
                    segment.generics = None;
                    changed += 1;
                }
            }

            changed
        })
    }

    fn chain_args(&self) -> Option<&[Token<'a>]> {
        let Token::Path { segments } = self else {
            return None;
        };

        let [segment] = segments.as_slice() else {
            return None;
        };

        if !segment.is_ident("CHAIN") {
            return None;
        }

        segment.generics.as_deref()
    }

    fn split_single_arg_path(&self) -> Option<(Token<'a>, &Token<'a>, Vec<PathSegment<'a>>)> {
        let Token::Path { segments } = self else {
            return None;
        };

        let mut generic_segments = segments
            .iter()
            .enumerate()
            .filter(|(_, segment)| segment.generics.is_some());

        let (split_index, split_segment) = generic_segments.next()?;

        if generic_segments.next().is_some() {
            return None;
        }

        let Some([only_generic]) = split_segment.generics.as_deref() else {
            return None;
        };

        Some((
            Token::Path {
                segments: segments[..split_index]
                    .iter()
                    .cloned()
                    .chain(std::iter::once(PathSegment {
                        name: split_segment.name.clone(),
                        generics: None,
                    }))
                    .collect(),
            },
            only_generic,
            segments[split_index + 1..].to_vec(),
        ))
    }

    pub fn collapse_single_arg_chain(&mut self) -> usize {
        self.visit(&mut |token| {
            let mut parts = Vec::new();
            let mut current: &Token<'_> = token;
            let mut outer_suffix: Option<Vec<PathSegment<'a>>> = None;

            while let Some((path_part, inner, suffix)) = current.split_single_arg_path() {
                parts.push(path_part);

                if outer_suffix.is_none() {
                    outer_suffix = Some(suffix);
                }

                current = inner;
            }

            let mut flattened = match current.chain_args() {
                Some(existing) => {
                    parts.extend(existing.iter().cloned());
                    true
                }
                None => false,
            };

            if !flattened {
                if parts.len() < 3 {
                    return 0;
                }

                parts.push(current.clone());
                flattened = true;
            }

            if !flattened {
                return 0;
            }

            let mut new_token = Token::Path {
                segments: vec![PathSegment {
                    name: PathSegmentName::Identifier(Cow::Borrowed("CHAIN")),
                    generics: Some(parts),
                }],
            };

            if let Some(suffix) = outer_suffix {
                let Token::Path { segments } = &mut new_token else {
                    unreachable!();
                };
                segments.extend(suffix);
            }

            *token = new_token;
            1
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
            + self.collapse_single_arg_chain()
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

        trait_name.generics.is_none()
    }

    fn is_placeholder_type(&self) -> bool {
        let Token::Path { segments } = self else {
            return false;
        };

        matches!(segments.as_slice(), [segment] if segment.is_ident("_") && segment.generics.is_none())
    }

    fn matches_path(segments: &[PathSegment<'_>], expected: &[&str]) -> bool {
        segments.len() == expected.len()
            && segments
                .iter()
                .zip(expected.iter().copied())
                .all(|(segment, expected)| segment.is_ident(expected))
    }

    fn erase_path_prefix_in_segments(
        segments: &mut Vec<PathSegment<'_>>,
        paths: &[&[&str]],
    ) -> bool {
        if segments.len() <= 1 {
            return false;
        }

        if !paths
            .iter()
            .copied()
            .any(|path| Self::matches_path(segments, path))
        {
            return false;
        }

        segments.drain(..segments.len() - 1);
        true
    }

    pub fn erase_path_prefixes(&mut self, paths: &[&[&str]]) -> usize {
        self.visit_segments(&mut |segments| {
            usize::from(Self::erase_path_prefix_in_segments(segments, paths))
        })
    }

    pub fn erase_well_known_paths(&mut self) -> usize {
        self.erase_path_prefixes(WELL_KNOWN_PATHS)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PathSegment<'a> {
    pub name: PathSegmentName<'a>,
    pub generics: Option<Vec<Token<'a>>>,
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

        if let Some(generic_args) = &self.generics {
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

const WELL_KNOWN_PATHS: &[&[&str]] = &[
    &["std", "vec", "Vec"],
    &["alloc", "vec", "Vec"],
    &["std", "string", "String"],
    &["alloc", "string", "String"],
    &["core", "primitive", "str"],
    &["std", "option", "Option"],
    &["core", "option", "Option"],
    &["std", "result", "Result"],
    &["core", "result", "Result"],
    &["std", "boxed", "Box"],
    &["alloc", "boxed", "Box"],
    &["std", "collections", "HashMap"],
    &["std", "collections", "HashSet"],
    &["std", "collections", "BTreeMap"],
    &["std", "collections", "BTreeSet"],
    &["std", "collections", "VecDeque"],
    &["std", "collections", "BinaryHeap"],
    &["std", "collections", "LinkedList"],
    &["std", "rc", "Rc"],
    &["std", "sync", "Arc"],
    &["std", "cell", "Cell"],
    &["std", "cell", "RefCell"],
    &["std", "cell", "UnsafeCell"],
    &["std", "sync", "Mutex"],
    &["std", "sync", "RwLock"],
    &["std", "sync", "OnceLock"],
    &["std", "sync", "LazyLock"],
    &["std", "borrow", "Cow"],
    &["alloc", "borrow", "Cow"],
    &["std", "path", "Path"],
    &["std", "path", "PathBuf"],
    &["std", "ffi", "OsStr"],
    &["std", "ffi", "OsString"],
    &["std", "ffi", "CStr"],
    &["std", "ffi", "CString"],
    &["std", "pin", "Pin"],
    &["core", "pin", "Pin"],
    &["std", "time", "Duration"],
    &["std", "time", "Instant"],
    &["std", "time", "SystemTime"],
    &["std", "ops", "Range"],
    &["std", "ops", "RangeInclusive"],
    &["std", "ops", "Bound"],
    &["std", "net", "IpAddr"],
    &["std", "net", "Ipv4Addr"],
    &["std", "net", "Ipv6Addr"],
    &["std", "net", "SocketAddr"],
    &["std", "num", "NonZeroUsize"],
    &["std", "num", "NonZeroU64"],
    &["std", "ptr", "NonNull"],
    &["std", "marker", "PhantomData"],
    &["std", "mem", "MaybeUninit"],
    &["std", "task", "Poll"],
    &["core", "task", "Poll"],
    &["std", "task", "Context"],
    &["core", "task", "Context"],
    &["std", "task", "Waker"],
    &["core", "task", "Waker"],
    &["std", "future", "Future"],
    &["core", "future", "future", "Future"],
    &["std", "sync", "mpsc", "Sender"],
    &["std", "sync", "mpsc", "Receiver"],
];

#[cfg(test)]
mod tests {
    use super::parse::*;
    use testresult::TestResult;

    fn roundtrip(s: &str) -> TestResult<usize> {
        let (_, token) = parse(s)?;
        let repr = token.to_string();
        println!("{token:#?}");
        let token2 = parse(repr.as_str())?.1;
        assert_eq!(token, token2, "Not equal: `{:?}` vs `{:?}`", token, token2);
        Ok(token.to_string().len())
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

        roundtrip(t)?;

        Ok(())
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

        roundtrip(t)?;

        Ok(())
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

        roundtrip(t)?;

        Ok(())
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

        roundtrip(t)?;

        Ok(())
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

        roundtrip(t)?;

        Ok(())
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

        let initial_size = roundtrip(t)? as f64;

        let (_, mut token) = parse(t)?;
        token.erase_all();

        let ratio = token.to_string().len() as f64 / initial_size;
        println!("{}", ratio);

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
            "CHAIN<axum::util::MapIntoResponseFuture, Box, Future, http::response::Response, tonic::body::Body>::poll"
        );

        println!("{:#?}", token);

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
