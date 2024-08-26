use regex::Regex;

pub trait Parser<T>: Fn(&str) -> Option<(T, &str)> {}
impl<T, F> Parser<T> for F where F: Fn(&str) -> Option<(T, &str)> {}

pub fn digits(s: &str) -> Option<(i64, &str)> {
    // Find the first non-digit character in the string
    let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
    match s[..end].parse() {
        Ok(value) => Some((value, &s[end..])),
        Err(_) => None,
    }
}

pub fn character(c: char) -> impl Parser<()> {
    move |s| {
        let mut chars = s.chars();
        if chars.next() == Some(c) {
            Some(((), chars.as_str()))
        } else {
            None
        }
    }
}

pub fn lexeme<T>(parser: impl Parser<T>) -> impl Parser<T> {
    move |s| {
        let s = s.trim_start();
        parser(s)
    }
}

pub fn string(target: &'static str) -> impl Parser<()> {
    move |s| {
        if s.starts_with(target) {
            Some(((), &s[target.len()..]))
        } else {
            None
        }
    }
}

pub fn map<A, B>(parser: impl Parser<A>, f: impl Fn(A) -> B) -> impl Parser<B> {
    move |s| parser(s).map(|(value, rest)| (f(value), rest))
}

pub fn choice<T>(parser1: impl Parser<T>, parser2: impl Parser<T>) -> impl Parser<T> {
    move |s| parser1(s).or_else(|| parser2(s))
}

#[macro_export]
macro_rules! choice {
    ($parser0:expr, $($parser:expr),*) => {{
        let p = $parser0;
        $(
            let p = $crate::parsers::choice(p, $parser);
        )*
        p
    }};
}

pub fn join<A, B>(parser1: impl Parser<A>, parser2: impl Parser<B>) -> impl Parser<(A, B)> {
    move |s| {
        parser1(s).and_then(|(value1, rest1)| {
            parser2(rest1).map(|(value2, rest2)| ((value1, value2), rest2))
        })
    }
}

#[macro_export]
macro_rules! join {
    ($parser0:expr, $($parser:expr),*) => {{
        let p = $parser0;
        $(
            let p = $crate::parsers::join(p, $parser);
        )*
        p
    }};
}

pub fn many<T>(parser: impl Parser<T>) -> impl Parser<Vec<T>> {
    move |mut s| {
        let mut ret = vec![];
        while let Some((value, rest)) = parser(s) {
            ret.push(value);
            s = rest;
        }
        Some((ret, s))
    }
}

pub fn separated<T>(parser: impl Parser<T>, separator: impl Parser<()>) -> impl Parser<Vec<T>> {
    move |mut s| {
        let mut ret = vec![];

        match parser(s) {
            Some((value, rest)) => {
                ret.push(value);
                s = rest;
            }
            None => {
                return Some((ret, s));
            }
        }

        while let Some((_, rest)) = separator(s) {
            s = rest;
            match parser(s) {
                Some((value, rest)) => {
                    ret.push(value);
                    s = rest;
                }
                None => {
                    return None;
                }
            }
        }

        Some((ret, s))
    }
}

pub fn regex<'a, T>(re: &'a Regex, f: impl Fn(&str) -> Option<T> + 'a) -> impl Parser<T> + 'a {
    move |s| {
        re.find(s).and_then(|matched| {
            f(matched.as_str()).map(|value| {
                let rest = &s[matched.end()..];
                (value, rest)
            })
        })
    }
}

#[macro_export]
macro_rules! regex {
    ($pattern:expr, $f:expr) => {{
        use once_cell::sync::Lazy;
        use regex::Regex;
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new($pattern).unwrap());
        $crate::parsers::regex(&RE, $f)
    }};
}
