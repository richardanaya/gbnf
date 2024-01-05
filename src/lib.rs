use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

// Represents a non-terminal symbol in the grammar.
// examples: root, expr, term, ident, ws, num
#[derive(Clone, Debug)]
pub struct NonTerminalSymbol {
    pub name: String,
}

// Represents a terminal symbol in the grammar.
// examples: "=", "a", "b", "c", "1", "2", "3", " ", "true", "false", "\n"
#[derive(Clone, Debug)]
pub struct TerminalSymbol {
    pub value: String,
}

// Represents a production in the grammar.
#[derive(Clone, Debug)]
pub struct Production {
    pub items: Vec<ProductionItem>,
}

#[derive(Clone, Debug)]
pub enum RepetitionType {
    // [a-z]+
    ZeroOrMore,
    // [a-z]*
    OneOrMore,
    // [a-z]?
    ZeroOrOne,
    // [a-z]
    One,
}

#[derive(Clone, Debug)]
pub enum RepetitionItem {
    Character(char),
    Range(char, char),
}

#[derive(Clone, Debug)]
pub struct Repetition {
    pub items: Vec<RepetitionItem>,
    pub repetition_type: RepetitionType,
}

// Represents different types of items that can be part of a production.
#[derive(Clone, Debug)]
pub enum ProductionItem {
    // example: # This is a comment
    Comment(String),
    Terminal(TerminalSymbol),
    NonTerminal(NonTerminalSymbol),
    // example: ( expr "=" ws term "\n" )
    Group(Box<Production>),
    // example: ident | num | "(" ws expr ")" ws
    OneOf(Vec<Production>),
    // examples: [a-z], [a-z0-9_]*, [0-9]+
    Repetition(Repetition),
    // Additional items as necessary
}

// Represents a grammar rule.
// example: root ::= "yes' | "no"
#[derive(Clone, Debug)]
pub struct Rule {
    pub lhs: NonTerminalSymbol,
    pub rhs: Production,
}

// Represents an item in the grammar.
#[derive(Clone, Debug)]
pub enum GrammarItem {
    // example: # This is a comment
    Comment(String),
    Rule(Rule),
}

// Represents the entire grammar.
#[derive(Clone, Debug)]
pub struct Grammar {
    pub items: Vec<GrammarItem>,
}

impl Display for NonTerminalSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Display for Repetition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        for item in &self.items {
            match item {
                RepetitionItem::Character(c) => {
                    s.push(*c);
                }
                RepetitionItem::Range(start, end) => {
                    s.push(*start);
                    s.push('-');
                    s.push(*end);
                }
            }
        }
        match self.repetition_type {
            RepetitionType::ZeroOrMore => {
                write!(f, "{}*", s)
            }
            RepetitionType::OneOrMore => {
                write!(f, "{}+", s)
            }
            RepetitionType::ZeroOrOne => {
                write!(f, "{}?", s)
            }
            RepetitionType::One => {
                write!(f, "{}", s)
            }
        }
    }
}

impl Display for Production {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        for item in &self.items {
            match item {
                ProductionItem::Comment(comment) => {
                    s.push_str(&format!("# {}\n", comment));
                }
                ProductionItem::Terminal(terminal) => {
                    s.push_str(&format!("\"{}\"", terminal.value));
                }
                ProductionItem::NonTerminal(non_terminal) => {
                    s.push_str(&format!("{}", non_terminal.name));
                }
                ProductionItem::Group(group) => {
                    s.push_str(&format!("({})", group.to_string()));
                }
                ProductionItem::OneOf(one_of) => {
                    let mut first = true;
                    for production in one_of {
                        if first {
                            first = false;
                        } else {
                            s.push_str(" | ");
                        }
                        s.push_str(&format!("{}", production.to_string()));
                    }
                }
                ProductionItem::Repetition(repetition) => {
                    s.push_str(&format!("{}", repetition.to_string()));
                }
            }
        }
        write!(f, "{}", s)
    }
}

impl Display for Grammar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        for item in &self.items {
            match item {
                GrammarItem::Comment(comment) => {
                    s.push_str(&format!("# {}\n", comment));
                }
                GrammarItem::Rule(rule) => {
                    s.push_str(&format!("{} ::= {}\n", rule.lhs, rule.rhs));
                }
            }
        }
        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_outputs() {
        // root ::= "yes" | "no"
        let g = Grammar {
            items: vec![GrammarItem::Rule(Rule {
                lhs: NonTerminalSymbol {
                    name: "root".to_string(),
                },
                rhs: Production {
                    items: vec![ProductionItem::OneOf(vec![
                        Production {
                            items: vec![ProductionItem::Terminal(TerminalSymbol {
                                value: "yes".to_string(),
                            })],
                        },
                        Production {
                            items: vec![ProductionItem::Terminal(TerminalSymbol {
                                value: "no".to_string(),
                            })],
                        },
                    ])],
                },
            })],
        };
        let s = g.to_string();
        assert_eq!(s, "root ::= \"yes\" | \"no\"\n");
    }
}
