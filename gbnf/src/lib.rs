use json::JsonSchemaParseError;
use json::parse_json_schema_to_grammar;
use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

pub mod json;

// Represents a non-terminal symbol in the grammar.
// examples: root, expr, term, ident, ws, num
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
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
    // [a-z]{m}
    Exact(usize),
    // [a-z]{m,}
    AtLeast(usize),
    // [a-z]{m,n},
    Between((usize, usize)),
    // [a-z]{0,n}
    AtMost(usize),
}

#[derive(Clone, Debug)]
pub enum CharacterSetItem {
    Character(char),
    Tab,
    NewLine,
    CharacterRange(char, char),
    Hex(String),
    Unicode(String),
    Return,
    Backslash,
}

#[derive(Clone, Debug)]
pub struct CharacterSet {
    pub is_complement: bool,
    pub items: Vec<CharacterSetItem>,
}

#[derive(Clone, Debug)]
pub struct ComplementCharacterSet {
    pub items: Vec<CharacterSetItem>,
}

// Represents different types of items that can be part of a production.
#[derive(Clone, Debug)]
pub enum ProductionItem {
    LineBreak,
    // example: # This is a comment
    Comment(String),
    Terminal(TerminalSymbol, RepetitionType),
    NonTerminal(NonTerminalSymbol, RepetitionType),
    // example: ( expr "=" ws term "\n" )
    Group(Box<Production>, RepetitionType),
    // example: ident | num | "(" ws expr ")" ws
    OneOf(Vec<Production>),
    // examples: [a-z], [a-z0-9_]*, [0-9]+
    CharacterSet(CharacterSet, RepetitionType),
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
    LineBreak,
    // example: # This is a comment
    Comment(String),
    Rule(Rule),
}

impl Display for GrammarItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GrammarItem::LineBreak => {
                write!(f, "\n")
            }
            GrammarItem::Comment(comment) => {
                write!(f, "#{}\n", comment)
            }
            GrammarItem::Rule(rule) => {
                write!(f, "{} ::= {}\n", rule.lhs, rule.rhs)
            }
        }
    }
}

// Represents the entire grammar.
#[derive(Clone, Debug)]
pub struct Grammar {
    pub items: Vec<GrammarItem>,
    // this container will later be linearized as rules, its purpose is
    // to hold grammar rules that occur multiple times to avoid duplicating
    // rules. This is very relevant for `primitive` type definitions
    pub recurring_items: BTreeMap<NonTerminalSymbol, Production>,
}

impl Default for Grammar {
    fn default() -> Self {
        Self {
            items: Default::default(),
            recurring_items: Default::default(),
        }
    }
}

impl Display for NonTerminalSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Display for CharacterSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        s.push('[');
        if self.is_complement {
            s.push('^');
        }
        for item in &self.items {
            match item {
                CharacterSetItem::Character(c) => {
                    s.push(*c);
                }
                CharacterSetItem::CharacterRange(start, end) => {
                    s.push(*start);
                    s.push('-');
                    s.push(*end);
                }
                CharacterSetItem::Tab => {
                    s.push_str("\\t");
                }
                CharacterSetItem::NewLine => {
                    s.push_str("\\n");
                }
                CharacterSetItem::Hex(hex) => {
                    s.push_str(&format!("\\x{}", hex));
                }
                CharacterSetItem::Unicode(unicode) => {
                    s.push_str(&format!("\\u{}", unicode));
                }
                CharacterSetItem::Return => {
                    s.push_str("\\r");
                }
                CharacterSetItem::Backslash => {
                    s.push_str("\\\\");
                }
            }
        }
        s.push(']');
        write!(f, "{}", s)
    }
}

impl Display for RepetitionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            RepetitionType::ZeroOrMore => write!(f, "*"),
            RepetitionType::OneOrMore => write!(f, "+"),
            RepetitionType::ZeroOrOne => write!(f, "?"),
            RepetitionType::One => write!(f, ""),
            RepetitionType::Exact(num) => write!(f, "{{{num}}}"),
            RepetitionType::AtLeast(num) => write!(f, "{{{num},}}"),
            RepetitionType::Between((a, b)) => write!(f, "{{{a},{b}}}"),
            RepetitionType::AtMost(num) => write!(f, "{{0,{num}}}"),
        }
    }
}

impl Display for Production {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut is_first_production = true;
        let mut s = String::new();
        for item in &self.items {
            if is_first_production {
                is_first_production = false;
            } else {
                s.push(' ');
            }
            match item {
                ProductionItem::LineBreak => {
                    s.push('\n');
                }
                ProductionItem::Comment(comment) => {
                    s.push_str(&format!("#{}\n", comment));
                }
                ProductionItem::Terminal(terminal, rep) => {
                    s.push_str(&format!("\"{}\"{}", terminal.value, rep));
                }
                ProductionItem::NonTerminal(non_terminal, rep) => {
                    s.push_str(format!("{}{}", &non_terminal.name.to_string(), rep).as_str());
                }
                ProductionItem::Group(group, rep) => {
                    s.push_str(&format!("({}){}", group, rep));
                }
                ProductionItem::OneOf(one_of) => {
                    let mut first = true;
                    for production in one_of {
                        if first {
                            first = false;
                        } else {
                            s.push_str(" | ");
                        }
                        s.push_str(&format!("{}", production));
                    }
                }
                ProductionItem::CharacterSet(character_set, rep) => {
                    s.push_str(&format!("{}{}", character_set, rep));
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
            s.push_str(&item.to_string())
        }
        for item in self.recurring_items.clone().into_iter().map(|(nts, prod)| {
            GrammarItem::Rule(Rule {
                lhs: nts,
                rhs: prod,
            })
        }) {
            s.push_str(&item.to_string());
        }
        write!(f, "{}", s)
    }
}

impl Grammar {
    pub fn from_json_schema(schema: &str) -> Result<Grammar, JsonSchemaParseError> {
        let json = serde_json::from_str::<serde_json::Value>(schema)?;
        Grammar::from_json_schema_value(&json)
    }
    pub fn from_json_schema_value(schema: &serde_json::Value) -> Result<Grammar, JsonSchemaParseError> {
        let mut g = Grammar::default();

        // add $id, $schema, title as commments at top of file
        g.items.push(GrammarItem::Comment(
            "###############################################".to_string(),
        ));
        g.items.push(GrammarItem::Comment(
            " DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR".to_string(),
        ));
        if let Some(id) = schema.get("$id") {
            g.items.push(GrammarItem::Comment(format!(
                " $id: {}",
                id.as_str().unwrap_or("")
            )));
        }
        if let Some(schema) = schema.get("$schema") {
            g.items.push(GrammarItem::Comment(format!(
                " $schema: {}",
                schema.as_str().unwrap_or("")
            )));
        }
        if let Some(title) = schema.get("title") {
            g.items.push(GrammarItem::Comment(format!(
                " title: {}",
                title.as_str().unwrap_or("")
            )));
        }
        g.items.push(GrammarItem::Comment(
            "###############################################".to_string(),
        ));
        g.items.push(GrammarItem::LineBreak);

        parse_json_schema_to_grammar(schema, &mut g, "root".to_string(), 0)?;

        // add comment for primitives
        g.items.push(GrammarItem::LineBreak);
        g.items.push(GrammarItem::Comment(
            "##############################".to_string(),
        ));
        g.items.push(GrammarItem::Comment(
            " Primitive value type symbols".to_string(),
        ));
        g.items.push(GrammarItem::Comment(
            "##############################".to_string(),
        ));
        g.recurring_items.insert(
            NonTerminalSymbol {
                name: "null".to_string(),
            },
            Production {
                items: vec![
                    ProductionItem::Terminal(
                        TerminalSymbol {
                            value: "null".to_string(),
                        },
                        RepetitionType::One,
                    ),
                    ProductionItem::NonTerminal(
                        NonTerminalSymbol {
                            name: "ws".to_string(),
                        },
                        RepetitionType::One,
                    ),
                ],
            },
        );
        g.recurring_items.insert(
            NonTerminalSymbol {
                name: "boolean".to_string(),
            },
            Production {
                items: vec![
                    ProductionItem::OneOf(vec![
                        Production {
                            items: vec![ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "true".to_string(),
                                },
                                RepetitionType::One,
                            )],
                        },
                        Production {
                            items: vec![ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "false".to_string(),
                                },
                                RepetitionType::One,
                            )],
                        },
                    ]),
                    ProductionItem::NonTerminal(
                        NonTerminalSymbol {
                            name: "ws".to_string(),
                        },
                        RepetitionType::One,
                    ),
                ],
            },
        );
        g.recurring_items.insert(
            NonTerminalSymbol {
                name: "string".to_string(),
            },
            Production {
                items: vec![
                    ProductionItem::Terminal(
                        TerminalSymbol {
                            value: r#"\""#.to_string(),
                        },
                        RepetitionType::One,
                    ),
                    ProductionItem::Group(
                        Box::new(Production {
                            items: vec![
                                ProductionItem::OneOf(vec![
                                    Production {
                                        items: vec![ProductionItem::CharacterSet(
                                            CharacterSet {
                                                is_complement: true,
                                                items: vec![
                                                    CharacterSetItem::Character('"'),
                                                    CharacterSetItem::Backslash,
                                                ],
                                            },
                                            RepetitionType::One,
                                        )],
                                    },
                                    Production {
                                        items: vec![ProductionItem::Terminal(
                                            TerminalSymbol {
                                                value: r#"\\"#.to_string(),
                                            },
                                            RepetitionType::One,
                                        )],
                                    },
                                ]),
                                ProductionItem::Group(
                                    Box::new(Production {
                                        items: vec![
                                            ProductionItem::OneOf(vec![
                                                Production {
                                                    items: vec![ProductionItem::CharacterSet(
                                                        CharacterSet {
                                                            is_complement: false,
                                                            items: vec![
                                                                CharacterSetItem::Character('"'),
                                                                CharacterSetItem::Backslash,
                                                                CharacterSetItem::Character('/'),
                                                                CharacterSetItem::Character('b'),
                                                                CharacterSetItem::Character('f'),
                                                                CharacterSetItem::Character('n'),
                                                                CharacterSetItem::Character('r'),
                                                                CharacterSetItem::Character('t'),
                                                            ],
                                                        },
                                                        RepetitionType::One,
                                                    )],
                                                },
                                                Production {
                                                    items: vec![ProductionItem::Terminal(
                                                        TerminalSymbol {
                                                            value: "u".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    )],
                                                },
                                            ]),
                                            ProductionItem::CharacterSet(
                                                CharacterSet {
                                                    is_complement: false,
                                                    items: vec![
                                                        CharacterSetItem::CharacterRange('0', '9'),
                                                        CharacterSetItem::CharacterRange('a', 'f'),
                                                        CharacterSetItem::CharacterRange('A', 'F'),
                                                    ],
                                                },
                                                RepetitionType::One,
                                            ),
                                            ProductionItem::CharacterSet(
                                                CharacterSet {
                                                    is_complement: false,
                                                    items: vec![
                                                        CharacterSetItem::CharacterRange('0', '9'),
                                                        CharacterSetItem::CharacterRange('a', 'f'),
                                                        CharacterSetItem::CharacterRange('A', 'F'),
                                                    ],
                                                },
                                                RepetitionType::One,
                                            ),
                                            ProductionItem::CharacterSet(
                                                CharacterSet {
                                                    is_complement: false,
                                                    items: vec![
                                                        CharacterSetItem::CharacterRange('0', '9'),
                                                        CharacterSetItem::CharacterRange('a', 'f'),
                                                        CharacterSetItem::CharacterRange('A', 'F'),
                                                    ],
                                                },
                                                RepetitionType::One,
                                            ),
                                            ProductionItem::CharacterSet(
                                                CharacterSet {
                                                    is_complement: false,
                                                    items: vec![
                                                        CharacterSetItem::CharacterRange('0', '9'),
                                                        CharacterSetItem::CharacterRange('a', 'f'),
                                                        CharacterSetItem::CharacterRange('A', 'F'),
                                                    ],
                                                },
                                                RepetitionType::One,
                                            ),
                                        ],
                                    }),
                                    RepetitionType::One,
                                ),
                            ],
                        }),
                        RepetitionType::ZeroOrMore,
                    ),
                    ProductionItem::Terminal(
                        TerminalSymbol {
                            value: r#"\""#.to_string(),
                        },
                        RepetitionType::One,
                    ),
                    ProductionItem::NonTerminal(
                        NonTerminalSymbol {
                            name: "ws".to_string(),
                        },
                        RepetitionType::One,
                    ),
                ],
            },
        );
        g.recurring_items.insert(
            NonTerminalSymbol {
                name: "number".to_string(),
            },
            Production {
                items: vec![
                    ProductionItem::Group(
                        Box::new(Production {
                            items: vec![
                                ProductionItem::Terminal(
                                    TerminalSymbol {
                                        value: "-".to_string(),
                                    },
                                    RepetitionType::ZeroOrOne,
                                ),
                                ProductionItem::Group(
                                    Box::new(Production {
                                        items: vec![ProductionItem::OneOf(vec![
                                            Production {
                                                items: vec![ProductionItem::CharacterSet(
                                                    CharacterSet {
                                                        is_complement: false,
                                                        items: vec![
                                                            CharacterSetItem::CharacterRange(
                                                                '0', '9',
                                                            ),
                                                        ],
                                                    },
                                                    RepetitionType::One,
                                                )],
                                            },
                                            Production {
                                                items: vec![
                                                    ProductionItem::CharacterSet(
                                                        CharacterSet {
                                                            is_complement: false,
                                                            items: vec![
                                                                CharacterSetItem::CharacterRange(
                                                                    '1', '9',
                                                                ),
                                                            ],
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                    ProductionItem::CharacterSet(
                                                        CharacterSet {
                                                            is_complement: false,
                                                            items: vec![
                                                                CharacterSetItem::CharacterRange(
                                                                    '0', '9',
                                                                ),
                                                            ],
                                                        },
                                                        RepetitionType::ZeroOrMore,
                                                    ),
                                                ],
                                            },
                                        ])],
                                    }),
                                    RepetitionType::One,
                                ),
                            ],
                        }),
                        RepetitionType::One,
                    ),
                    ProductionItem::Group(
                        Box::new(Production {
                            items: vec![
                                ProductionItem::Terminal(
                                    TerminalSymbol {
                                        value: ".".to_string(),
                                    },
                                    RepetitionType::One,
                                ),
                                ProductionItem::CharacterSet(
                                    CharacterSet {
                                        is_complement: false,
                                        items: vec![CharacterSetItem::CharacterRange('0', '9')],
                                    },
                                    RepetitionType::OneOrMore,
                                ),
                            ],
                        }),
                        RepetitionType::ZeroOrOne,
                    ),
                    ProductionItem::Group(
                        Box::new(Production {
                            items: vec![
                                ProductionItem::CharacterSet(
                                    CharacterSet {
                                        is_complement: false,
                                        items: vec![
                                            CharacterSetItem::Character('e'),
                                            CharacterSetItem::Character('E'),
                                        ],
                                    },
                                    RepetitionType::One,
                                ),
                                ProductionItem::CharacterSet(
                                    CharacterSet {
                                        is_complement: false,
                                        items: vec![
                                            CharacterSetItem::Character('-'),
                                            CharacterSetItem::Character('+'),
                                        ],
                                    },
                                    RepetitionType::ZeroOrOne,
                                ),
                                ProductionItem::CharacterSet(
                                    CharacterSet {
                                        is_complement: false,
                                        items: vec![CharacterSetItem::CharacterRange('0', '9')],
                                    },
                                    RepetitionType::OneOrMore,
                                ),
                            ],
                        }),
                        RepetitionType::ZeroOrOne,
                    ),
                    ProductionItem::NonTerminal(
                        NonTerminalSymbol {
                            name: "ws".to_string(),
                        },
                        RepetitionType::One,
                    ),
                ],
            },
        );
        g.recurring_items.insert(
            NonTerminalSymbol {
                name: "integer".to_string(),
            },
            Production {
                items: vec![
                    ProductionItem::Group(
                        Box::new(Production {
                            items: vec![
                                ProductionItem::Terminal(
                                    TerminalSymbol {
                                        value: "-".to_string(),
                                    },
                                    RepetitionType::ZeroOrOne,
                                ),
                                ProductionItem::Group(
                                    Box::new(Production {
                                        items: vec![ProductionItem::OneOf(vec![
                                            Production {
                                                items: vec![ProductionItem::CharacterSet(
                                                    CharacterSet {
                                                        is_complement: false,
                                                        items: vec![
                                                            CharacterSetItem::CharacterRange(
                                                                '0', '9',
                                                            ),
                                                        ],
                                                    },
                                                    RepetitionType::One,
                                                )],
                                            },
                                            Production {
                                                items: vec![
                                                    ProductionItem::CharacterSet(
                                                        CharacterSet {
                                                            is_complement: false,
                                                            items: vec![
                                                                CharacterSetItem::CharacterRange(
                                                                    '1', '9',
                                                                ),
                                                            ],
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                    ProductionItem::CharacterSet(
                                                        CharacterSet {
                                                            is_complement: false,
                                                            items: vec![
                                                                CharacterSetItem::CharacterRange(
                                                                    '0', '9',
                                                                ),
                                                            ],
                                                        },
                                                        RepetitionType::ZeroOrMore,
                                                    ),
                                                ],
                                            },
                                        ])],
                                    }),
                                    RepetitionType::One,
                                ),
                            ],
                        }),
                        RepetitionType::One,
                    ),
                    ProductionItem::NonTerminal(
                        NonTerminalSymbol {
                            name: "ws".to_string(),
                        },
                        RepetitionType::One,
                    ),
                ],
            },
        );
        g.recurring_items.insert(
            NonTerminalSymbol {
                name: "ws".to_string(),
            },
            Production {
                items: vec![ProductionItem::CharacterSet(
                    CharacterSet {
                        is_complement: false,
                        items: vec![
                            CharacterSetItem::Character(' '),
                            CharacterSetItem::Tab,
                            CharacterSetItem::NewLine,
                        ],
                    },
                    RepetitionType::ZeroOrMore,
                )],
            },
        );
        Ok(g)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_0() {
        // root ::= "yes" | "no"
        let g = Grammar {
            items: vec![GrammarItem::Rule(Rule {
                lhs: NonTerminalSymbol {
                    name: "root".to_string(),
                },
                rhs: Production {
                    items: vec![ProductionItem::OneOf(vec![
                        Production {
                            items: vec![ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "yes".to_string(),
                                },
                                RepetitionType::One,
                            )],
                        },
                        Production {
                            items: vec![ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "no".to_string(),
                                },
                                RepetitionType::One,
                            )],
                        },
                    ])],
                },
            })],
            ..Default::default()
        };
        let s = g.to_string();
        pretty_assertions::assert_eq!(s, "root ::= \"yes\" | \"no\"\n");
    }

    #[test]
    fn simple_1() {
        //# This is a comment
        // root ::= answers
        // answers := "yes" | "no"

        let g = Grammar {
            items: vec![
                GrammarItem::Comment(" This is a comment".to_string()),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "root".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::NonTerminal(
                            NonTerminalSymbol {
                                name: "answers".to_string(),
                            },
                            RepetitionType::One,
                        )],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "answers".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::OneOf(vec![
                            Production {
                                items: vec![ProductionItem::Terminal(
                                    TerminalSymbol {
                                        value: "yes".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                            Production {
                                items: vec![ProductionItem::Terminal(
                                    TerminalSymbol {
                                        value: "no".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                        ])],
                    },
                }),
            ],
            ..Default::default()
        };
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            "# This is a comment\nroot ::= answers\nanswers ::= \"yes\" | \"no\"\n"
        );
    }

    #[test]
    fn simple_2() {
        // # A probably incorrect grammar for japanese word
        // root        ::= jp-char+
        // jp-char     ::= hiragana | katakana
        // hiragana    ::= [ぁ-ゟ]
        // katakana    ::= [ァ-ヿ]

        let g = Grammar {
            items: vec![
                GrammarItem::Comment(" A probably incorrect grammar for japanese word".to_string()),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "root".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::NonTerminal(
                            NonTerminalSymbol {
                                name: "jp-char".to_string(),
                            },
                            RepetitionType::OneOrMore,
                        )],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "jp-char".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::OneOf(vec![
                            Production {
                                items: vec![ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: "hiragana".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                            Production {
                                items: vec![ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: "katakana".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                        ])],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "hiragana".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![CharacterSetItem::CharacterRange('ぁ', 'ゟ')],
                            },
                            RepetitionType::One,
                        )],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "katakana".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![CharacterSetItem::CharacterRange('ァ', 'ヿ')],
                            },
                            RepetitionType::One,
                        )],
                    },
                }),
            ],
            ..Default::default()
        };
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            "# A probably incorrect grammar for japanese word\nroot ::= jp-char+\njp-char ::= hiragana | katakana\nhiragana ::= [ぁ-ゟ]\nkatakana ::= [ァ-ヿ]\n"
        );
    }

    #[test]
    fn japanese() {
        // # A probably incorrect grammar for Japanese
        // root        ::= jp-char+ ([ \t\n] jp-char+)*
        // jp-char     ::= hiragana | katakana | punctuation | cjk
        // hiragana    ::= [ぁ-ゟ]
        // katakana    ::= [ァ-ヿ]
        // punctuation ::= [、-〾]
        // cjk         ::= [一-鿿]

        let g = Grammar {
            items: vec![
                GrammarItem::Comment(" A probably incorrect grammar for Japanese".to_string()),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "root".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "jp-char".to_string(),
                                },
                                RepetitionType::OneOrMore,
                            ),
                            ProductionItem::Group(
                                Box::new(Production {
                                    items: vec![
                                        ProductionItem::CharacterSet(
                                            CharacterSet {
                                                is_complement: false,
                                                items: vec![
                                                    CharacterSetItem::Character(' '),
                                                    CharacterSetItem::Character('\t'),
                                                    CharacterSetItem::Character('\n'),
                                                ],
                                            },
                                            RepetitionType::One,
                                        ),
                                        ProductionItem::NonTerminal(
                                            NonTerminalSymbol {
                                                name: "jp-char".to_string(),
                                            },
                                            RepetitionType::OneOrMore,
                                        ),
                                    ],
                                }),
                                RepetitionType::ZeroOrMore,
                            ),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "jp-char".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::OneOf(vec![
                            Production {
                                items: vec![ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: "hiragana".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                            Production {
                                items: vec![ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: "katakana".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                            Production {
                                items: vec![ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: "punctuation".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                            Production {
                                items: vec![ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: "cjk".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                        ])],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "hiragana".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![CharacterSetItem::CharacterRange('ぁ', 'ゟ')],
                            },
                            RepetitionType::One,
                        )],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "katakana".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![CharacterSetItem::CharacterRange('ァ', 'ヿ')],
                            },
                            RepetitionType::One,
                        )],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "punctuation".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![CharacterSetItem::CharacterRange('、', '〾')],
                            },
                            RepetitionType::One,
                        )],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "cjk".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![CharacterSetItem::CharacterRange('一', '鿿')],
                            },
                            RepetitionType::One,
                        )],
                    },
                }),
            ],
            ..Default::default()
        };
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            "# A probably incorrect grammar for Japanese\nroot ::= jp-char+ ([ \t\n] jp-char+)*\njp-char ::= hiragana | katakana | punctuation | cjk\nhiragana ::= [ぁ-ゟ]\nkatakana ::= [ァ-ヿ]\npunctuation ::= [、-〾]\ncjk ::= [一-鿿]\n"
        );
    }

    #[test]
    fn arithmatic() {
        // root  ::= (expr "=" ws term "\n")+
        // expr  ::= term ([-+*/] term)*
        // term  ::= ident | num | "(" ws expr ")" ws
        // ident ::= [a-z] [a-z0-9_]* ws
        // num   ::= [0-9]+ ws
        // ws    ::= [ \t\n]*

        let g = Grammar {
            items: vec![
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "root".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::Group(
                            Box::new(Production {
                                items: vec![
                                    ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "expr".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                    ProductionItem::Terminal(
                                        TerminalSymbol {
                                            value: "=".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                    ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "ws".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                    ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "term".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                    ProductionItem::Terminal(
                                        TerminalSymbol {
                                            value: "\\n".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                ],
                            }),
                            RepetitionType::OneOrMore,
                        )],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "expr".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "term".to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::Group(
                                Box::new(Production {
                                    items: vec![
                                        ProductionItem::CharacterSet(
                                            CharacterSet {
                                                is_complement: false,
                                                items: vec![
                                                    CharacterSetItem::Character('-'),
                                                    CharacterSetItem::Character('+'),
                                                    CharacterSetItem::Character('*'),
                                                    CharacterSetItem::Character('/'),
                                                ],
                                            },
                                            RepetitionType::One,
                                        ),
                                        ProductionItem::NonTerminal(
                                            NonTerminalSymbol {
                                                name: "term".to_string(),
                                            },
                                            RepetitionType::One,
                                        ),
                                    ],
                                }),
                                RepetitionType::ZeroOrMore,
                            ),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "term".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::OneOf(vec![
                            Production {
                                items: vec![ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: "ident".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                            Production {
                                items: vec![ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: "num".to_string(),
                                    },
                                    RepetitionType::One,
                                )],
                            },
                            Production {
                                items: vec![
                                    ProductionItem::Terminal(
                                        TerminalSymbol {
                                            value: "(".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                    ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "ws".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                    ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "expr".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                    ProductionItem::Terminal(
                                        TerminalSymbol {
                                            value: ")".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                    ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "ws".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                ],
                            },
                        ])],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "ident".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::CharacterSet(
                                CharacterSet {
                                    is_complement: false,
                                    items: vec![CharacterSetItem::CharacterRange('a', 'z')],
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::CharacterSet(
                                CharacterSet {
                                    is_complement: false,
                                    items: vec![
                                        CharacterSetItem::CharacterRange('a', 'z'),
                                        CharacterSetItem::CharacterRange('0', '9'),
                                        CharacterSetItem::Character('_'),
                                    ],
                                },
                                RepetitionType::ZeroOrMore,
                            ),
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "ws".to_string(),
                                },
                                RepetitionType::One,
                            ),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "num".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::CharacterSet(
                                CharacterSet {
                                    is_complement: false,
                                    items: vec![CharacterSetItem::CharacterRange('0', '9')],
                                },
                                RepetitionType::OneOrMore,
                            ),
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "ws".to_string(),
                                },
                                RepetitionType::One,
                            ),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "ws".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![
                                    CharacterSetItem::Character(' '),
                                    CharacterSetItem::Tab,
                                    CharacterSetItem::NewLine,
                                ],
                            },
                            RepetitionType::ZeroOrMore,
                        )],
                    },
                }),
            ],
            ..Default::default()
        };
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            "root ::= (expr \"=\" ws term \"\\n\")+\nexpr ::= term ([-+*/] term)*\nterm ::= ident | num | \"(\" ws expr \")\" ws\nident ::= [a-z] [a-z0-9_]* ws\nnum ::= [0-9]+ ws\nws ::= [ \\t\\n]*\n"
        );
    }

    #[test]
    fn chess() {
        // # Specifies chess moves as a list in algebraic notation, using PGN conventions
        // # Force first move to "1. ", then any 1-2 digit number after, relying on model to follow the pattern
        // root    ::= "1. " move " " move "\n" ([1-9] [0-9]? ". " move " " move "\n")+
        // move    ::= (pawn | nonpawn | castle) [+#]?
        // # piece type, optional file/rank, optional capture, dest file & rank
        // nonpawn ::= [NBKQR] [a-h]? [1-8]? "x"? [a-h] [1-8]
        // # optional file & capture, dest file & rank, optional promotion
        // pawn    ::= ([a-h] "x")? [a-h] [1-8] ("=" [NBKQR])?
        // castle  ::= "O-O" "-O"?

        let g = Grammar{
            items: vec![
                GrammarItem::Comment(" Specifies chess moves as a list in algebraic notation, using PGN conventions".to_string()),
                GrammarItem::Comment(" Force first move to \"1. \", then any 1-2 digit number after, relying on model to follow the pattern".to_string()),
                GrammarItem::Rule(Rule{
                    lhs: NonTerminalSymbol{
                        name: "root".to_string(),
                    },
                    rhs: Production{
                        items: vec![
                            ProductionItem::Terminal(TerminalSymbol{
                                value: "1. ".to_string(),
                            }, RepetitionType::One),
                            ProductionItem::NonTerminal(NonTerminalSymbol{
                                name: "move".to_string(),
                            }, RepetitionType::One),
                            ProductionItem::Terminal(TerminalSymbol{
                                value: " ".to_string(),
                            }, RepetitionType::One),
                            ProductionItem::NonTerminal(NonTerminalSymbol{
                                name: "move".to_string(),
                            }, RepetitionType::One),
                            ProductionItem::Terminal(TerminalSymbol{
                                value: "\\n".to_string(),
                            }, RepetitionType::One),
                            ProductionItem::Group(Box::new(Production{
                                items: vec![
                                    ProductionItem::CharacterSet(CharacterSet{
                                        is_complement: false,
                                        items: vec![
                                            CharacterSetItem::CharacterRange('1', '9'),
                                        ],
                                    }, RepetitionType::One),
                                    ProductionItem::CharacterSet(CharacterSet{
                                        is_complement: false,
                                        items: vec![
                                            CharacterSetItem::CharacterRange('0', '9'),
                                        ],
                                    }, RepetitionType::ZeroOrOne),
                                    ProductionItem::Terminal(TerminalSymbol{
                                        value: ". ".to_string(),
                                    }, RepetitionType::One),
                                    ProductionItem::NonTerminal(NonTerminalSymbol{
                                        name: "move".to_string(),
                                    }, RepetitionType::One),
                                    ProductionItem::Terminal(TerminalSymbol{
                                        value: " ".to_string(),
                                    }, RepetitionType::One),
                                    ProductionItem::NonTerminal(NonTerminalSymbol{
                                        name: "move".to_string(),
                                    }, RepetitionType::One),
                                    ProductionItem::Terminal(TerminalSymbol{
                                        value: "\\n".to_string(),
                                    }, RepetitionType::One),
                                ],
                            }), RepetitionType::OneOrMore),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule{
                    lhs: NonTerminalSymbol{
                        name: "move".to_string(),
                    },
                    rhs: Production{
                        items: vec![
                            ProductionItem::Group(Box::new(Production{
                                items: vec![
                                    ProductionItem::OneOf(vec![
                                        Production{
                                            items: vec![
                                                ProductionItem::NonTerminal(NonTerminalSymbol{
                                                    name: "pawn".to_string(),
                                                }, RepetitionType::One),
                                            ],
                                        },
                                        Production{
                                            items: vec![
                                                ProductionItem::NonTerminal(NonTerminalSymbol{
                                                    name: "nonpawn".to_string(),
                                                }, RepetitionType::One),
                                            ],
                                        },
                                        Production{
                                            items: vec![
                                                ProductionItem::NonTerminal(NonTerminalSymbol{
                                                    name: "castle".to_string(),
                                                }, RepetitionType::One),
                                            ],
                                        },
                                    ]),
                                ],
                            }), RepetitionType::One),
                            ProductionItem::CharacterSet(CharacterSet{
                                is_complement: false,
                                items: vec![
                                    CharacterSetItem::Character('+'),
                                    CharacterSetItem::Character('#'),
                                ],
                            }, RepetitionType::ZeroOrOne),
                        ],
                    },
                }),
                GrammarItem::Comment(" piece type, optional file/rank, optional capture, dest file & rank".to_string()),
                GrammarItem::Rule(Rule{
                    lhs: NonTerminalSymbol{
                        name: "nonpawn".to_string(),
                    },
                    rhs: Production{
                        items: vec![
                            ProductionItem::CharacterSet(CharacterSet{
                                is_complement: false,
                                items: vec![
                                    CharacterSetItem::Character('N'),
                                    CharacterSetItem::Character('B'),
                                    CharacterSetItem::Character('K'),
                                    CharacterSetItem::Character('Q'),
                                    CharacterSetItem::Character('R'),
                                ],
                            }, RepetitionType::One),
                            ProductionItem::CharacterSet(CharacterSet{
                                is_complement: false,
                                items: vec![
                                    CharacterSetItem::CharacterRange('a', 'h'),
                                ],
                            }, RepetitionType::ZeroOrOne),
                            ProductionItem::CharacterSet(CharacterSet{
                                is_complement: false,
                                items: vec![
                                    CharacterSetItem::CharacterRange('1', '8'),
                                ],
                            }, RepetitionType::ZeroOrOne),
                            ProductionItem::Terminal(TerminalSymbol{
                                value: "x".to_string(),
                            }, RepetitionType::ZeroOrOne),
                            ProductionItem::CharacterSet(CharacterSet{
                                is_complement: false,
                                items: vec![
                                    CharacterSetItem::CharacterRange('a', 'h'),
                                ],
                            }, RepetitionType::One),
                            ProductionItem::CharacterSet(CharacterSet{
                                is_complement: false,
                                items: vec![
                                    CharacterSetItem::CharacterRange('1', '8'),
                                ],
                            }, RepetitionType::One),
                        ],
                    },
                }),
                GrammarItem::Comment(" optional file & capture, dest file & rank, optional promotion".to_string()),
                GrammarItem::Rule(Rule{
                    lhs: NonTerminalSymbol{
                        name: "pawn".to_string(),
                    },
                    rhs: Production{
                        items: vec![
                            ProductionItem::Group(Box::new(Production{
                                items: vec![
                                    ProductionItem::CharacterSet(CharacterSet{
                                        is_complement: false,
                                        items: vec![
                                            CharacterSetItem::CharacterRange('a', 'h'),
                                        ],
                                    }, RepetitionType::One),
                                    ProductionItem::Terminal(TerminalSymbol{
                                        value: "x".to_string(),
                                    }, RepetitionType::One),
                                ],
                            }), RepetitionType::ZeroOrOne),
                            ProductionItem::CharacterSet(CharacterSet{
                                is_complement: false,
                                items: vec![
                                    CharacterSetItem::CharacterRange('a', 'h'),
                                ],
                            }, RepetitionType::One),
                            ProductionItem::CharacterSet(CharacterSet{
                                is_complement: false,
                                items: vec![
                                    CharacterSetItem::CharacterRange('1', '8'),
                                ],
                            }, RepetitionType::One),
                            ProductionItem::Group(Box::new(Production{
                                items: vec![
                                    ProductionItem::Terminal(TerminalSymbol{
                                        value: "=".to_string(),
                                    }, RepetitionType::One),
                                    ProductionItem::CharacterSet(CharacterSet{
                                        is_complement: false,
                                        items: vec![
                                            CharacterSetItem::Character('N'),
                                            CharacterSetItem::Character('B'),
                                            CharacterSetItem::Character('K'),
                                            CharacterSetItem::Character('Q'),
                                            CharacterSetItem::Character('R'),
                                        ],
                                    }, RepetitionType::One),
                                ],
                            }), RepetitionType::ZeroOrOne),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule{
                    lhs: NonTerminalSymbol{
                        name: "castle".to_string(),
                    },
                    rhs: Production{
                        items: vec![
                            ProductionItem::Terminal(TerminalSymbol{
                                value: "O-O".to_string(),
                            }, RepetitionType::One),
                            ProductionItem::Terminal(TerminalSymbol{
                                value: "-O".to_string(),
                            }, RepetitionType::ZeroOrOne),
                        ],
                    },
                }),
            ],
            ..Default::default()
        };
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            "# Specifies chess moves as a list in algebraic notation, using PGN conventions\n# Force first move to \"1. \", then any 1-2 digit number after, relying on model to follow the pattern\nroot ::= \"1. \" move \" \" move \"\\n\" ([1-9] [0-9]? \". \" move \" \" move \"\\n\")+\nmove ::= (pawn | nonpawn | castle) [+#]?\n# piece type, optional file/rank, optional capture, dest file & rank\nnonpawn ::= [NBKQR] [a-h]? [1-8]? \"x\"? [a-h] [1-8]\n# optional file & capture, dest file & rank, optional promotion\npawn ::= ([a-h] \"x\")? [a-h] [1-8] (\"=\" [NBKQR])?\ncastle ::= \"O-O\" \"-O\"?\n"
        );
    }

    #[test]
    fn list() {
        // root ::= item+
        // # Excludes various line break characters
        // item ::= "- " [^\r\n\x0b\x0c\x85\u2028\u2029]+ "\n"

        let g = Grammar {
            items: vec![
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "root".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::NonTerminal(
                            NonTerminalSymbol {
                                name: "item".to_string(),
                            },
                            RepetitionType::OneOrMore,
                        )],
                    },
                }),
                GrammarItem::Comment(" Excludes various line break characters".to_string()),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "item".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "- ".to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::CharacterSet(
                                CharacterSet {
                                    is_complement: true,
                                    items: vec![
                                        CharacterSetItem::Return,
                                        CharacterSetItem::NewLine,
                                        CharacterSetItem::Hex("0b".to_string()),
                                        CharacterSetItem::Hex("0c".to_string()),
                                        CharacterSetItem::Hex("85".to_string()),
                                        CharacterSetItem::Unicode("2028".to_string()),
                                        CharacterSetItem::Unicode("2029".to_string()),
                                    ],
                                },
                                RepetitionType::OneOrMore,
                            ),
                            ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "\\n".to_string(),
                                },
                                RepetitionType::One,
                            ),
                        ],
                    },
                }),
            ],
            ..Default::default()
        };

        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            "root ::= item+\n# Excludes various line break characters\nitem ::= \"- \" [^\\r\\n\\x0b\\x0c\\x85\\u2028\\u2029]+ \"\\n\"\n"
        );
    }

    #[test]
    fn json() {
        // root   ::= object
        // value  ::= object | array | string | number | ("true" | "false" | "null") ws
        // object ::=
        //  "{" ws (
        //            string ":" ws value
        //    ("," ws string ":" ws value)*
        //  )? "}" ws
        //array  ::=
        //  "[" ws (
        //            value
        //    ("," ws value)*
        //  )? "]" ws
        //string ::=
        //  "\"" (
        //    [^"\\] |
        //    "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]) # escapes
        //  )* "\"" ws
        //number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
        //# Optional space: by convention, applied in this grammar after literal chars when allowed
        //ws ::= ([ \t\n] ws)?

        let g = Grammar {
            items: vec![
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "root".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::NonTerminal(
                            NonTerminalSymbol {
                                name: "object".to_string(),
                            },
                            RepetitionType::One,
                        )],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "value".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::OneOf(vec![
                                Production {
                                    items: vec![ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "object".to_string(),
                                        },
                                        RepetitionType::One,
                                    )],
                                },
                                Production {
                                    items: vec![ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "array".to_string(),
                                        },
                                        RepetitionType::One,
                                    )],
                                },
                                Production {
                                    items: vec![ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "string".to_string(),
                                        },
                                        RepetitionType::One,
                                    )],
                                },
                                Production {
                                    items: vec![ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "number".to_string(),
                                        },
                                        RepetitionType::One,
                                    )],
                                },
                                Production {
                                    items: vec![ProductionItem::Group(
                                        Box::new(Production {
                                            items: vec![ProductionItem::OneOf(vec![
                                                Production {
                                                    items: vec![ProductionItem::Terminal(
                                                        TerminalSymbol {
                                                            value: "true".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    )],
                                                },
                                                Production {
                                                    items: vec![ProductionItem::Terminal(
                                                        TerminalSymbol {
                                                            value: "false".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    )],
                                                },
                                                Production {
                                                    items: vec![ProductionItem::Terminal(
                                                        TerminalSymbol {
                                                            value: "null".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    )],
                                                },
                                            ])],
                                        }),
                                        RepetitionType::One,
                                    )],
                                },
                            ]),
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "ws".to_string(),
                                },
                                RepetitionType::One,
                            ),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "object".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "{".to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "ws".to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::Group(
                                Box::new(Production {
                                    items: vec![
                                        ProductionItem::NonTerminal(
                                            NonTerminalSymbol {
                                                name: "string".to_string(),
                                            },
                                            RepetitionType::One,
                                        ),
                                        ProductionItem::Terminal(
                                            TerminalSymbol {
                                                value: ":".to_string(),
                                            },
                                            RepetitionType::One,
                                        ),
                                        ProductionItem::NonTerminal(
                                            NonTerminalSymbol {
                                                name: "ws".to_string(),
                                            },
                                            RepetitionType::One,
                                        ),
                                        ProductionItem::NonTerminal(
                                            NonTerminalSymbol {
                                                name: "value".to_string(),
                                            },
                                            RepetitionType::One,
                                        ),
                                        ProductionItem::Group(
                                            Box::new(Production {
                                                items: vec![
                                                    ProductionItem::Terminal(
                                                        TerminalSymbol {
                                                            value: ",".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                    ProductionItem::NonTerminal(
                                                        NonTerminalSymbol {
                                                            name: "ws".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                    ProductionItem::NonTerminal(
                                                        NonTerminalSymbol {
                                                            name: "string".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                    ProductionItem::Terminal(
                                                        TerminalSymbol {
                                                            value: ":".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                    ProductionItem::NonTerminal(
                                                        NonTerminalSymbol {
                                                            name: "ws".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                    ProductionItem::NonTerminal(
                                                        NonTerminalSymbol {
                                                            name: "value".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                ],
                                            }),
                                            RepetitionType::ZeroOrMore,
                                        ),
                                    ],
                                }),
                                RepetitionType::ZeroOrOne,
                            ),
                            ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "}".to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "ws".to_string(),
                                },
                                RepetitionType::One,
                            ),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "array".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "[".to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "ws".to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::Group(
                                Box::new(Production {
                                    items: vec![
                                        ProductionItem::NonTerminal(
                                            NonTerminalSymbol {
                                                name: "value".to_string(),
                                            },
                                            RepetitionType::One,
                                        ),
                                        ProductionItem::Group(
                                            Box::new(Production {
                                                items: vec![
                                                    ProductionItem::Terminal(
                                                        TerminalSymbol {
                                                            value: ",".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                    ProductionItem::NonTerminal(
                                                        NonTerminalSymbol {
                                                            name: "ws".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                    ProductionItem::NonTerminal(
                                                        NonTerminalSymbol {
                                                            name: "value".to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                ],
                                            }),
                                            RepetitionType::ZeroOrMore,
                                        ),
                                    ],
                                }),
                                RepetitionType::ZeroOrOne,
                            ),
                            ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: "]".to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "ws".to_string(),
                                },
                                RepetitionType::One,
                            ),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "string".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: r#"\""#.to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::Group(
                                Box::new(Production {
                                    items: vec![
                                        ProductionItem::OneOf(vec![
                                            Production {
                                                items: vec![
                                                    ProductionItem::CharacterSet(
                                                        CharacterSet {
                                                            is_complement: true,
                                                            items: vec![
                                                                CharacterSetItem::Character('"'),
                                                                CharacterSetItem::Backslash,
                                                            ],
                                                        },
                                                        RepetitionType::One,
                                                    )
                                                ],
                                            },
                                            Production {
                                                items: vec![
                                                    ProductionItem::Terminal(
                                                        TerminalSymbol {
                                                            value: r#"\\"#.to_string(),
                                                        },
                                                        RepetitionType::One,
                                                    ),
                                                ],
                                            },
                                        ]),
                                        ProductionItem::Group(Box::new(Production{
                                            items: vec![
                                                ProductionItem::OneOf(
                                                    vec![
                                                        Production {
                                                            items: vec![
                                                                ProductionItem::CharacterSet(
                                                                    CharacterSet {
                                                                        is_complement: false,
                                                                        items: vec![
                                                                            CharacterSetItem::Character('"'),
                                                                            CharacterSetItem::Backslash,
                                                                            CharacterSetItem::Character('/'),
                                                                            CharacterSetItem::Character('b'),
                                                                            CharacterSetItem::Character('f'),
                                                                            CharacterSetItem::Character('n'),
                                                                            CharacterSetItem::Character('r'),
                                                                            CharacterSetItem::Character('t'),
                                                                        ],
                                                                    },
                                                                    RepetitionType::One,
                                                                )
                                                            ]
                                                        },
                                                        Production {
                                                            items: vec![
                                                                ProductionItem::Terminal(
                                                                    TerminalSymbol {
                                                                        value: "u".to_string(),
                                                                    },
                                                                    RepetitionType::One,
                                                                ),
                                                            ]
                                                        },
                                                    ]
                                                ),
                                                ProductionItem::CharacterSet(
                                                    CharacterSet {
                                                        is_complement: false,
                                                        items: vec![
                                                            CharacterSetItem::CharacterRange('0', '9'),
                                                            CharacterSetItem::CharacterRange('a', 'f'),
                                                            CharacterSetItem::CharacterRange('A', 'F'),
                                                        ],
                                                    },
                                                    RepetitionType::One,
                                                ),
                                                ProductionItem::CharacterSet(
                                                    CharacterSet {
                                                        is_complement: false,
                                                        items: vec![
                                                            CharacterSetItem::CharacterRange('0', '9'),
                                                            CharacterSetItem::CharacterRange('a', 'f'),
                                                            CharacterSetItem::CharacterRange('A', 'F'),
                                                        ],
                                                    },
                                                    RepetitionType::One,
                                                ),
                                                ProductionItem::CharacterSet(
                                                    CharacterSet {
                                                        is_complement: false,
                                                        items: vec![
                                                            CharacterSetItem::CharacterRange('0', '9'),
                                                            CharacterSetItem::CharacterRange('a', 'f'),
                                                            CharacterSetItem::CharacterRange('A', 'F'),
                                                        ],
                                                    },
                                                    RepetitionType::One,
                                                ),
                                                ProductionItem::CharacterSet(
                                                    CharacterSet {
                                                        is_complement: false,
                                                        items: vec![
                                                            CharacterSetItem::CharacterRange('0', '9'),
                                                            CharacterSetItem::CharacterRange('a', 'f'),
                                                            CharacterSetItem::CharacterRange('A', 'F'),
                                                        ],
                                                    },
                                                    RepetitionType::One,
                                                ),
                                            ]
                                        }), RepetitionType::One),
                                    ],
                                }),
                                RepetitionType::ZeroOrMore,
                            ),
                            ProductionItem::Terminal(
                                TerminalSymbol {
                                    value: r#"\""#.to_string(),
                                },
                                RepetitionType::One,
                            ),
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "ws".to_string(),
                                },
                                RepetitionType::One,
                            ),
                        ],
                    },
                }),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "number".to_string(),
                    },
                    rhs: Production {
                        items: vec![
                            ProductionItem::Group(
                                Box::new(Production {
                                    items: vec![
                                        ProductionItem::Terminal(
                                            TerminalSymbol {
                                                value: "-".to_string(),
                                            },
                                            RepetitionType::ZeroOrOne,
                                        ),
                                        ProductionItem::Group(
                                            Box::new(Production {
                                                items: vec![
                                                    ProductionItem::OneOf(vec![
                                                        Production {
                                                            items: vec![ProductionItem::CharacterSet(
                                                                CharacterSet {
                                                                    is_complement: false,
                                                                    items: vec![
                                                                        CharacterSetItem::CharacterRange(
                                                                            '0', '9',
                                                                        ),
                                                                    ],
                                                                },
                                                                RepetitionType::One,
                                                            )],
                                                        },
                                                        Production {
                                                            items: vec![
                                                                ProductionItem::CharacterSet(
                                                                    CharacterSet {
                                                                        is_complement: false,
                                                                        items: vec![
                                                                            CharacterSetItem::CharacterRange(
                                                                                '1', '9',
                                                                            ),
                                                                        ],
                                                                    },
                                                                    RepetitionType::One,
                                                                ),
                                                                ProductionItem::CharacterSet(
                                                                    CharacterSet {
                                                                        is_complement: false,
                                                                        items: vec![
                                                                            CharacterSetItem::CharacterRange(
                                                                                '0', '9',
                                                                            ),
                                                                        ],
                                                                    },
                                                                    RepetitionType::ZeroOrMore,
                                                                ),
                                                            ],
                                                        },
                                                    ]),
                                                ],
                                            }),
                                            RepetitionType::One,
                                        ),
                                    ],
                                }),
                                RepetitionType::One,
                            ),
                            ProductionItem::Group(
                                Box::new(Production {
                                    items: vec![
                                        ProductionItem::Terminal(
                                            TerminalSymbol {
                                                value: ".".to_string(),
                                            },
                                            RepetitionType::One,
                                        ),
                                        ProductionItem::CharacterSet(
                                            CharacterSet {
                                                is_complement: false,
                                                items: vec![CharacterSetItem::CharacterRange(
                                                    '0', '9',
                                                )],
                                            },
                                            RepetitionType::OneOrMore,
                                        ),
                                    ],
                                }),
                                RepetitionType::ZeroOrOne,
                            ),
                            ProductionItem::Group(
                                Box::new(Production {
                                    items: vec![
                                        ProductionItem::CharacterSet(
                                            CharacterSet {
                                                is_complement: false,
                                                items: vec![
                                                    CharacterSetItem::Character('e'),
                                                    CharacterSetItem::Character('E'),
                                                ],
                                            },
                                            RepetitionType::One,
                                        ),
                                        ProductionItem::CharacterSet(
                                            CharacterSet {
                                                is_complement: false,
                                                items: vec![
                                                    CharacterSetItem::Character('-'),
                                                    CharacterSetItem::Character('+'),
                                                ],
                                            },
                                            RepetitionType::ZeroOrOne,
                                        ),
                                        ProductionItem::CharacterSet(
                                            CharacterSet {
                                                is_complement: false,
                                                items: vec![
                                                    CharacterSetItem::CharacterRange(
                                                        '0', '9',
                                                    ),
                                                ],
                                            },
                                            RepetitionType::OneOrMore,
                                        ),
                                    ],
                                }),
                                RepetitionType::ZeroOrOne,
                            ),
                            ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: "ws".to_string(),
                                },
                                RepetitionType::One,
                            ),
                        ],
                    },
                }),
                GrammarItem::Comment(" Optional space: by convention, applied in this grammar after literal chars when allowed".to_string()),
                GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol {
                        name: "ws".to_string(),
                    },
                    rhs: Production {
                        items: vec![ProductionItem::Group(
                            Box::new(Production {
                                items: vec![
                                    ProductionItem::CharacterSet(
                                        CharacterSet {
                                            is_complement: false,
                                            items: vec![
                                                CharacterSetItem::Character(' '),
                                                CharacterSetItem::Tab,
                                                CharacterSetItem::NewLine,
                                            ],
                                        },
                                        RepetitionType::One,
                                    ),
                                    ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "ws".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                ],
                            }),
                            RepetitionType::ZeroOrOne,
                        )],
                    },
                }),
            ],
            ..Default::default()
        };

        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            "root ::= object\nvalue ::= object | array | string | number | (\"true\" | \"false\" | \"null\") ws\nobject ::= \"{\" ws (string \":\" ws value (\",\" ws string \":\" ws value)*)? \"}\" ws\narray ::= \"[\" ws (value (\",\" ws value)*)? \"]\" ws\nstring ::= \"\\\"\" ([^\"\\\\] | \"\\\\\" ([\"\\\\/bfnrt] | \"u\" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* \"\\\"\" ws\nnumber ::= (\"-\"? ([0-9] | [1-9] [0-9]*)) (\".\" [0-9]+)? ([eE] [-+]? [0-9]+)? ws\n# Optional space: by convention, applied in this grammar after literal chars when allowed\nws ::= ([ \\t\\n] ws)?\n"
        );
    }

    #[test]
    fn json_schema_property_underscores_are_sanitized_to_dashes() {
        // schema with three properties containing underscores
        let schema = r#"
        {
            "$schema": "https://json-schema.org/draft/2019-09/schema",
            "type": "object",
            "properties": {
                "text_1": { "type": "string" },
                "text__1": { "type": "string" },
                "__text__": { "type": "string" }
            }
        }
        "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();

        let expected = r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $schema: https://json-schema.org/draft/2019-09/schema
################################################

symbol1-text-1-value ::= string ws
symbol2-text--1-value ::= string ws
symbol3---text---value ::= string ws
root ::= "{" ws "\"text_1\"" ws ":" ws symbol1-text-1-value "," ws "\"text__1\"" ws ":" ws symbol2-text--1-value "," ws "\"__text__\"" ws ":" ws symbol3---text---value "}" ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
ws ::= [ \t\n]*
"#;
        
        pretty_assertions::assert_eq!(s, expected);
    }
}
