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
pub enum CharacterSetItem {
    Character(char),
    Tab,
    NewLine,
    CharacterRange(char, char),
    Hex(String),
    Unicode(String),
    Return,
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
            match item {
                GrammarItem::Comment(comment) => {
                    s.push_str(&format!("#{}\n", comment));
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
        };
        let s = g.to_string();
        assert_eq!(s, "root ::= \"yes\" | \"no\"\n");
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
        };
        let s = g.to_string();
        assert_eq!(
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
        };
        let s = g.to_string();
        assert_eq!(
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
        };
        let s = g.to_string();
        assert_eq!(
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
        };
        let s = g.to_string();
        assert_eq!(
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
        };
        let s = g.to_string();
        pretty_assertions::assert_eq!(s, "# Specifies chess moves as a list in algebraic notation, using PGN conventions\n# Force first move to \"1. \", then any 1-2 digit number after, relying on model to follow the pattern\nroot ::= \"1. \" move \" \" move \"\\n\" ([1-9] [0-9]? \". \" move \" \" move \"\\n\")+\nmove ::= (pawn | nonpawn | castle) [+#]?\n# piece type, optional file/rank, optional capture, dest file & rank\nnonpawn ::= [NBKQR] [a-h]? [1-8]? \"x\"? [a-h] [1-8]\n# optional file & capture, dest file & rank, optional promotion\npawn ::= ([a-h] \"x\")? [a-h] [1-8] (\"=\" [NBKQR])?\ncastle ::= \"O-O\" \"-O\"?\n");
    }

    #[test]
    fn list(){
        // root ::= item+
        // # Excludes various line break characters
        // item ::= "- " [^\r\n\x0b\x0c\x85\u2028\u2029]+ "\n"

        let g = Grammar{
            items: vec![
                GrammarItem::Rule(Rule{
                    lhs: NonTerminalSymbol{
                        name: "root".to_string(),
                    },
                    rhs: Production{
                        items: vec![
                            ProductionItem::NonTerminal(NonTerminalSymbol{
                                name: "item".to_string(),
                            }, RepetitionType::OneOrMore),
                        ],
                    },
                }),
                GrammarItem::Comment(" Excludes various line break characters".to_string()),
                GrammarItem::Rule(Rule{
                    lhs: NonTerminalSymbol{
                        name: "item".to_string(),
                    },
                    rhs: Production{
                        items: vec![
                            ProductionItem::Terminal(TerminalSymbol{
                                value: "- ".to_string(),
                            }, RepetitionType::One),
                            ProductionItem::CharacterSet(CharacterSet{
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
                            }, RepetitionType::OneOrMore),
                            ProductionItem::Terminal(TerminalSymbol{
                                value: "\\n".to_string(),
                            }, RepetitionType::One),
                        ],
                    },
                }),
            ],
        };

        let s = g.to_string();
        pretty_assertions::assert_eq!(s, "root ::= item+\n# Excludes various line break characters\nitem ::= \"- \" [^\\r\\n\\x0b\\x0c\\x85\\u2028\\u2029]+ \"\\n\"\n");

    }
}
