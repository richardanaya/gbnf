use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use thiserror::Error;

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
            match item {
                GrammarItem::LineBreak => {
                    s.push('\n');
                }
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

#[derive(Error, Debug)]
pub enum JsonSchemaParseError {
    #[error("failed to parse json schema")]
    Failed,
}

fn parse_json_schema_to_grammar(
    value: &serde_json::Value,
    g: &mut Grammar,
    name: String,
    symbol_count: usize,
) -> Result<usize, JsonSchemaParseError> {
    let mut c = symbol_count;
    c += 1;

    // if its a basic type, get the type name
    let t = match value.get("type") {
        Some(t) => t,
        None => {
            // if its not a basic type, probably "oneOf"
            match value.get("oneOf") {
                Some(one_of) => {
                    let one_of_array = one_of.as_array().unwrap();

                    if one_of_array.is_empty() {
                        return Err(JsonSchemaParseError::Failed);
                    }

                    let mut possible_symbols: Vec<Production> = vec![];
                    let mut possible_names: Vec<Production> = vec![];
                    for (value, i) in one_of_array.iter().zip(0..) {
                        let new_c = parse_json_schema_to_grammar(
                            value,
                            g,
                            format!("symbol-{}-oneof-{}", c, i),
                            c,
                        )?;
                        possible_symbols.push(Production {
                            items: vec![
                                ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: "ws".to_string(),
                                    },
                                    RepetitionType::One,
                                ),
                                ProductionItem::NonTerminal(
                                    NonTerminalSymbol {
                                        name: format!("symbol-{}-oneof-{}", c, i),
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
                        });
                        possible_names.push(Production {
                            items: vec![ProductionItem::NonTerminal(
                                NonTerminalSymbol {
                                    name: format!("symbol-{}-oneof-{}", c, i),
                                },
                                RepetitionType::One,
                            )],
                        });
                        c = new_c;
                    }
                    // add production for oneof
                    g.items.push(GrammarItem::Rule(Rule {
                        lhs: NonTerminalSymbol { name },
                        rhs: Production {
                            items: vec![ProductionItem::OneOf(possible_names)],
                        },
                    }));
                    return Ok(c);
                }
                None => {
                    // lets handle "enum" if its not "oneOf"

                    // if its not a basic type, probably "enum"

                    match value.get("enum") {
                        Some(enum_value) => {
                            let enum_array = enum_value.as_array().unwrap();

                            if enum_array.is_empty() {
                                return Err(JsonSchemaParseError::Failed);
                            }

                            let mut possible_strings: Vec<Production> = vec![];
                            for value in enum_array {
                                let value_as_string = match value.as_str() {
                                    Some(value_as_string) => value_as_string,
                                    None => return Err(JsonSchemaParseError::Failed),
                                };

                                possible_strings.push(Production {
                                    items: vec![ProductionItem::Terminal(
                                        TerminalSymbol {
                                            value: format!("\\\"{}\\\"", value_as_string),
                                        },
                                        RepetitionType::One,
                                    )],
                                });
                            }
                            // add production for enum
                            g.items.push(GrammarItem::Rule(Rule {
                                lhs: NonTerminalSymbol { name },
                                rhs: Production {
                                    items: vec![ProductionItem::OneOf(possible_strings)],
                                },
                            }));
                            return Ok(c);
                        }
                        None => {
                            // if its not enum , probably constant value

                            match value.get("const") {
                                Some(v) => {
                                    if v.is_string() {
                                        let v_as_string = match v.as_str() {
                                            Some(v_as_string) => v_as_string,
                                            None => return Err(JsonSchemaParseError::Failed),
                                        };

                                        // add production for constant value
                                        g.items.push(GrammarItem::Rule(Rule {
                                            lhs: NonTerminalSymbol { name },
                                            rhs: Production {
                                                items: vec![ProductionItem::Terminal(
                                                    TerminalSymbol {
                                                        value: format!("\\\"{}\\\"", v_as_string),
                                                    },
                                                    RepetitionType::One,
                                                )],
                                            },
                                        }));
                                    } else if v.is_number() {
                                        let v_as_number = match v.as_f64() {
                                            Some(v_as_number) => v_as_number,
                                            None => return Err(JsonSchemaParseError::Failed),
                                        };

                                        // add production for constant value
                                        g.items.push(GrammarItem::Rule(Rule {
                                            lhs: NonTerminalSymbol { name },
                                            rhs: Production {
                                                items: vec![ProductionItem::Terminal(
                                                    TerminalSymbol {
                                                        value: v_as_number.to_string(),
                                                    },
                                                    RepetitionType::One,
                                                )],
                                            },
                                        }));
                                    } else if v.is_boolean() {
                                        let v_as_boolean = match v.as_bool() {
                                            Some(v_as_boolean) => v_as_boolean,
                                            None => return Err(JsonSchemaParseError::Failed),
                                        };

                                        // add production for constant value
                                        g.items.push(GrammarItem::Rule(Rule {
                                            lhs: NonTerminalSymbol { name },
                                            rhs: Production {
                                                items: vec![ProductionItem::Terminal(
                                                    TerminalSymbol {
                                                        value: match v_as_boolean {
                                                            true => "true".to_string(),
                                                            false => "false".to_string(),
                                                        },
                                                    },
                                                    RepetitionType::One,
                                                )],
                                            },
                                        }));
                                    } else {
                                        return Err(JsonSchemaParseError::Failed);
                                    }
                                    return Ok(c);
                                }
                                None => return Err(JsonSchemaParseError::Failed),
                            }
                        }
                    }
                }
            };
        }
    };

    if t == "boolean" {
        g.items.push(GrammarItem::Rule(Rule {
            lhs: NonTerminalSymbol { name },
            rhs: Production {
                items: vec![
                    ProductionItem::NonTerminal(
                        NonTerminalSymbol {
                            name: "boolean".to_string(),
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
        }));
    } else if t == "number" {
        g.items.push(GrammarItem::Rule(Rule {
            lhs: NonTerminalSymbol { name },
            rhs: Production {
                items: vec![
                    ProductionItem::NonTerminal(
                        NonTerminalSymbol {
                            name: "number".to_string(),
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
        }));
    } else if t == "integer" {
        g.items.push(GrammarItem::Rule(Rule {
            lhs: NonTerminalSymbol { name },
            rhs: Production {
                items: vec![
                    ProductionItem::NonTerminal(
                        NonTerminalSymbol {
                            name: "integer".to_string(),
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
        }));
    } else if t == "string" {
        g.items.push(GrammarItem::Rule(Rule {
            lhs: NonTerminalSymbol { name },
            rhs: Production {
                items: vec![
                    ProductionItem::NonTerminal(
                        NonTerminalSymbol {
                            name: "string".to_string(),
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
        }));
    } else if t == "array" {
        match value.get("items") {
            Some(items) => {
                let item_template_name = format!("symbol{}-item", c);
                let new_c = parse_json_schema_to_grammar(items, g, item_template_name.clone(), c)?;
                c = new_c;

                let rhs_start = vec![
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
                ];

                let rhs_end = vec![
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
                ];

                g.items.push(GrammarItem::Rule(Rule {
                    lhs: NonTerminalSymbol { name },
                    rhs: Production {
                        items: rhs_start
                            .iter()
                            .chain(
                                [
                                    ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: item_template_name.clone(),
                                        },
                                        RepetitionType::ZeroOrMore,
                                    ),
                                    ProductionItem::NonTerminal(
                                        NonTerminalSymbol {
                                            name: "ws".to_string(),
                                        },
                                        RepetitionType::One,
                                    ),
                                ]
                                .iter(),
                            )
                            .chain(rhs_end.iter())
                            .cloned()
                            .collect(),
                    },
                }));
            }
            None => return Err(JsonSchemaParseError::Failed),
        };
    } else if t == "object" {
        let properties = match value.get("properties") {
            Some(properties) => properties,
            None => return Err(JsonSchemaParseError::Failed),
        };
        let mut prop_rules = vec![];
        let mut is_first = true;
        for (key, value) in properties.as_object().unwrap() {
            let new_c =
                parse_json_schema_to_grammar(value, g, format!("symbol{}-{}-value", c, key.replace("_", "-")), c)?;
            if !is_first {
                prop_rules.push(ProductionItem::Terminal(
                    TerminalSymbol {
                        value: ",".to_string(),
                    },
                    RepetitionType::One,
                ));
                prop_rules.push(ProductionItem::NonTerminal(
                    NonTerminalSymbol {
                        name: "ws".to_string(),
                    },
                    RepetitionType::One,
                ));
            } else {
                is_first = false;
            }
            prop_rules.push(ProductionItem::Terminal(
                TerminalSymbol {
                    value: format!("\\\"{}\\\"", key),
                },
                RepetitionType::One,
            ));
            prop_rules.push(ProductionItem::NonTerminal(
                NonTerminalSymbol {
                    name: "ws".to_string(),
                },
                RepetitionType::One,
            ));
            prop_rules.push(ProductionItem::Terminal(
                TerminalSymbol {
                    value: ":".to_string(),
                },
                RepetitionType::One,
            ));
            prop_rules.push(ProductionItem::NonTerminal(
                NonTerminalSymbol {
                    name: "ws".to_string(),
                },
                RepetitionType::One,
            ));
            prop_rules.push(ProductionItem::NonTerminal(
                NonTerminalSymbol {
                    name: format!("symbol{}-{}-value", c, key.replace("_", "-")),
                },
                RepetitionType::One,
            ));

            c = new_c;
        }

        let rhs_start = vec![
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
        ];

        let rhs_end = vec![
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
        ];

        g.items.push(GrammarItem::Rule(Rule {
            lhs: NonTerminalSymbol { name: name.clone() },
            rhs: Production {
                items: rhs_start
                    .iter()
                    .chain(prop_rules.iter())
                    .chain(rhs_end.iter())
                    .cloned()
                    .collect(),
            },
        }));
    }
    Ok(c)
}

impl Grammar {
    pub fn from_json_schema(schema: &str) -> Result<Grammar, JsonSchemaParseError> {
        let mut g = Grammar { items: vec![] };
        // parse json
        let json = match serde_json::from_str::<serde_json::Value>(schema) {
            Ok(json) => json,
            Err(_) => return Err(JsonSchemaParseError::Failed),
        };

        // add $id, $schema, title as commments at top of file
        g.items.push(GrammarItem::Comment(
            "###############################################".to_string(),
        ));
        g.items.push(GrammarItem::Comment(
            " DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR".to_string(),
        ));
        if let Some(id) = json.get("$id") {
            g.items.push(GrammarItem::Comment(format!(
                " $id: {}",
                id.as_str().unwrap_or("")
            )));
        }
        if let Some(schema) = json.get("$schema") {
            g.items.push(GrammarItem::Comment(format!(
                " $schema: {}",
                schema.as_str().unwrap_or("")
            )));
        }
        if let Some(title) = json.get("title") {
            g.items.push(GrammarItem::Comment(format!(
                " title: {}",
                title.as_str().unwrap_or("")
            )));
        }
        g.items.push(GrammarItem::Comment(
            "###############################################".to_string(),
        ));
        g.items.push(GrammarItem::LineBreak);

        parse_json_schema_to_grammar(&json, &mut g, "root".to_string(), 0)?;

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
        g.items.push(GrammarItem::Rule(Rule {
            lhs: NonTerminalSymbol {
                name: "null".to_string(),
            },
            rhs: Production {
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
        }));
        g.items.push(GrammarItem::Rule(Rule {
            lhs: NonTerminalSymbol {
                name: "boolean".to_string(),
            },
            rhs: Production {
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
        }));
        g.items.push(GrammarItem::Rule(Rule {
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
        }));
        g.items.push(GrammarItem::Rule(Rule {
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
        }));
        g.items.push(GrammarItem::Rule(Rule {
            lhs: NonTerminalSymbol {
                name: "integer".to_string(),
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
        }));
        g.items.push(GrammarItem::Rule(Rule {
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
        }));
        Ok(g)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_json_schema_boolean() {
        let schema = r#"
    {
        "$id": "https://example.com/enumerated-values.schema.json",
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "title": "Enumerated Values",
        "type": "boolean"
    }
            "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $id: https://example.com/enumerated-values.schema.json
# $schema: https://json-schema.org/draft/2020-12/schema
# title: Enumerated Values
################################################

root ::= boolean ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        );
    }

    #[test]
    fn simple_json_schema_number() {
        let schema = r#"
    {
        "$id": "https://example.com/enumerated-values.schema.json",
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "title": "Enumerated Values",
        "type": "number"
    }
            "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $id: https://example.com/enumerated-values.schema.json
# $schema: https://json-schema.org/draft/2020-12/schema
# title: Enumerated Values
################################################

root ::= number ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        );
    }

    #[test]
    fn simple_json_schema_string() {
        let schema = r#"
    {
        "$id": "https://example.com/enumerated-values.schema.json",
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "title": "Enumerated Values",
        "type": "string"
    }
            "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $id: https://example.com/enumerated-values.schema.json
# $schema: https://json-schema.org/draft/2020-12/schema
# title: Enumerated Values
################################################

root ::= string ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        );
    }

    #[test]
    fn simple_json_schema_basic_object() {
        let schema = r#"
    {
        "$id": "https://example.com/enumerated-values.schema.json",
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "title": "Enumerated Values",
        "type": "object",
        "properties": {
            "a": {
                "type": "boolean"
            },
            "b": {
                "type": "number"
            },
            "c": {
                "type": "string"
            }
        }
    }
            "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $id: https://example.com/enumerated-values.schema.json
# $schema: https://json-schema.org/draft/2020-12/schema
# title: Enumerated Values
################################################

symbol1-a-value ::= boolean ws
symbol2-b-value ::= number ws
symbol3-c-value ::= string ws
root ::= "{" ws "\"a\"" ws ":" ws symbol1-a-value "," ws "\"b\"" ws ":" ws symbol2-b-value "," ws "\"c\"" ws ":" ws symbol3-c-value "}" ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_nested_object() {
        let schema = r#"
    {
        "$id": "https://example.com/enumerated-values.schema.json",
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "title": "Enumerated Values",
        "type": "object",
        "properties": {
            "a": {
                "type": "boolean"
            },
            "b": {
                "type": "number"
            },
            "c": {
                "type": "object",
                "properties": {
                    "x": {
                        "type": "boolean"
                    },
                    "y": {
                        "type": "number"
                    },
                    "z": {
                        "type": "string"
                    }
                }
            }
        }
    }
            "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $id: https://example.com/enumerated-values.schema.json
# $schema: https://json-schema.org/draft/2020-12/schema
# title: Enumerated Values
################################################

symbol1-a-value ::= boolean ws
symbol2-b-value ::= number ws
symbol4-x-value ::= boolean ws
symbol5-y-value ::= number ws
symbol6-z-value ::= string ws
symbol3-c-value ::= "{" ws "\"x\"" ws ":" ws symbol4-x-value "," ws "\"y\"" ws ":" ws symbol5-y-value "," ws "\"z\"" ws ":" ws symbol6-z-value "}" ws
root ::= "{" ws "\"a\"" ws ":" ws symbol1-a-value "," ws "\"b\"" ws ":" ws symbol2-b-value "," ws "\"c\"" ws ":" ws symbol3-c-value "}" ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_oneof() {
        let schema = r#"
   {
	"$schema": "https://json-schema.org/draft/2019-09/schema",
	"oneOf": [
      {
        "type" : "object",
        "properties" : {
            "firstName" : {
                "type" : "string"
            },
            "lastName" : {
                "type" : "string"
            },
            "sport" : {
                "type" : "string"
            }
          }
      },
      {
        "type" : "number"
      }
    ]
}
            "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $schema: https://json-schema.org/draft/2019-09/schema
################################################

symbol2-firstName-value ::= string ws
symbol3-lastName-value ::= string ws
symbol4-sport-value ::= string ws
symbol-1-oneof-0 ::= "{" ws "\"firstName\"" ws ":" ws symbol2-firstName-value "," ws "\"lastName\"" ws ":" ws symbol3-lastName-value "," ws "\"sport\"" ws ":" ws symbol4-sport-value "}" ws
symbol-5-oneof-1 ::= number ws
root ::= symbol-1-oneof-0 | symbol-5-oneof-1

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#,
        )
    }

    #[test]
    fn simple_json_schema_enum() {
        let schema = r#"
        {
         "$schema": "https://json-schema.org/draft/2019-09/schema",
         "enum": [
              "red",
              "amber",
              "green"
         ]
     }
                 "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();

        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $schema: https://json-schema.org/draft/2019-09/schema
################################################

root ::= "\"red\"" | "\"amber\"" | "\"green\""

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_value_string() {
        let schema = r#"
        {
         "$schema": "https://json-schema.org/draft/2019-09/schema",
         "const": "red"
     }
                 "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();

        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $schema: https://json-schema.org/draft/2019-09/schema
################################################

root ::= "\"red\""

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_value_number() {
        let schema = r#"
        {
         "$schema": "https://json-schema.org/draft/2019-09/schema",
         "const": 42
     }
                 "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();

        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $schema: https://json-schema.org/draft/2019-09/schema
################################################

root ::= "42"

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_value_boolean() {
        let schema = r#"
        {
         "$schema": "https://json-schema.org/draft/2019-09/schema",
         "const": true
     }
                 "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();

        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $schema: https://json-schema.org/draft/2019-09/schema
################################################

root ::= "true"

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_array() {
        let schema = r#"
        {
         "$schema": "https://json-schema.org/draft/2019-09/schema",
         "type": "array",
         "items": {
             "type": "string"
         }
     }
                 "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();

        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $schema: https://json-schema.org/draft/2019-09/schema
################################################

symbol1-item ::= string ws
root ::= "[" ws symbol1-item* ws "]" ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_kitchen_sink() {
        let schema = r#"
        {
            "$schema": "https://json-schema.org/draft/2019-09/schema",
            "type": "object",
            "properties": {
                "name": {
                    "type": "string"
                },
                "age": {
                    "type": "number"
                },
                "usesAI": {
                    "type": "boolean"
                },
                "favoriteAnimal": {
                    "enum": [
                        "dog",
                        "cat",
                        "none"
                    ]
                },
                "currentAIModel": {
                    "oneOf": [
                        {
                            "type": "object",
                            "properties": {
                                "type": {
                                    "const": "hugging_face"
                                },
                                "name": {
                                    "type": "string"
                                }
                            }
                        },
                        {
                            "type": "object",
                            "properties": {
                                "type": {
                                    "const": "openai"
                                }
                            }
                        }
                    ]
                },
                "favoriteColors": {
                    "type": "array",
                    "items": {
                        "type": "string"
                    }
                }
            }
        }
                 "#;
        let g = Grammar::from_json_schema(schema).unwrap();
        let s = g.to_string();

        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $schema: https://json-schema.org/draft/2019-09/schema
################################################

symbol1-name-value ::= string ws
symbol2-age-value ::= number ws
symbol3-usesAI-value ::= boolean ws
symbol4-favoriteAnimal-value ::= "\"dog\"" | "\"cat\"" | "\"none\""
symbol7-type-value ::= "\"hugging_face\""
symbol8-name-value ::= string ws
symbol-6-oneof-0 ::= "{" ws "\"type\"" ws ":" ws symbol7-type-value "," ws "\"name\"" ws ":" ws symbol8-name-value "}" ws
symbol10-type-value ::= "\"openai\""
symbol-9-oneof-1 ::= "{" ws "\"type\"" ws ":" ws symbol10-type-value "}" ws
symbol5-currentAIModel-value ::= symbol-6-oneof-0 | symbol-9-oneof-1
symbol12-item ::= string ws
symbol11-favoriteColors-value ::= "[" ws symbol12-item* ws "]" ws
root ::= "{" ws "\"name\"" ws ":" ws symbol1-name-value "," ws "\"age\"" ws ":" ws symbol2-age-value "," ws "\"usesAI\"" ws ":" ws symbol3-usesAI-value "," ws "\"favoriteAnimal\"" ws ":" ws symbol4-favoriteAnimal-value "," ws "\"currentAIModel\"" ws ":" ws symbol5-currentAIModel-value "," ws "\"favoriteColors\"" ws ":" ws symbol11-favoriteColors-value "}" ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ \t\n]*
"#
        )
    }

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
        };

        let s = g.to_string();
        pretty_assertions::assert_eq!(
            s,
            "root ::= object\nvalue ::= object | array | string | number | (\"true\" | \"false\" | \"null\") ws\nobject ::= \"{\" ws (string \":\" ws value (\",\" ws string \":\" ws value)*)? \"}\" ws\narray ::= \"[\" ws (value (\",\" ws value)*)? \"]\" ws\nstring ::= \"\\\"\" ([^\"\\\\] | \"\\\\\" ([\"\\\\/bfnrt] | \"u\" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* \"\\\"\" ws\nnumber ::= (\"-\"? ([0-9] | [1-9] [0-9]*)) (\".\" [0-9]+)? ([eE] [-+]? [0-9]+)? ws\n# Optional space: by convention, applied in this grammar after literal chars when allowed\nws ::= ([ \\t\\n] ws)?\n"
        );
    }
}
