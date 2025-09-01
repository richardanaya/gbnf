use std::{primitive, str::FromStr};

use crate::{
    CharacterSet, CharacterSetItem, Grammar, GrammarItem, NonTerminalSymbol, Production,
    ProductionItem, RepetitionType, Rule, TerminalSymbol,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum JsonSchemaParseError {
    #[error("invalid json string: {0}")]
    JsonParseError(#[from] serde_json::Error),
    #[error(
        "failed find schema information, if you are sure the schema conforms to the json schema spec, please open a bug report!"
    )]
    UnknownSchemaType,
    #[error(
        "the value with name `{0}` is expected to be of type array but isn't, can not create grammar"
    )]
    ExpectedValueWithTypeArray(String),
    #[error("array with name `{0}` has no items declared, can not create grammar")]
    ArrayTypeWithoutItems(String),
    #[error("enum with name `{0}` has no variants declared, can not create grammar")]
    EnumTypeWithoutVariants(String),
    #[error("object with name `{0}` has no properties declared, can not create grammar")]
    ObjectTypeWithoutProperties(String),
    #[error("failed to parse constant json value as type `{0}`")]
    ConstParseError(String),
    #[error(
        "failed to find constant json value, if you think this might be a bug please open a bug report!"
    )]
    UnknownConstantValueType,
    #[error("unknown string formatting, no grammar has been implemented for `{0}` yet!")]
    UnknownStringFormat(String),
}

fn create_boolean_grammar_item(name: String) -> GrammarItem {
    GrammarItem::Rule(Rule {
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
    })
}

fn create_number_grammar_item(name: String) -> GrammarItem {
    GrammarItem::Rule(Rule {
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
    })
}

fn create_integer_grammar_item(name: String) -> GrammarItem {
    GrammarItem::Rule(Rule {
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
    })
}

fn create_simple_string_grammar_item(name: String) -> GrammarItem {
    GrammarItem::Rule(Rule {
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
    })
}

#[derive(Debug, strum::IntoStaticStr, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
enum JsonSchemaStringFormat {
    Date,
}

impl JsonSchemaStringFormat {
    pub fn to_grammar_rule(&self) -> (NonTerminalSymbol, Production) {
        match self {
            JsonSchemaStringFormat::Date => (
                NonTerminalSymbol {
                    name: "date".to_string(),
                },
                Production {
                    items: vec![
                        ProductionItem::Terminal(
                            TerminalSymbol {
                                value: "\\\"".to_string(),
                            },
                            RepetitionType::One,
                        ),
                        ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![CharacterSetItem::CharacterRange('0', '9')],
                            },
                            RepetitionType::Exact(4),
                        ),
                        ProductionItem::Terminal(
                            TerminalSymbol {
                                value: "-".to_string(),
                            },
                            RepetitionType::One,
                        ),
                        ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![CharacterSetItem::CharacterRange('0', '9')],
                            },
                            RepetitionType::Exact(2),
                        ),
                        ProductionItem::Terminal(
                            TerminalSymbol {
                                value: "-".to_string(),
                            },
                            RepetitionType::One,
                        ),
                        ProductionItem::CharacterSet(
                            CharacterSet {
                                is_complement: false,
                                items: vec![CharacterSetItem::CharacterRange('0', '9')],
                            },
                            RepetitionType::Exact(2),
                        ),
                        ProductionItem::Terminal(
                            TerminalSymbol {
                                value: "\\\"".to_string(),
                            },
                            RepetitionType::One,
                        ),
                    ],
                },
            ),
        }
    }
}

fn display_string_grammar_item(
    name: String,
    g: &mut Grammar,
    value: &serde_json::Value,
) -> Result<GrammarItem, JsonSchemaParseError> {
    if let Some(string_format) = value.get("format") {
        if let Ok(format_type) = JsonSchemaStringFormat::from_str(
            string_format
                .as_str()
                .ok_or(JsonSchemaParseError::ConstParseError("string".to_string()))?,
        ) {
            let (term_sym, prod) = format_type.to_grammar_rule();
            let primitive_rule = GrammarItem::Rule(Rule {
                lhs: term_sym.clone(),
                rhs: prod,
            });
            g.items.push(primitive_rule);
            Ok(GrammarItem::Rule(Rule {
                lhs: NonTerminalSymbol { name },
                rhs: Production {
                    items: vec![
                        ProductionItem::NonTerminal(term_sym, RepetitionType::One),
                        ProductionItem::NonTerminal(
                            NonTerminalSymbol {
                                name: "ws".to_string(),
                            },
                            RepetitionType::One,
                        ),
                    ],
                },
            }))
        } else {
            Err(JsonSchemaParseError::UnknownStringFormat(
                string_format.as_str().unwrap().to_string(),
            ))
        }
    } else {
        Ok(create_simple_string_grammar_item(name))
    }
}

fn create_array_grammar_items(
    value: &serde_json::Value,
    g: &mut Grammar,
    name: String,
    c: &mut usize,
) -> Result<GrammarItem, JsonSchemaParseError> {
    if let Some(items) = value.get("items") {
        let item_template_name = format!("symbol{}-item", c);
        let new_c = parse_json_schema_to_grammar(items, g, item_template_name.clone(), *c)?;
        *c = new_c;

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

        Ok(GrammarItem::Rule(Rule {
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
        }))
    } else {
        Err(JsonSchemaParseError::ArrayTypeWithoutItems(name))
    }
}

fn create_one_of_grammar_rules(
    value: &serde_json::Value,
    g: &mut Grammar,
    name: String,
    c: &mut usize,
) -> Result<GrammarItem, JsonSchemaParseError> {
    let one_of_array = value
        .as_array()
        .ok_or(JsonSchemaParseError::ExpectedValueWithTypeArray(
            name.clone(),
        ))?;

    if one_of_array.is_empty() {
        return Err(JsonSchemaParseError::ArrayTypeWithoutItems(name));
    }

    let mut possible_symbols: Vec<Production> = vec![];
    let mut possible_names: Vec<Production> = vec![];
    for (value, i) in one_of_array.iter().zip(0..) {
        let new_c =
            parse_json_schema_to_grammar(value, g, format!("symbol-{}-oneof-{}", c, i), *c)?;
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
        *c = new_c;
    }
    // add production for oneof
    Ok(GrammarItem::Rule(Rule {
        lhs: NonTerminalSymbol { name },
        rhs: Production {
            items: vec![ProductionItem::OneOf(possible_names)],
        },
    }))
}

fn create_enum_grammar_items(
    value: &serde_json::Value,
    name: String,
) -> Result<GrammarItem, JsonSchemaParseError> {
    let enum_array = value
        .as_array()
        .ok_or(JsonSchemaParseError::ExpectedValueWithTypeArray(
            name.clone(),
        ))?;

    if enum_array.is_empty() {
        return Err(JsonSchemaParseError::EnumTypeWithoutVariants(name));
    }

    let mut possible_strings: Vec<Production> = vec![];
    for value in enum_array {
        if let Some(value_as_string) = value.as_str() {
            possible_strings.push(Production {
                items: vec![ProductionItem::Terminal(
                    TerminalSymbol {
                        value: format!("\\\"{}\\\"", value_as_string),
                    },
                    RepetitionType::One,
                )],
            });
        } else {
            return Err(JsonSchemaParseError::ConstParseError("string".to_string()));
        }
    }
    // add production for enum
    Ok(GrammarItem::Rule(Rule {
        lhs: NonTerminalSymbol { name },
        rhs: Production {
            items: vec![ProductionItem::OneOf(possible_strings)],
        },
    }))
}

fn create_object_grammar_items(
    value: &serde_json::Value,
    g: &mut Grammar,
    name: String,
    c: &mut usize,
) -> Result<GrammarItem, JsonSchemaParseError> {
    if let Some(properties) = value.get("properties") {
        let mut prop_rules = vec![];
        let mut is_first = true;
        for (key, value) in properties.as_object().unwrap() {
            let new_c =
                parse_json_schema_to_grammar(value, g, format!("symbol{}-{}-value", c, key.replace("_", "-")), *c)?;
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
            *c = new_c;
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

        Ok(GrammarItem::Rule(Rule {
            lhs: NonTerminalSymbol { name: name.clone() },
            rhs: Production {
                items: rhs_start
                    .iter()
                    .chain(prop_rules.iter())
                    .chain(rhs_end.iter())
                    .cloned()
                    .collect(),
            },
        }))
    } else {
        Err(JsonSchemaParseError::ObjectTypeWithoutProperties(name))
    }
}

fn create_const_grammar_item(
    value: &serde_json::Value,
    name: String,
) -> Result<GrammarItem, JsonSchemaParseError> {
    if value.is_string() {
        if let Some(v_as_string) = value.as_str() {
            Ok(GrammarItem::Rule(Rule {
                lhs: NonTerminalSymbol { name },
                rhs: Production {
                    items: vec![ProductionItem::Terminal(
                        TerminalSymbol {
                            value: format!("\\\"{}\\\"", v_as_string),
                        },
                        RepetitionType::One,
                    )],
                },
            }))
        } else {
            Err(JsonSchemaParseError::ConstParseError("string".to_string()))
        }
    } else if value.is_number() {
        if let Some(v_as_number) = value.as_f64() {
            Ok(GrammarItem::Rule(Rule {
                lhs: NonTerminalSymbol { name },
                rhs: Production {
                    items: vec![ProductionItem::Terminal(
                        TerminalSymbol {
                            value: v_as_number.to_string(),
                        },
                        RepetitionType::One,
                    )],
                },
            }))
        } else {
            Err(JsonSchemaParseError::ConstParseError("number".to_string()))
        }
    } else if value.is_boolean() {
        if let Some(v_as_boolean) = value.as_bool() {
            Ok(GrammarItem::Rule(Rule {
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
            }))
        } else {
            Err(JsonSchemaParseError::ConstParseError("boolean".to_string()))
        }
    } else {
        Err(JsonSchemaParseError::UnknownConstantValueType)
    }
}

pub(crate) fn parse_json_schema_to_grammar(
    value: &serde_json::Value,
    g: &mut Grammar,
    name: String,
    symbol_count: usize,
) -> Result<usize, JsonSchemaParseError> {
    let mut c = symbol_count;
    c += 1;

    // if its a basic type, get the type name
    if let Some(t) = value.get("type") {
        if t == "boolean" {
            g.items.push(create_boolean_grammar_item(name));
        } else if t == "number" {
            g.items.push(create_number_grammar_item(name));
        } else if t == "integer" {
            g.items.push(create_integer_grammar_item(name));
        } else if t == "string" {
            let rule = display_string_grammar_item(name, g, value)?;
            g.items.push(rule);
        } else if t == "array" {
            let rule = create_array_grammar_items(value, g, name, &mut c)?;
            g.items.push(rule);
        } else if t == "object" {
            let rule = create_object_grammar_items(value, g, name, &mut c)?;
            g.items.push(rule);
        }
    } else {
        // if its not a basic type, probably "oneOf"
        if let Some(one_of) = value.get("oneOf") {
            let rule = create_one_of_grammar_rules(one_of, g, name, &mut c)?;
            g.items.push(rule);
        } else if let Some(enum_val) = value.get("enum") {
            let rule = create_enum_grammar_items(enum_val, name)?;
            g.items.push(rule);
        } else if let Some(const_val) = value.get("const") {
            // if its not enum , probably constant value
            let rule = create_const_grammar_item(const_val, name)?;
            g.items.push(rule);
        } else {
            return Err(JsonSchemaParseError::UnknownSchemaType);
        }
    }

    Ok(c)
}
