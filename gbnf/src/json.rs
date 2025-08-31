use crate::{
    Grammar, GrammarItem, NonTerminalSymbol, Production, ProductionItem, RepetitionType, Rule,
    TerminalSymbol,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum JsonSchemaParseError {
    #[error("failed to parse json schema")]
    Failed,
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
                parse_json_schema_to_grammar(value, g, format!("symbol{}-{}-value", c, key), c)?;
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
                    name: format!("symbol{}-{}-value", c, key),
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
