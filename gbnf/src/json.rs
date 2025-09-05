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
/// enum to handle json schema string formats
///
/// [List of Json Schema Formats](https://www.learnjsonschema.com/2020-12/format-assertion/format/)
pub enum JsonSchemaStringFormat {
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

fn dispatch_string_grammar_item(
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
            if !g.recurring_items.contains_key(&term_sym) {
                g.recurring_items.insert(term_sym.clone(), prod);
            }
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
    } else if let Some(t) = value.get("type") {
        if t == "boolean" {
            g.items.push(create_boolean_grammar_item(name));
        } else if t == "number" {
            g.items.push(create_number_grammar_item(name));
        } else if t == "integer" {
            g.items.push(create_integer_grammar_item(name));
        } else if t == "string" {
            let rule = dispatch_string_grammar_item(name, g, value)?;
            g.items.push(rule);
        } else if t == "array" {
            let rule = create_array_grammar_items(value, g, name, &mut c)?;
            g.items.push(rule);
        } else if t == "object" {
            let rule = create_object_grammar_items(value, g, name, &mut c)?;
            g.items.push(rule);
        }
    } else {
        return Err(JsonSchemaParseError::UnknownSchemaType);
    }

    Ok(c)
}


#[cfg(test)]
mod json_schema_test{
    use schemars::{schema_for, JsonSchema};
    use crate::Grammar;

    #[test]
    fn simple_json_schema_boolean() {
        #[derive(JsonSchema)]
        #[allow(dead_code)]
        #[schemars(
            title = "Enumerated Values",
            extend(
                "$id"="https://example.com/enumerated-values.schema.json"
            )
        )]
        struct TestSchema(bool);

        let g = Grammar::from_json_schema_value(&schema_for!(TestSchema).to_value()).unwrap();
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#
        );
    }

    #[test]
    fn simple_json_schema_number() {
        #[derive(JsonSchema)]
        #[allow(dead_code)]
        #[schemars(
            title = "Enumerated Values",
            extend(
                "$id"="https://example.com/enumerated-values.schema.json"
            )
        )]
        struct TestSchema(f32);
        let g = Grammar::from_json_schema_value(&schema_for!(TestSchema).to_value()).unwrap();
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#
        );
    }

    #[test]
    fn simple_json_schema_string() {
        #[derive(JsonSchema)]
        #[allow(dead_code)]
        #[schemars(
            title = "Enumerated Values",
            extend(
                "$id"="https://example.com/enumerated-values.schema.json"
            )
        )]
        struct TestSchema(String);
        let g = Grammar::from_json_schema_value(&schema_for!(TestSchema).to_value()).unwrap();
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#
        );
    }

    #[test]
    fn simple_json_schema_basic_object() {
        #[derive(JsonSchema)]
        #[allow(dead_code)]
        #[schemars(
            title = "Enumerated Values",
            extend(
                "$id"="https://example.com/enumerated-values.schema.json"
            )
        )]
        struct TestSchema{
            a: bool,
            b: f32,
            c: String
        }
        let g = Grammar::from_json_schema_value(&schema_for!(TestSchema).to_value()).unwrap();
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_nested_object() {
        #[derive(JsonSchema)]
        #[allow(dead_code)]
        #[schemars(inline)]
        struct Nested{
            x: bool,
            y: f32,
            z: String
        }
        #[derive(JsonSchema)]
        #[allow(dead_code)]
        #[schemars(
            title = "Enumerated Values",
            extend(
                "$id"="https://example.com/enumerated-values.schema.json"
            )
        )]
        struct TestSchema{
            a: bool,
            b: f32,
            c: Nested
        }
        let g = Grammar::from_json_schema_value(&schema_for!(TestSchema).to_value()).unwrap();
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
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
        //TODO Get this to work as `onfOf`, currently this results in `anyOf` being generated:
        // https://github.com/GREsau/schemars/pull/108
        //
        //#[derive(JsonSchema)]
        //#[allow(dead_code, non_snake_case)]
        //#[schemars(inline)]
        //struct Kind1 {
            //firstName: String,
            //lastName: String,
            //sport: String
        //}
        //#[derive(JsonSchema)]
        //#[allow(dead_code)]
        //#[schemars(untagged)]
        //enum TestSchema {
            //Complex(Kind1),
            //Simple(f32)
        //}
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#,
        )
    }

    #[test]
    fn simple_json_schema_enum() {
        #[derive(JsonSchema)]
        #[allow(dead_code, non_camel_case_types)]
        #[schemars(
            title = "Enumerated Values",
        )]
        enum TestSchema{
            red,
            amber,
            green
        }
        println!("{}", serde_json::to_string_pretty(&schema_for!(TestSchema).to_value()).unwrap());
        let g = Grammar::from_json_schema_value(&schema_for!(TestSchema).to_value()).unwrap();
        let s = g.to_string();

        pretty_assertions::assert_eq!(
            s,
            r#"################################################
# DYNAMICALLY GENERATED JSON-SCHEMA GRAMMAR
# $schema: https://json-schema.org/draft/2020-12/schema
# title: Enumerated Values
################################################

root ::= "\"red\"" | "\"amber\"" | "\"green\""

###############################
# Primitive value type symbols
###############################
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_value_string() {
        // can not be created with schemars as far as I can tell
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_value_number() {
        // can not be created with schemars as far as I can tell
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_schema_value_boolean() {
        // can not be created with schemars as far as I can tell
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#
        )
    }

    #[test]
    fn simple_json_kitchen_sink() {
        // can not be created with schemars as far as I can tell because of `const` usage
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
boolean ::= "true" | "false" ws
integer ::= ("-"? ([0-9] | [1-9] [0-9]*)) ws
null ::= "null" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
ws ::= [ \t\n]*
"#
        )
    }
}
