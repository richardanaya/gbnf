# GBNF-rs

A library for working with llama.cpp GBNF files.

Goals of this project:
* offer standard grammars
* handle conversion of JSON schema to GBNF
* parsing and rendering of GBNF files

# Installing

```
cargo add gnbf
```

# JSON-Schema Conversation

```rust
fn simple_json_schema_basic_object_example() {
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
root ::= "{" ws 
 "a" ws ":" ws symbol2-a-value 
 "b" ws ":" ws symbol3-b-value "," ws 
 "c" ws ":" ws symbol4-c-value "," ws 
 "}" ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= ([ \t\n] ws)?
"#
        )
    }
```

See the [documentation](https://docs.rs/gbnf).
