# GBNF-rs

A library for working with llama.cpp GBNF files. This project is meant to help make it easier to constrain and guide GBNF driven AIs like [llama.cpp](https://github.com/ggerganov/llama.cpp) using JSON schema https://json-schema.org/ ( a way to define the shape of JSON data ).

* Data structures for representing GBNF
* Rendering of a GBNF file from data structures
* Conversion of a useful subset of JSON schema to GBNF grammar
* Easily installable CLI converter [jsonschema2gbnf](https://github.com/richardanaya/gbnf/tree/main/jsonschema2gbnf) that uses library 
* **MIT** licensed

This library was primarily built for it's sister project, an LLM API [epistemology](https://github.com/richardanaya/epistemology/).

# Installing

```
cargo add gnbf
```

# JSON schema support

Currently this library can convert a limited but very useful subset of JSON schema:
* boolean, number, string
* object with all required properties
* enum
* oneOf
* property order of objects is preserved

Known issues:
* objects with property names with underscores don't translate well right now

Here's one of the most complext JSON schemas that can be handled right now:

```json
{
    "$schema": "https://json-schema.org/draft/2019-09/schema",
    "type": "object",
    "properties": {
        "name": {
            "description": "name of a computer user",
            "type": "string"
        },
        "age": {
            "description": "age of a computer user",
            "type": "number"
        },
        "usesAI": {
            "description": "do they use AI",
            "type": "boolean"
        },
        "favoriteAnimal": {
            "description": "favorite animal",
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
                            "value": "hugging_face"
                        },
                        "name": {
                            "description": "name of hugging face model",
                            "type": "string"
                        }
                    }
                },
                {
                    "type": "object",
                    "properties": {
                        "type": {
                            "value": "openai"
                        }
                    }
                }
            ]
        }
    }
}
```

# JSON-Schema Converting to AI Grammar

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
 "a" ws ":" ws symbol1-a-value 
 "b" ws ":" ws symbol2-b-value "," ws 
 "c" ws ":" ws symbol3-c-value "," ws 
 "}" ws

###############################
# Primitive value type symbols
###############################
null ::= "null" ws
boolean ::= "true" | "false" ws
string ::= "\"" ([^"\\] | "\\" (["\\/bfnrt] | "u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]))* "\"" ws
number ::= ("-"? ([0-9] | [1-9] [0-9]*)) ("." [0-9]+)? ([eE] [-+]? [0-9]+)? ws
ws ::= [ ]
"#
        )
    }
```

# Future goals of this project:
* offer standard grammars
* handle useful conversion of JSON schema to GBNF (we won't be able to handle it all probably).
* parsing of GBNF files using Nom 7.

I'm totally down for contributors, please add tests.

See the [documentation](https://docs.rs/gbnf).

# Attribution

Multiple **MIT** licensed examples of GBNF were used from the `llama.cpp` [examples for grammar](https://github.com/ggerganov/llama.cpp/tree/master/grammars) for automated tests for compliance and general inspiration for this project from [python JSON schema converter](https://github.com/ggerganov/llama.cpp/blob/master/examples/json-schema-to-grammar.py). Thank you.


