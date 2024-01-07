use clap::Parser;
use gbnf::Grammar;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Clone)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(value_name = "JSON_SCHEMA_FILE", help = "Path to JSON schema file")]
    input_json_schema: PathBuf,

    #[arg(
        short,
        value_name = "GBNF_FILE",
        help = "Path to GBNF file",
        default_value = "grammar.gbnf"
    )]
    output_gbnf: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let json_schema = fs::read_to_string(args.input_json_schema)?;
    let gbnf = Grammar::from_json_schema(&json_schema)?;

    let gbnf_file = args.output_gbnf.expect("GBNF file path is required");
    fs::write(gbnf_file, gbnf.to_string())?;

    Ok(())
}
