use crate::{
    indent::{IndentConfig, IndentedWriter},
};
use serde_reflection::{ContainerFormat, Format, Registry};
use std::{
    collections::HashMap,
    io::{self, Write},
};
use std::io::{Error, ErrorKind};

fn err<T>(msg: &str) -> Result<T, Error> {
    Err(Error::new(ErrorKind::InvalidInput, msg))
}

/// Main configuration Msg file generation
pub struct CodeGenerator<'a> {
    schema_names: Option<&'a HashMap<String, String>>,
}

/// Shared state for the code generation of a Rust source file.
struct Ros2msgEmitter<'a, T> {
    /// Writer.
    out: IndentedWriter<T>,
    /// Generator.
    generator: &'a CodeGenerator<'a>,
    contains_unit: bool, // Tells if any field is unit, thus include builtin_interfaces/msg/Empty
}

impl<'a> CodeGenerator<'a> {
    /// Create a Rust code generator for the given config.
    pub fn new() -> Self {
        Self{schema_names: None}
    }

    /// Rename rust types with ros2 schema names. The keys in map are rust struct names,
    /// e.g. "MyMessage", and values ros2 msg paths, e.g. "pkg/MyMesage".
    pub fn with_schema_names(mut self, schema_names: &'a HashMap<String, String>) -> Self {
        self.schema_names = Some(schema_names);
        self
    }

    // Matches if `name` has form like "float32[2]" or "float64[8]"
    fn is_float_array(name: &str) -> bool {
        name.len() >= 9
            && name.starts_with("float")
            && name.is_ascii()
            && (name[5..7] == *"32" ||  name[5..7] == *"64")
            && name[7..8] == *"["
            && name[name.len()-1..name.len()] == *"]"
            && name[8..name.len()-1].parse::<u32>().is_ok()
    }


    /// Write container definitions in ros2 msg format.
    pub fn output(&self, out: &mut dyn Write, registry: &Registry) -> io::Result<()> {
        let mut emitter = Ros2msgEmitter {
            out: IndentedWriter::new(out, IndentConfig::Space(4)),
            generator: self,
            contains_unit: false,
        };

        let d = "================================================================================";
        for (i, (name, format)) in registry.iter().enumerate() {
            let prefix = if i == 0 {
                ""
            } else {
                writeln!(emitter.out, "{d}")?;
                "MSG: "
            };

            // If user has provided type name translation table, rename rust struct.
            // Struct can be renamed with schema path e.g. "package/MyStruct" or with
            // ros2 array type, e.g. float32[2]. If latter, then no need to add schema for the type.
            let replacement_name = self.schema_names.and_then(|n| n.get(name).map(|s|s.as_str()));
            let name = match replacement_name {
                Some(replacement) => if Self::is_float_array(replacement) {
                    continue // User has assigned this to a float array like float32[2]
                } else {
                    replacement // Schema name like "package/MyStruct"
                },
                None => name // No replacement found
            };

            emitter.output_container(name, format, prefix).map_err(|e|
                match e.kind() {
                    ErrorKind::InvalidInput => Error::new(
                        ErrorKind::InvalidInput,
                        format!("Could not serialize `{name}` {e}").as_str()
                    ),
                    _ => e,
                }
            )?;
        }
        if emitter.contains_unit {
            writeln!(emitter.out, "{d}\nMSG: example_interfaces/msg/Empty")?;
        }
        Ok(())
    }
}

impl<'a> Default for CodeGenerator<'a> {
    fn default() -> Self {
        Self::new()
    }
}


impl<'a, T> Ros2msgEmitter<'a, T>
where
    T: Write,
{
    #[allow(clippy::only_used_in_recursion)] // Clippy thinks `field` is unused parameter
    fn quote_type(&mut self, format: &Format, field: &str) -> io::Result<String> {
        use Format::*;
        let ros2type = match format {
            TypeName(x) => { // user-defined struct as a field. Assuming it is known.
                // If user has provided name translation table, rename the struct with schema name
                // or with primitive array type like "float32[2]"
                match self.generator.schema_names.and_then(|n| n.get(x).map(|s| s.as_str())) {
                    Some(replacement) => replacement.into(),
                    None => x.into(),
                }
            },
            Unit => {
                self.contains_unit = true;
                "std_msgs/Empty".into()
            },
            Bool => "bool".into(),
            I8 => "int8".into(),
            I16 => "int16".into(),
            I32 => "int32".into(),
            I64 => "int64".into(),
            I128 => return err("field `{field}`: 128 bit integers not supported in ros2 msg"),
            U8 => "uint8".into(),
            U16 => "uint16".into(),
            U32 => "uint32".into(),
            U64 => "uint64".into(),
            U128 => return err("field `{field}`: 128 bit integers not supported in ros2 msg"),
            F32 => "float32".into(),
            F64 => "float64".into(),
            Char => "char".into(),
            Str => "string".into(),
            Bytes => "uint8[]".into(),
            Seq(format) => format!("{}[]", self.quote_type(format, field)?),
            TupleArray { content, size } =>
                format!("sequence<{}, {size}>", self.quote_type(content, field)?),
            Option(_format) => return err("field `{field}`: Option type not supported in ros2 msg"),
            Map {..} => return err("field `{field}`: Maps not supported in ros2 msg"),
            Tuple(_formats) => return err("field `{field}`: Tuples not supported in ros2 msg"),
            Variable(_) => panic!("unexpected value"),
        };
        Ok(ros2type)
    }

    fn output_tuple(&mut self, formats: &[Format]) -> io::Result<()> {
        const TUPLE_FIELDS: [&str; 6] = ["first", "second", "third", "fourth", "fifth", "sixth"];
        for (i, format) in formats.iter().enumerate() {
            let field = TUPLE_FIELDS.get(i).expect("Max size for tupple is 6.");
            let typ = self.quote_type(format, field)?;
            writeln!(self.out, "{typ} {field}")?;
        }
        Ok(())
    }

    fn output_container(
        &mut self,
        name: &str,
        format: &ContainerFormat,
        prefix: &str
    ) -> io::Result<()> {
        use ContainerFormat::*;

        writeln!(self.out, "{prefix}{name}")?;
        match format {
            UnitStruct => (),
            NewTypeStruct(format) => self.output_tuple(&[(**format).clone()])?,
            TupleStruct(formats) => self.output_tuple(formats)?,
            Struct(fields) => {
                for field in fields {
                    let typ = self.quote_type(&field.value, &field.name)?;
                    writeln!(self.out, "{} {}", typ, &field.name)?;
                }
            }
            Enum(_) => return err("Enums not supported in ros2 msg"),
        }
        Ok(())
    }
}

