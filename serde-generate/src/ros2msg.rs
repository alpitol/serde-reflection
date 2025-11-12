use crate::{
    indent::{IndentConfig, IndentedWriter},
};
use serde_reflection::{ContainerFormat, Format, Registry};
use std::{
    collections::{HashMap, HashSet},
    io::{self, Write},
};
use std::io::{Error, ErrorKind};

const LN: &str = "================================================================================";

const TUPLE_FIELDS: [&str; 6] = ["first", "second", "third", "fourth", "fifth", "sixth"];

// Return type of `preprocess_iterator()`.
struct PreprocessedTypes<'b> {
    root_type: (&'b String, &'b ContainerFormat), // Name and type of the root type
    types: Vec<(&'b String, &'b ContainerFormat)>, // Name and type for the rest types
    arrays: HashMap<String, (String, usize)>, // User's array types e.g. <"Vec3", ("f32", 3)>,
    contains_unit: bool, // True there is at least one unit type somewhere in the root struct.
}

fn err<T>(msg: &str) -> Result<T, Error> {
    Err(Error::new(ErrorKind::InvalidInput, msg))
}

// Add context to error message
fn add_field_name(field: &str) -> impl FnOnce(Error) -> Error + '_ {
    move |e: Error|
        match e.kind() { // double closure
            ErrorKind::InvalidInput => Error::new(
                ErrorKind::InvalidInput,
                format!("Field `{field}`: {e}").as_str(),
            ),
            _ => e,
        }
}

// Add context to error message
fn add_struct_name(name: &str) -> impl FnOnce(Error) -> Error + '_ {
    move |e: Error|
        match e.kind() { // double closure
            ErrorKind::InvalidInput => Error::new(
                ErrorKind::InvalidInput,
                format!("Could not serialize `{name}`: {e}").as_str()
            ),
            _ => e,
        }
}

/// Main configuration Msg file generation
pub struct CodeGenerator<'a> {
    name: String,
    overrides: &'a HashMap<String, String>,
    array_types: Option<&'a HashSet<String>>,
}

/// Shared state for the code generation of a Rust source file.
struct Ros2msgEmitter<'a, T> {
    /// Writer.
    out: IndentedWriter<T>,
    /// Generator.
    generator: &'a CodeGenerator<'a>,
}

impl<'a> CodeGenerator<'a> {
    /// Create a Rust code generator for a single schema file.
    ///
    /// # Type Parameters:
    /// * `RootType`:
    ///   Only those types are generated that are part of this type
    ///
    /// # Parameters
    /// * `name`: The root type of the ros2msg schema file. For example, for rust type `MyType`
    ///    the name is "MyType". Please use `serde_name::trace_name::<MyType>()` to obtain the
    ///    name using reflection.
    /// * `overrides`: Assing ros2 schema names to your rust types.
    ///   For example, to assign ros2 schema to a rust type `MyType`, include a key-value
    ///   pair `("MyType", "pkg/MyType")` into the map. It is recommended to use
    ///   `serde_name::trace_name::<MyType>()` to obtain the type name by reflection.
    ///   If entry is not found for a type, a path of form `dummy/MyType` is given.
    ///
    /// Returns `None` if root type is a primitive rust type instead of struct or tuple
    pub fn new(name: &str, overrides: &'a HashMap<String, String>) -> Self {
        Self{
            name: name.to_owned(),
            overrides,
            array_types: None,
        }
    }


    /// Defines array types that will replaced with a corresponding ros2msg array.
    /// For example, if there is a struct `MyVec2{x: f32, y: f32}`, defining "MyVec2"
    /// as an array will replace it with a ros2msg primitive "float32[2]" in the schema file.
    /// Please use `serde_name::trace_name::<MyType>()` to obtain the type name by reflection.
    ///
    /// Note 1:
    ///     All elements of the array type must have the same type, or else schema generation
    ///     will fail.
    /// Note 2:
    ///     If the array type is a generic struct, like `MyVec2<T>`, then only one variant of it
    ///     (like `MyVec2<f32>`,) can exists in the schema file at a time. This limitation of serde,
    ///     as it does not save the generic type information. Workaround for this issue is to
    ///     write (de)serialize impl blocks by hand and add name adapters, as instructed here:
    ///     https://docs.rs/serde-name/0.2.1/serde_name/#overriding-serde-names"
    pub fn define_array_types(mut self, array_types: &'a HashSet<String>) -> Self {
        self.array_types = Some(array_types);
        self
    }

    // If given array Vec2<f32>, extracts type "f32" and length 2.
    // Asserts if fields mix different types, like f32 and f64
    // Limitation: if the element type is another array, this may or may not work, depending
    // if the inner type has been processed earlier or not. This comes from alphabetical order.
    // Could be fixed, but requires extra work. Also, having multiple different instances of
    // the same generic array type is poorly supported anyways, as it requires name adapters. See
    // https://docs.rs/serde-name/0.2.1/serde_name/#overriding-serde-names
    fn get_value_type(
        &self,
        format: &ContainerFormat,
        arrays: &HashMap<String, (String, usize)>
    ) -> io::Result<(String, usize)> {
        let mut types = Vec::new();

        match format {
            ContainerFormat::TupleStruct(formats) => {
                for (i, format) in formats.iter().enumerate() {
                    let field = TUPLE_FIELDS.get(i).expect("Max size for tupple is 6.");
                    types.push(self.quote_type(format, arrays)
                        .map_err(add_field_name(field))?)
                }
            },
            ContainerFormat::Struct(fields) => {
                for field in fields {
                    types.push(self.quote_type(&field.value, arrays)
                        .map_err(add_field_name(&field.name))?);
                }
            }
            _ => return err("Array type must be either Struct or tupple")
        }
        for t in &types {
            if *t != types[0] {
                return err(&format!(
                    "Is not an array: contains different types {} and {}",
                    types[0], t
                ))
            }
        }
        let len = types.len();
        Ok((types.pop().unwrap(), len))
    }

    // Checks if container contains at least one unit type.
    // Unit type requires a ros2 declaration to the end of schema file.
    fn contains_unit_type(container_format: &ContainerFormat) -> bool {
        let formats: Vec<Format> = match container_format {
            ContainerFormat::NewTypeStruct(format) => vec![(**format).clone()],
            ContainerFormat::Struct(fields) => fields.iter().map(|field| field.value.clone()).collect(),
            ContainerFormat::TupleStruct(formats) => formats.clone(),
            _ => Vec::new(),
        };

        fn contains_unit_type_reqursive(format: &Format) -> bool {
            match format {
                Format::Unit => true,
                Format::Seq(format) => contains_unit_type_reqursive(format),
                Format::Option(format) => contains_unit_type_reqursive(format),
                Format::Tuple(formats) => formats.iter().any(contains_unit_type_reqursive),
                _ => false
            }
        }

        formats.iter().any(contains_unit_type_reqursive)
    }

    // Processes an item from the middle of iterator first, collect data.
    fn preprocess_iterator<'b>(
        &'b self,
        registry: &'b Registry
    ) -> io::Result<PreprocessedTypes<'b>> {
        let mut root_type = None;

        let mut contains_unit = false;
        let mut types = Vec::new();

        let mut collected_array_types = HashMap::new();

        for (name, format) in registry.iter() {
            if **name == self.name {
                root_type = Some((name, format));
                continue
            } else if let Some(array_types) = self.array_types {
                if array_types.contains(name) {
                    let (typ, len) = self.get_value_type(format, &collected_array_types)
                        .map_err(add_struct_name(name))?;
                    match collected_array_types.get(name) {
                        None => {
                            collected_array_types.insert(name.into(), (typ, len));
                        }
                        Some(_) => return err(&format!(
                            "Struct {name} contains multiple generic instantiations of the same\n\
                            array type, which is not supported by serde-reflection.\n\
                            Workaround:
                            Add serialize adapter impls to your array type like here\n\
                            https://docs.rs/serde-name/0.2.1/serde_name/#overriding-serde-names"
                        ))
                    }
                }
            }
            contains_unit |= Self::contains_unit_type(format);
            types.push((name, format))
        }

        let root_type = match root_type {
            Some(v) => v,
            None => return err(&format!("No type matches name of root type {}", self.name))
        };

        Ok(PreprocessedTypes {
            root_type,
            types,
            arrays: collected_array_types,
            contains_unit
        })
    }

    /// Write container definitions in ros2 msg format.
    pub fn output(&self, out: &mut dyn Write, registry: &Registry) -> io::Result<()> {
        let mut emitter = Ros2msgEmitter {
            out: IndentedWriter::new(out, IndentConfig::Space(4)),
            generator: self,
        };

        let PreprocessedTypes {root_type, types, arrays, contains_unit}
            = self.preprocess_iterator(registry)?;

        // First type is the root type, and its name is not written to the schema,
        // because it is in the filename of schema.
        emitter.output_fields(root_type.1, &arrays).map_err(add_struct_name(root_type.0))?;

        for (name, format) in types {
            // If user has provided type name translation table, rename rust struct using that.
            // Struct can be renamed with ros2 schema e.g. "package/MyStruct" or with
            // ros2 array type, e.g. "float32[2]".
            if arrays.contains_key(name) {
                continue // User has assigned this type to an array (e.g. Vec2<f32> -> float32[2])
            }
            let schema_path = match self.overrides.get(name) {
                Some(replacement) => replacement.into(), // Schema name like "package/MyStruct"
                None => format!("dummy/{name}") // No replacement found
            };
            writeln!(emitter.out, "{LN}")?;
            writeln!(emitter.out, "MSG: {schema_path}")?;
            emitter.output_fields(format, &arrays).map_err(add_struct_name(&schema_path))?;
        }
        if contains_unit { // If we came across a unit type, write definition for that
            writeln!(emitter.out, "{LN}\nMSG: example_interfaces/msg/Empty")?;
        }
        Ok(())
    }

    #[allow(clippy::only_used_in_recursion)] // Clippy thinks `field` is unused parameter
    fn quote_type(
        &self,
        format: &Format,
        arrays: &HashMap<String, (String, usize)>,
    ) -> io::Result<String> {
        use Format::*;
        let ros2type = match format {
            TypeName(typename) => { // user-defined struct as a type of a field. E.g. "MyType".
                match arrays.get(typename) {
                    Some((typ, len)) => { // Special case: is user-defined array type
                        let element_type = self.overrides.get(typ).unwrap_or(typ);
                        format!("{element_type}[{len}]")
                    }
                    None => {
                        // Replace rust type name with ros2 message path, if it is given
                        match self.overrides.get(typename) {
                            Some(replacement) => replacement.into(), // e.g. "pkg/TypeName"
                            None => format!("dummy/{typename}") // No replacement found
                        }
                    }
                }
            },
            Unit => "std_msgs/Empty".into(),
            Bool => "bool".into(),
            I8 => "int8".into(),
            I16 => "int16".into(),
            I32 => "int32".into(),
            I64 => "int64".into(),
            I128 => return err("128 bit integers not supported in ros2 msg"),
            U8 => "uint8".into(),
            U16 => "uint16".into(),
            U32 => "uint32".into(),
            U64 => "uint64".into(),
            U128 => return err("128 bit integers not supported in ros2 msg"),
            F32 => "float32".into(),
            F64 => "float64".into(),
            Char => "char".into(),
            Str => "string".into(),
            Bytes => "uint8[]".into(),
            Seq(format) => format!("{}[]", self.quote_type(format, arrays)?),
            TupleArray { content, size } =>
                format!("sequence<{}, {size}>", self.quote_type(content, arrays)?),
            Option(_format) => return err("Option type not supported in ros2 msg"),
            Map {..} => return err("Maps not supported in ros2 msg"),
            Tuple(_formats) => return err("Tuples not supported in ros2 msg"),
            Variable(_) => panic!("unexpected value"),
        };
        Ok(ros2type)
    }
}


impl<'a, T> Ros2msgEmitter<'a, T>
where
    T: Write,
{
    fn output_tuple(
        &mut self,
        formats: &[Format],
        arrays: &HashMap<String, (String, usize)>,
    ) -> io::Result<()> {
        for (i, format) in formats.iter().enumerate() {
            let field = TUPLE_FIELDS.get(i).expect("Max size for tupple is 6.");
            let typ = self.generator.quote_type(format, arrays)
                .map_err(add_field_name(field))?;
            writeln!(self.out, "{typ} {field}")?;
        }
        Ok(())
    }

    fn output_fields(
        &mut self,
        format: &ContainerFormat,
        arrays: &HashMap<String, (String, usize)>,
    ) -> io::Result<()> {
        use ContainerFormat::*;
        match format {
            UnitStruct => (),
            NewTypeStruct(format) => self.output_tuple(&[(**format).clone()], arrays)?,
            TupleStruct(formats) => self.output_tuple(formats, arrays)?,
            Struct(fields) => {
                for field in fields {
                    let typ = self.generator.quote_type(&field.value, arrays)
                        .map_err(add_field_name(&field.name))?;
                    writeln!(self.out, "{} {}", typ, &field.name)?;
                }
            }
            Enum(_) => return err("Enums not supported in ros2 msg"),
        }
        Ok(())
    }
}

