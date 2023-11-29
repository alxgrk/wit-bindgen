use anyhow::Result;
use heck::{ToLowerCamelCase, ToShoutySnakeCase, ToUpperCamelCase};
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    iter, mem,
    ops::Deref,
};
use wit_bindgen_core::{uwrite, uwriteln, wit_parser::{
    abi::{AbiVariant, WasmType},
    Docs, Enum, Flags, FlagsRepr, Function, FunctionKind, Int, InterfaceId, Record,
    Resolve, Result_, SizeAlign, Tuple, Type, TypeDef, TypeDefKind, TypeId, TypeOwner,
    Variant, WorldId,
}, Files, InterfaceGenerator as _, Ns, WorldGenerator, Direction, Source, abi};
use wit_bindgen_core::abi::{Bindgen, Bitcast, Instruction, LiftLower};
use wit_bindgen_core::wit_parser::WorldKey;

const IMPORTS: &str = "\
import kotlin.collections.ArrayList

// helper methods for string conversion, made internal in Kotlin 1.9
import toChars
import toCodePoint

import kotlin.wasm.WasmImport
import kotlin.wasm.unsafe.Pointer
import kotlin.wasm.unsafe.withScopedMemoryAllocator\
";

#[derive(Default, Debug, Clone)]
#[cfg_attr(feature = "clap", derive(clap::Args))]
pub struct Opts {
    /// Whether or not to generate a stub class for exported functions
    #[cfg_attr(feature = "clap", arg(long))]
    pub generate_stub: bool,
}

impl Opts {
    pub fn build(&self) -> Box<dyn WorldGenerator> {
        Box::new(Kotlin {
            opts: self.clone(),
            ..Kotlin::default()
        })
    }
}

struct InterfaceFragment {
    src: String,
    stub: String,
}

#[derive(Default)]
pub struct Kotlin {
    opts: Opts,
    name: String,
    return_area_size: usize,
    return_area_align: usize,
    tuple_counts: HashSet<usize>,
    needs_cleanup: bool,
    needs_result: bool,
    interface_fragments: HashMap<String, Vec<InterfaceFragment>>,
    world_fragments: Vec<InterfaceFragment>,
    sizes: SizeAlign,
    interface_names: HashMap<InterfaceId, String>,
}

impl Kotlin {
    fn qualifier(&self) -> String {
        format!("{}.", self.name)
    }

    fn interface<'a>(&'a mut self, resolve: &'a Resolve, name: &'a str) -> InterfaceGenerator<'a> {
        InterfaceGenerator {
            src: String::new(),
            stub: String::new(),
            external_functions: Vec::new(),
            gen: self,
            resolve,
            name,
        }
    }
}

impl WorldGenerator for Kotlin {
    fn preprocess(&mut self, resolve: &Resolve, world: WorldId) {
        self.name = world_name(resolve, world);
        self.sizes.fill(resolve);
    }

    fn import_interface(
        &mut self,
        resolve: &Resolve,
        key: &WorldKey,
        id: InterfaceId,
        _files: &mut Files,
    ) {
        let name = interface_name(resolve, key, Direction::Import);
        self.interface_names.insert(id, name.clone());

        let mut gen = self.interface(resolve, &name);
        uwriteln!(
            gen.src,
            "object {name} {{"
        );
        gen.types(id);

        for (_, func) in resolve.interfaces[id].functions.iter() {
            gen.import(&resolve.name_world_key(key), func);
        }
        uwriteln!(gen.src, "}}\n");

        for external_function in &gen.external_functions {
            uwriteln!(gen.src, "{external_function}");
        }

        gen.add_interface_fragment();
    }

    fn export_interface(
        &mut self,
        resolve: &Resolve,
        key: &WorldKey,
        id: InterfaceId,
        _files: &mut Files,
    ) -> Result<()> {
        let name = interface_name(resolve, key, Direction::Export);
        self.interface_names.insert(id, name.clone());

        let mut gen = self.interface(resolve, &name);
        uwriteln!(
            gen.src,
            "object {name} {{"
        );
        gen.types(id);

        for (_, func) in resolve.interfaces[id].functions.iter() {
            gen.export(Some(&resolve.name_world_key(key)), func);
        }
        uwriteln!(gen.src, "}}");

        gen.add_interface_fragment();
        Ok(())
    }

    fn import_funcs(
        &mut self,
        resolve: &Resolve,
        world: WorldId,
        funcs: &[(&str, &Function)],
        _files: &mut Files,
    ) {
        let name = world_name(resolve, world);
        let mut gen = self.interface(resolve, &name);

        for (_, func) in funcs {
            gen.import("$root", func);
        }

        gen.add_world_fragment();
    }

    fn export_funcs(
        &mut self,
        resolve: &Resolve,
        world: WorldId,
        funcs: &[(&str, &Function)],
        _files: &mut Files,
    ) -> Result<()> {
        let name = world_name(resolve, world);
        let mut gen = self.interface(resolve, &name);

        for (_, func) in funcs {
            gen.export(None, func);
        }

        gen.add_world_fragment();
        Ok(())
    }

    fn import_types(
        &mut self,
        resolve: &Resolve,
        world: WorldId,
        types: &[(&str, TypeId)],
        _files: &mut Files,
    ) {
        let name = world_name(resolve, world);
        let mut gen = self.interface(resolve, &name);

        for (ty_name, ty) in types {
            gen.define_type(ty_name, *ty);
        }

        gen.add_world_fragment();
    }

    fn finish(&mut self, resolve: &Resolve, id: WorldId, files: &mut Files) {
        let name = world_name(resolve, id);
        let package = package_name();

        let mut src = Source::default();
        let version = env!("CARGO_PKG_VERSION");
        wit_bindgen_core::generated_preamble(&mut src, version);

        uwrite!(
            src,
            "package {package};

             {IMPORTS}
             // import kotlin.wasm.unsafe.CustomSection;

             object {name} {{
            "
        );

        src.push_str(
            &self
                .world_fragments
                .iter()
                .map(|f| f.src.deref())
                .collect::<Vec<_>>()
                .join("\n"),
        );

        let mut producers = wasm_metadata::Producers::empty();
        producers.add(
            "processed-by",
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION"),
        );

        let component_type = wit_component::metadata::encode(
            resolve,
            id,
            wit_component::StringEncoding::UTF8,
            Some(&producers),
            None,
        )
        .unwrap();

        let component_type = component_type
            .into_iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<Vec<_>>()
            .concat();

        // TODO
        uwriteln!(
            src,
            r#"
            //@CustomSection(name = "component-type:{name}")
            private const val __WIT_BINDGEN_COMPONENT_TYPE = "{component_type}"
            "#
        );

        for &count in &self.tuple_counts {
            if count == 0 {
                continue;
            }

            let type_params = format!(
                "<{}>",
                (0..count)
                    .map(|index| format!("T{index}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );

            let value_params = (0..count)
                .map(|index| format!("val f{index}: T{index}"))
                .collect::<Vec<_>>()
                .join(", ");

            uwrite!(
                src,
                "data class Tuple{count}{type_params}({value_params})\n"
            )
        }

        if self.needs_result {
            src.push_str(
                r#"
                sealed interface Result<T, E> {
                    class Ok<T, E>(val value: T) : Result<T, E>
                    class Err<T, E>(val value: E) : Result<T, E>
                    companion object {
                        fun <Ok, Err> ok(ok: Ok): Result<Ok, Err> = Ok(ok)
                        fun <Ok, Err> err(err: Err): Result<Ok, Err> = Err(err)
                    }
                }
                "#,
            )
        }

        if self.needs_cleanup {
            src.push_str(
                "
                data class Cleanup (val address: Int, val size: Int, val align: Int)
                ",
            );
        }

        if self.return_area_align > 0 {
            let size = self.return_area_size;
            let align = self.return_area_align;

            // TODO
            uwriteln!(
                src,
                "val RETURN_AREA: UInt = Pointer({size}, {align})",
            );
        }

        src.push_str("}\n");

        files.push(&format!("{name}.kt"), indent(&src).as_bytes());

        let generate_stub =
            |package: &str, name, fragments: &[InterfaceFragment], files: &mut Files| {
                let b = fragments
                    .iter()
                    .map(|f| f.stub.deref())
                    .collect::<Vec<_>>()
                    .join("\n");

                let mut body = Source::default();
                wit_bindgen_core::generated_preamble(&mut body, version);
                uwriteln!(
                    &mut body,
                    "package {package};

                 {IMPORTS}

                 class {name} {{
                     {b}
                 }}
                "
                );

                files.push(&format!("{name}.kt"), indent(&body).as_bytes());
            };

        if self.opts.generate_stub {
            generate_stub(
                &package,
                format!("{name}Impl"),
                &self.world_fragments,
                files
            );
        }

        for (name, fragments) in &self.interface_fragments {
            let b = fragments
                .iter()
                .map(|f| f.src.deref())
                .collect::<Vec<_>>()
                .join("\n");

            let mut body = Source::default();
            wit_bindgen_core::generated_preamble(&mut body, version);
            uwriteln!(
                &mut body,
                "package {package};

                 {IMPORTS}

                 {b}
                "
            );

            files.push(&format!("{name}.kt"), indent(&body).as_bytes());

            if self.opts.generate_stub {
                generate_stub(&package, format!("{name}Impl"), fragments, files);
            }
        }
    }
}

struct InterfaceGenerator<'a> {
    src: String,
    stub: String,
    external_functions: Vec<String>,
    gen: &'a mut Kotlin,
    resolve: &'a Resolve,
    name: &'a str,
}

impl InterfaceGenerator<'_> {
    fn qualifier(&self, when: bool, ty: &TypeDef) -> String {
        if let TypeOwner::Interface(id) = &ty.owner {
            if let Some(name) = self.gen.interface_names.get(id) {
                if name != self.name {
                    return format!("{name}.");
                }
            }
        }

        if when {
            format!("{}.", self.name)
        } else {
            String::new()
        }
    }

    fn add_interface_fragment(self) {
        self.gen
            .interface_fragments
            .entry(self.name.to_owned())
            .or_default()
            .push(InterfaceFragment {
                src: self.src,
                stub: self.stub,
            });
    }

    fn add_world_fragment(self) {
        self.gen.world_fragments.push(InterfaceFragment {
            src: self.src,
            stub: self.stub,
        });
    }

    fn import(&mut self, module: &str, func: &Function) {
        if func.kind != FunctionKind::Freestanding {
            todo!("resources");
        }

        let mut bindgen = FunctionBindgen::new(
            self,
            AbiVariant::GuestImport,
            &func.name,
            func.params
                .iter()
                .map(|(name, _)| name.to_kotlin_ident())
                .collect(),
        );

        abi::call(
            bindgen.gen.resolve,
            AbiVariant::GuestImport,
            LiftLower::LowerArgsLiftResults,
            func,
            &mut bindgen,
        );

        let src = bindgen.src;

        // TODO disabled for now since memory is "freed" once out of method call scope anyways
        // let cleanup_list = if bindgen.needs_cleanup_list {
        //     self.gen.needs_cleanup = true;
        //
        //     format!(
        //         "val cleanupList: ArrayList<{}Cleanup> = arrayListOf();\n",
        //         self.gen.qualifier()
        //     )
        // } else {
        //     String::new()
        // };
        let cleanup_list = String::new();

        let name = &func.name;

        let sig = self.resolve.wasm_signature(AbiVariant::GuestImport, func);

        let result_type = match &sig.results[..] {
            [] => "Unit",
            [result] => wasm_type(*result),
            _ => unreachable!(),
        };

        let camel_name = func.name.to_upper_camel_case();

        let params = sig
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let ty = wasm_type(*param);
                format!("p{i}: {ty}")
            })
            .collect::<Vec<_>>()
            .join(", ");

        let sig = self.sig_string(func, false);

        self.external_functions.push(format!(
            r#"@WasmImport(name = "{name}", module = "{module}")
               private external fun wasmImport{camel_name}({params}): {result_type}
            "#
        ));

        uwrite!(
            self.src,
            r#"{sig} {{
                   return withScopedMemoryAllocator {{ allocator ->
                       {cleanup_list} {src}
                   }}
               }}
            "#
        );
    }

    fn export(&mut self, interface_name: Option<&str>, func: &Function) {
        let sig = self.resolve.wasm_signature(AbiVariant::GuestExport, func);

        let export_name = func.core_export_name(interface_name);

        let mut bindgen = FunctionBindgen::new(
            self,
            AbiVariant::GuestExport,
            &func.name,
            (0..sig.params.len()).map(|i| format!("p{i}")).collect(),
        );

        abi::call(
            bindgen.gen.resolve,
            AbiVariant::GuestExport,
            LiftLower::LiftArgsLowerResults,
            func,
            &mut bindgen,
        );

        // TODO disabled for now since memory is "freed" once out of method call scope anyways
        // assert!(!bindgen.needs_cleanup_list);

        let src = bindgen.src;

        let result_type = match &sig.results[..] {
            [] => "Unit",
            [result] => wasm_type(*result),
            _ => unreachable!(),
        };

        let camel_name = func.name.to_upper_camel_case();

        let params = sig
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let ty = wasm_type(*param);
                format!("p{i}: {ty}")
            })
            .collect::<Vec<_>>()
            .join(", ");

        uwrite!(
            self.src,
            r#"

            @ExperimentalJsExport
            @JsName(name = "{export_name}")
            fun wasmExport{camel_name}({params}): {result_type} {{
                return withScopedMemoryAllocator {{ allocator ->
                    {src}
                }}
            }}
            "#
        );

        if abi::guest_export_needs_post_return(self.resolve, func) {
            let params = sig
                .results
                .iter()
                .enumerate()
                .map(|(i, param)| {
                    let ty = wasm_type(*param);
                    format!("p{i}: {ty}")
                })
                .collect::<Vec<_>>()
                .join(", ");

            let mut bindgen = FunctionBindgen::new(
                self,
                AbiVariant::GuestExport,
                "INVALID",
                (0..sig.results.len()).map(|i| format!("p{i}")).collect(),
            );

            abi::post_return(bindgen.gen.resolve, func, &mut bindgen);

            let src = bindgen.src;

            uwrite!(
                self.src,
                r#"
                @ExperimentalJsExport
                @JsName(name = "cabi_post_{export_name}")
                fun wasmExport{camel_name}PostReturn({params}): Unit {{
                    // this is currently a no_op since ScopedMemoryAllocator will overwrite
                    // the previous memory allocation when needed and not instantly
                    //  -> this is an implementation detail, which might change in the future
                    // and lead to the need of a means to explicitly free allocated memory
                    /* {src} */
                }}
                "#
            );
        }

        if self.gen.opts.generate_stub {
            let sig = self.sig_string(func, true);

            uwrite!(
                self.stub,
                r#"
                {sig} {{
                    TODO()
                }}
                "#
            );
        }
    }

    fn type_name(&mut self, ty: &Type) -> String {
        self.type_name_with_qualifier(ty, false)
    }

    fn type_name_with_qualifier(&mut self, ty: &Type, qualifier: bool) -> String {
        match ty {
            Type::Bool => "Boolean".into(),
            Type::U8 => "UByte".into(),
            Type::U16 => "UShort".into(),
            Type::U32 => "UInt".into(),
            Type::Char => "String".into(),
            Type::U64 => "ULong".into(),
            Type::S8 => "Byte".into(),
            Type::S16 => "Short".into(),
            Type::S32 => "Int".into(),
            Type::S64 => "Long".into(),
            Type::Float32 => "Float".into(),
            Type::Float64 => "Double".into(),
            Type::String => "String".into(),
            Type::Id(id) => {
                let ty = &self.resolve.types[*id];
                match &ty.kind {
                    TypeDefKind::Type(ty) => self.type_name_with_qualifier(ty, qualifier),
                    TypeDefKind::List(ty) => {
                        if is_primitive(ty) {
                            format!("{}Array", self.type_name(ty))
                        } else {
                            format!("ArrayList<{}>", self.type_name_boxed(ty, qualifier))
                        }
                    }
                    TypeDefKind::Tuple(tuple) => {
                        let count = tuple.types.len();
                        self.gen.tuple_counts.insert(count);

                        if count == 0 {
                            String::from("Unit")
                        } else {
                            let params = format!(
                                "<{}>",
                                tuple
                                    .types
                                    .iter()
                                    .map(|ty| self.type_name_boxed(ty, qualifier))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            );

                            format!("{}Tuple{count}{params}", self.gen.qualifier())
                        }
                    }
                    TypeDefKind::Option(ty) => format!("{}?", self.type_name_boxed(ty, qualifier)),
                    TypeDefKind::Result(result) => {
                        self.gen.needs_result = true;
                        let mut name = |ty: &Option<Type>| {
                            ty.as_ref()
                                .map(|ty| self.type_name_boxed(ty, qualifier))
                                .unwrap_or_else(|| {
                                    self.gen.tuple_counts.insert(0);

                                    String::from("Unit")
                                })
                        };
                        let ok = name(&result.ok);
                        let err = name(&result.err);

                        format!("{}Result<{ok}, {err}>", self.gen.qualifier())
                    }
                    _ => {
                        if let Some(name) = &ty.name {
                            format!(
                                "{}{}",
                                self.qualifier(qualifier, ty),
                                name.to_upper_camel_case()
                            )
                        } else {
                            unreachable!()
                        }
                    }
                }
            }
        }
    }

    fn type_name_boxed(&mut self, ty: &Type, qualifier: bool) -> String {
        match ty {
            Type::Bool => "Boolean".into(),
            Type::U8 => "UByte".into(),
            Type::U16 => "UShort".into(),
            Type::U32=> "UInt".into(),
            Type::Char => "String".into(),
            Type::U64 => "ULong".into(),
            Type::S8 => "Byte".into(),
            Type::S16 => "Short".into(),
            Type::S32 => "Int".into(),
            Type::S64 => "Long".into(),
            Type::Float32 => "Float".into(),
            Type::Float64 => "Double".into(),
            Type::Id(id) => {
                let def = &self.resolve.types[*id];
                match &def.kind {
                    TypeDefKind::Type(ty) => self.type_name_boxed(ty, qualifier),
                    _ => self.type_name_with_qualifier(ty, qualifier),
                }
            }
            _ => self.type_name_with_qualifier(ty, qualifier),
        }
    }

    fn print_docs(&mut self, docs: &Docs) {
        if let Some(docs) = &docs.contents {
            let lines = docs
                .trim()
                .lines()
                .map(|line| format!("* {line}"))
                .collect::<Vec<_>>()
                .join("\n");

            uwrite!(
                self.src,
                "
                /**
                 {lines}
                 */
                "
            )
        }
    }

    fn non_empty_type<'a>(&self, ty: Option<&'a Type>) -> Option<&'a Type> {
        if let Some(ty) = ty {
            let id = match ty {
                Type::Id(id) => *id,
                _ => return Some(ty),
            };
            match &self.resolve.types[id].kind {
                TypeDefKind::Type(t) => self.non_empty_type(Some(t)).map(|_| ty),
                TypeDefKind::Record(r) => (!r.fields.is_empty()).then_some(ty),
                TypeDefKind::Tuple(t) => (!t.types.is_empty()).then_some(ty),
                _ => Some(ty),
            }
        } else {
            None
        }
    }

    fn sig_string(&mut self, func: &Function, qualifier: bool) -> String {
        let name = func.name.to_kotlin_ident();

        let result_type = match func.results.len() {
            0 => "Unit".into(),
            1 => {
                self.type_name_with_qualifier(func.results.iter_types().next().unwrap(), qualifier)
            }
            count => {
                self.gen.tuple_counts.insert(count);
                format!(
                    "{}Tuple{count}<{}>",
                    self.gen.qualifier(),
                    func.results
                        .iter_types()
                        .map(|ty| self.type_name_boxed(ty, qualifier))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        };

        let params = func
            .params
            .iter()
            .map(|(name, ty)| {
                let ty = self.type_name_with_qualifier(ty, qualifier);
                let name = name.to_kotlin_ident();
                format!("{name}: {ty}")
            })
            .collect::<Vec<_>>()
            .join(", ");

        format!("fun {name}({params}): {result_type}")
    }
}

impl<'a> wit_bindgen_core::InterfaceGenerator<'a> for InterfaceGenerator<'a> {
    fn resolve(&self) -> &'a Resolve {
        self.resolve
    }

    fn type_record(&mut self, _id: TypeId, name: &str, record: &Record, docs: &Docs) {
        self.print_docs(docs);

        let name = name.to_upper_camel_case();

        let parameters = record
            .fields
            .iter()
            .map(|field| {
                format!(
                    "val {}: {}",
                    field.name.to_kotlin_ident(),
                    self.type_name(&field.ty)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");

        uwrite!(
            self.src,
            "
            data class {name}({parameters})
            "
        );
    }

    fn type_resource(&mut self, id: TypeId, name: &str, docs: &Docs) {
        _ = (id, name, docs);
        todo!()
    }

    fn type_flags(&mut self, _id: TypeId, name: &str, flags: &Flags, docs: &Docs) {
        self.print_docs(docs);

        let name = name.to_upper_camel_case();

        let ty = match flags.repr() {
            FlagsRepr::U8 => "UByte",
            FlagsRepr::U16 => "UShort",
            FlagsRepr::U32(1) => "UInt",
            FlagsRepr::U32(2) => "ULong",
            repr => todo!("flags {repr:?}"),
        };

        let flags = flags
            .flags
            .iter()
            .enumerate()
            .map(|(i, flag)| {
                let flag_name = flag.name.to_shouty_snake_case();
                let suffix = if matches!(flags.repr(), FlagsRepr::U32(2)) {
                    "uL"
                } else {
                    "u"
                };
                format!(
                    "val {flag_name}: {name} = {name}((1{suffix} shl {i}).to{ty}());"
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        uwrite!(
            self.src,
            "
            data class {name}(val value: {ty}) {{
                companion object {{
                    {flags}
                }}
            }}
            "
        );
    }

    fn type_tuple(&mut self, id: TypeId, _name: &str, _tuple: &Tuple, _docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_variant(&mut self, _id: TypeId, name: &str, variant: &Variant, docs: &Docs) {
        self.print_docs(docs);

        let name = name.to_upper_camel_case();

        let cases = variant
            .cases
            .iter()
            .map(|case| {
                let case_class_name = case.name.to_upper_camel_case();
                if let Some(ty) = self.non_empty_type(case.ty.as_ref())
                {
                    format!("class {case_class_name}(val value: {}) : {name}", self.type_name(ty))
                } else {
                    format!("object {case_class_name} : {name}")
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        uwrite!(
            self.src,
            "
            sealed interface {name} {{
                {cases}
            }}
            "
        );
    }

    fn type_option(&mut self, id: TypeId, _name: &str, _payload: &Type, _docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_result(&mut self, id: TypeId, _name: &str, _result: &Result_, _docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_enum(&mut self, _id: TypeId, name: &str, enum_: &Enum, docs: &Docs) {
        self.print_docs(docs);

        let name = name.to_upper_camel_case();

        let cases = enum_
            .cases
            .iter()
            .map(|case| case.name.to_shouty_snake_case())
            .collect::<Vec<_>>()
            .join(", ");

        uwrite!(
            self.src,
            "
            enum class {name} {{
                {cases}
            }}
            "
        );
    }

    fn type_alias(&mut self, id: TypeId, _name: &str, _ty: &Type, _docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_list(&mut self, id: TypeId, _name: &str, _ty: &Type, _docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_builtin(&mut self, _id: TypeId, _name: &str, _ty: &Type, _docs: &Docs) {
        unimplemented!();
    }
}

struct Block {
    body: String,
    results: Vec<String>,
    element: String,
    base: String,
}

// TODO suppress warning to be removed once memory can be freed explicitly
#[allow(dead_code)]
struct Cleanup {
    address: String,
    size: String,
    align: usize,
}

struct BlockStorage {
    body: String,
    element: String,
    base: String,
    cleanup: Vec<Cleanup>,
}

struct FunctionBindgen<'a, 'b> {
    gen: &'b mut InterfaceGenerator<'a>,
    abi_variant: AbiVariant,
    func_name: &'b str,
    params: Box<[String]>,
    src: String,
    locals: Ns,
    block_storage: Vec<BlockStorage>,
    blocks: Vec<Block>,
    payloads: Vec<String>,
    cleanup: Vec<Cleanup>,
    // TODO suppress warning to be removed once memory can be freed explicitly
    #[allow(dead_code)]
    needs_cleanup_list: bool,
}

impl<'a, 'b> FunctionBindgen<'a, 'b> {
    fn new(
        gen: &'b mut InterfaceGenerator<'a>,
        abi_variant: AbiVariant,
        func_name: &'b str,
        params: Box<[String]>,
    ) -> FunctionBindgen<'a, 'b> {
        Self {
            gen,
            abi_variant,
            func_name,
            params,
            src: String::new(),
            locals: Ns::default(),
            block_storage: Vec::new(),
            blocks: Vec::new(),
            payloads: Vec::new(),
            cleanup: Vec::new(),
            needs_cleanup_list: false,
        }
    }

    fn lower_variant(
        &mut self,
        name: &str,
        cases: &[(&str, Option<Type>)],
        lowered_types: &[WasmType],
        op: &str,
        results: &mut Vec<String>,
    ) {
        let case_class_prefix = format!("{}.", name.to_upper_camel_case());

        let blocks = self
            .blocks
            .drain(self.blocks.len() - cases.len()..)
            .collect::<Vec<_>>();

        let payloads = self
            .payloads
            .drain(self.payloads.len() - cases.len()..)
            .collect::<Vec<_>>();

        let lowered = lowered_types
            .iter()
            .map(|_| self.locals.tmp("lowered"))
            .collect::<Vec<_>>();

        results.extend(lowered.iter().cloned());

        let declarations = lowered
            .iter()
            .zip(lowered_types)
            .map(|(lowered, ty)| format!("val {lowered}: {}", wasm_type(*ty)))
            .collect::<Vec<_>>()
            .join("\n");

        let cases = cases
            .iter()
            .zip(blocks)
            .zip(payloads)
            .map(
                |(((name, ty), Block { body, results, .. }), payload)| {
                    let case_class_name = name.to_upper_camel_case();

                    let payload = if let Some(_) = self.gen.non_empty_type(ty.as_ref()) {
                        format!("val {payload} = ({op}).value")
                    } else {
                        String::new()
                    };

                    let assignments = lowered
                        .iter()
                        .zip(&results)
                        .map(|(lowered, result)| format!("{lowered} = {result}\n"))
                        .collect::<Vec<_>>()
                        .concat();

                    format!(
                        "is {case_class_prefix}{case_class_name} -> {{
                             {payload}
                             {body}
                             {assignments}
                         }}"
                    )
                },
            )
            .collect::<Vec<_>>()
            .join("\n");

        uwrite!(
            self.src,
            r#"
            {declarations}

            when ({op}) {{
                {cases}
            }}
            "#
        );
    }

    fn lift_variant(
        &mut self,
        ty: &Type,
        cases: &[(&str, Option<Type>)],
        op: &str,
        results: &mut Vec<String>,
    ) {
        let blocks = self
            .blocks
            .drain(self.blocks.len() - cases.len()..)
            .collect::<Vec<_>>();

        let ty = self.gen.type_name(ty);
        let generics_position = ty.find('<');
        let lifted = self.locals.tmp("lifted");

        let cases = cases
            .iter()
            .zip(blocks)
            .enumerate()
            .map(|(i, ((case_name, case_ty), Block { body, results, .. }))| {
                let payload = if self.gen.non_empty_type(case_ty.as_ref()).is_some() {
                    format!("({})", results.into_iter().next().unwrap())
                } else if generics_position.is_some() {
                    if let Some(ty) = case_ty.as_ref() {
                        format!("({}.INSTANCE)", self.gen.type_name(ty))
                    } else {
                        String::from("(Unit)")
                    }
                } else {
                    String::new()
                };

                let method = case_name.to_upper_camel_case();

                let call = if let Some(position) = generics_position {
                    let (ty, generics) = ty.split_at(position);
                    format!("{ty}.{method}{generics}")
                } else {
                    format!("{ty}.{method}")
                };

                format!(
                    "{i} -> {{
                         {body}
                         {call}{payload}
                     }}"
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        uwrite!(
            self.src,
            r#"
            val {lifted}: {ty} = when ({op}.toInt()) {{
                {cases}

                else -> throw AssertionError("invalid discriminant: ${{{op}}}");
            }}
            "#
        );

        results.push(lifted);
    }
}

impl Bindgen for FunctionBindgen<'_, '_> {
    type Operand = String;

    fn emit(
        &mut self,
        _resolve: &Resolve,
        inst: &Instruction<'_>,
        operands: &mut Vec<String>,
        results: &mut Vec<String>,
    ) {
        match inst {
            Instruction::GetArg { nth } => results.push(self.params[*nth].clone()),
            Instruction::I32Const { val } => results.push(val.to_string()),
            Instruction::ConstZero { tys } => results.extend(tys.iter().map(|ty| {
                match ty {
                    WasmType::I32 => "0",
                    WasmType::I64 => "0L",
                    WasmType::F32 => "0.0F",
                    WasmType::F64 => "0.0",
                }
                .to_owned()
            })),

            // TODO: checked
            Instruction::U8FromI32 => results.push(format!("({}).toUByte()", operands[0])),
            Instruction::S8FromI32 => results.push(format!("({}).toByte()", operands[0])),
            Instruction::U16FromI32 => results.push(format!("({}).toUShort()", operands[0])),
            Instruction::S16FromI32 => results.push(format!("({}).toShort()", operands[0])),

            Instruction::I32FromU8 => results.push(format!("(({}).toInt()) and 0xFF", operands[0])),
            Instruction::I32FromU16 => results.push(format!("(({}).toInt()) and 0xFFFF", operands[0])),

            Instruction::I32FromS8 | Instruction::I32FromS16 => {
                results.push(format!("({}).toInt()", operands[0]))
            }

            Instruction::U32FromI32 => results.push(format!("({}).toUInt()", operands[0])),
            Instruction::I32FromU32 => results.push(format!("({}).toInt()", operands[0])),
            Instruction::U64FromI64 => results.push(format!("({}).toULong()", operands[0])),
            Instruction::I64FromU64 => results.push(format!("({}).toLong()", operands[0])),

            Instruction::CharFromI32 => results.push(format!("Char.toChars({}).concatToString()", operands[0])),
            Instruction::I32FromChar => results.push(format!("({}).let {{ char -> if (char.hasSurrogatePairAt(0)) {{ Char.toCodePoint(char[0], char[1]) }} else {{ char[0].code }} }}", operands[0])),

            Instruction::S32FromI32
            | Instruction::S64FromI64
            | Instruction::I32FromS32
            | Instruction::I64FromS64
            | Instruction::F32FromFloat32
            | Instruction::F64FromFloat64
            | Instruction::Float32FromF32
            | Instruction::Float64FromF64 => results.push(operands[0].clone()),

            Instruction::Bitcasts { casts } => {
                results.extend(casts.iter().zip(operands).map(|(cast, op)| match cast {
                    Bitcast::I32ToF32 => format!("Float.fromBits({op})"),
                    Bitcast::I64ToF32 => format!("Float.fromBits(({op}).toInt())"),
                    Bitcast::F32ToI32 => format!("{op}.toRawBits()"),
                    Bitcast::F32ToI64 => format!("{op}.toRawBits().toLong()"),
                    Bitcast::I64ToF64 => format!("Double.fromBits({op})"),
                    Bitcast::F64ToI64 => format!("{op}.toRawBits()"),
                    Bitcast::I32ToI64 => format!("{op}.toLong()"),
                    Bitcast::I64ToI32 => format!("{op}.toInt()"),
                    Bitcast::None => op.to_owned(),
                }))
            }

            Instruction::I32FromBool => {
                results.push(format!("(if ({}) 1 else 0)", operands[0]));
            }
            Instruction::BoolFromI32 => results.push(format!("({}.toInt() != 0)", operands[0])),

            // TODO: checked
            Instruction::FlagsLower { flags, .. } => match flags_repr(flags) {
                Int::U8 | Int::U16 | Int::U32 => {
                    results.push(format!("({}).value", operands[0]));
                }
                Int::U64 => {
                    let op = &operands[0];
                    results.push(format!("({op}).value and 0xffffffffuL"));
                    results.push(format!("(({op}).value shr 32) and 0xffffffffuL"));
                }
            },

            Instruction::FlagsLift { flags, ty, .. } => match flags_repr(flags) {
                Int::U8 | Int::U16 | Int::U32 => {
                    results.push(format!(
                        "{}({}.to{}())",
                        self.gen.type_name(&Type::Id(*ty)),
                        operands[0],
                        int_type(flags_repr(flags))
                    ));
                }
                Int::U64 => {
                    results.push(format!(
                        "{}({}.toULong() or ({}.toULong() shl 32))",
                        self.gen.type_name(&Type::Id(*ty)),
                        operands[0],
                        operands[1]
                    ));
                }
            },

            Instruction::HandleLower { .. } | Instruction::HandleLift { .. } => todo!(),

            Instruction::RecordLower { record, .. } => {
                let op = &operands[0];
                for field in record.fields.iter() {
                    results.push(format!("({op}).{}", field.name.to_kotlin_ident()));
                }
            }
            Instruction::RecordLift { ty, .. } | Instruction::TupleLift { ty, .. } => {
                let ops = operands
                    .iter()
                    .map(|op| op.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                let type_name = self.gen.type_name(&Type::Id(*ty));
                if type_name == "Unit" {
                    results.push(type_name);
                } else {
                    results.push(format!("{}({ops})", type_name));
                }
            }

            Instruction::TupleLower { tuple, .. } => {
                let op = &operands[0];
                for i in 0..tuple.types.len() {
                    results.push(format!("({op}).f{i}"));
                }
            }

            Instruction::VariantPayloadName => {
                let payload = self.locals.tmp("payload");
                results.push(payload.clone());
                self.payloads.push(payload);
            }

            Instruction::VariantLower {
                variant,
                name,
                results: lowered_types,
                ..
            } => self.lower_variant(
                name,
                &variant
                    .cases
                    .iter()
                    .map(|case| (case.name.deref(), case.ty))
                    .collect::<Vec<_>>(),
                lowered_types,
                &operands[0],
                results,
            ),

            Instruction::VariantLift { variant, ty, .. } => self.lift_variant(
                &Type::Id(*ty),
                &variant
                    .cases
                    .iter()
                    .map(|case| (case.name.deref(), case.ty))
                    .collect::<Vec<_>>(),
                &operands[0],
                results,
            ),

            Instruction::OptionLower {
                results: lowered_types,
                payload,
                ..
            } => {
                let some = self.blocks.pop().unwrap();
                let none = self.blocks.pop().unwrap();
                let some_payload = self.payloads.pop().unwrap();
                let none_payload = self.payloads.pop().unwrap();

                let lowered = lowered_types
                    .iter()
                    .map(|_| self.locals.tmp("lowered"))
                    .collect::<Vec<_>>();

                results.extend(lowered.iter().cloned());

                let declarations = lowered
                    .iter()
                    .zip(lowered_types.iter())
                    .map(|(lowered, ty)| format!("var {lowered}: {}? = null", wasm_type(*ty)))
                    .collect::<Vec<_>>()
                    .join("\n");

                let op = &operands[0];

                let mut block = |ty: Option<&Type>, Block { body, results, .. }, payload| {
                    let payload = if let Some(ty) = self.gen.non_empty_type(ty) {
                        let ty = self.gen.type_name(ty);

                        format!("val {payload}: {ty} = ({op}) as {ty}")
                    } else {
                        String::new()
                    };

                    let assignments = lowered
                        .iter()
                        .zip(&results)
                        .map(|(lowered, result)| format!("{lowered} = {result}\n"))
                        .collect::<Vec<_>>()
                        .concat();

                    format!(
                        "{payload}
                         {body}
                         {assignments}"
                    )
                };

                let none = block(None, none, none_payload);
                let some = block(Some(payload), some, some_payload);

                uwrite!(
                    self.src,
                    r#"
                    {declarations}

                    if (({op}) == null) {{
                        {none}
                    }} else {{
                        {some}
                    }}
                    "#
                );
            }

            Instruction::OptionLift { payload, ty } => {
                let some = self.blocks.pop().unwrap();
                let _none = self.blocks.pop().unwrap();

                let ty = self.gen.type_name(&Type::Id(*ty));
                let lifted = self.locals.tmp("lifted");
                let op = &operands[0];

                let payload = if self.gen.non_empty_type(Some(*payload)).is_some() {
                    some.results.into_iter().next().unwrap()
                } else {
                    "null".into()
                };

                let some = some.body;

                uwrite!(
                    self.src,
                    r#"
                    val {lifted}: {ty} = when({op}.toInt()) {{
                        0 -> {{
                            null
                        }}

                        1 -> {{
                            {some}
                            {payload}
                        }}

                        else -> throw AssertionError("invalid discriminant: ${{{op}}}")
                    }}
                    "#
                );

                results.push(lifted);
            }

            Instruction::ResultLower {
                results: lowered_types,
                result,
                ..
            } => self.lower_variant(
                format!("{}Result", self.gen.gen.qualifier()).as_str(),
                &[("ok", result.ok), ("err", result.err)],
                lowered_types,
                &operands[0],
                results,
            ),

            Instruction::ResultLift { result, ty } => self.lift_variant(
                &Type::Id(*ty),
                &[("ok", result.ok), ("err", result.err)],
                &operands[0],
                results,
            ),

            Instruction::EnumLower { .. } => results.push(format!("{}.ordinal", operands[0])),

            Instruction::EnumLift { ty, .. } => results.push(format!(
                "{}.entries[{}.toInt()]",
                self.gen.type_name(&Type::Id(*ty)),
                operands[0]
            )),

            Instruction::ListCanonLower { element, realloc: _ } => {
                let op = &operands[0];
                let (size, _) = list_element_info(element);
                let (raw_type, conversion) = store_and_convert(element);

                let result_pointer = self.locals.tmp("resultPointer");
                let current_pointer = self.locals.tmp("currentPointer");

                uwrite!(
                    self.src,
                    "
                    val {result_pointer} = allocator.allocate({size} * ({op}).size)
                    var {current_pointer} = Pointer({result_pointer}.address)
                    for (el in {op}) {{
                        {current_pointer}.store{raw_type}(el{conversion})
                        {current_pointer} += {size}
                    }}
                    "
                );

                // TODO disabled for now since memory is "freed" once out of method call scope anyways
                // if realloc.is_none() {
                //     self.cleanup.push(Cleanup {
                //         address: format!("{address}.toInt()"),
                //         size: format!("{size} * ({op}).size"),
                //         align: size,
                //     });
                // }

                results.push(format!("{result_pointer}.address"));
                results.push(format!("({op}).size"));
            }

            Instruction::ListCanonLift { element, .. } => {
                let (size, ty) = list_element_info(element);
                let (raw_type, conversion) = load_and_convert(element);
                let array = self.locals.tmp("array");
                let pointer = self.locals.tmp("pointer");
                let address = &operands[0];
                let length = &operands[1];

                uwrite!(
                    self.src,
                    "
                    val {array} = {ty}Array({length})
                    var {pointer} = Pointer({address}.toUInt())
                    for (index in 0..{length}) {{
                        {array}[index] = {pointer}.load{raw_type}(){conversion}
                        {pointer} += {size}
                    }}
                    "
                );

                results.push(array);
            }

            Instruction::StringLower { realloc: _ } => {
                let op = &operands[0];
                let bytes = self.locals.tmp("bytes");
                uwriteln!(
                    self.src,
                    "val {bytes} = ({op}).encodeToByteArray()"
                );

                let result_pointer = self.locals.tmp("resultPointer");
                let current_pointer = self.locals.tmp("currentPointer");

                uwrite!(
                    self.src,
                    "
                    val {result_pointer} = allocator.allocate(({bytes}).size)
                    var {current_pointer} = Pointer({result_pointer}.address)
                    for (el in {bytes}) {{
                        {current_pointer}.storeByte(el)
                        {current_pointer} += 1
                    }}
                    "
                );

                results.push(format!("{result_pointer}.address"));
                results.push(format!("{bytes}.size"));
            }

            Instruction::StringLift { .. } => {
                let bytes = self.locals.tmp("bytes");
                let pointer = self.locals.tmp("pointer");
                let address = &operands[0];
                let length = &operands[1];

                uwrite!(
                    self.src,
                    "
                    val {bytes} = ByteArray({length})
                    var {pointer} = Pointer({address}.toUInt())
                    for (index in 0..{length}) {{
                        {bytes}[index] = {pointer}.loadByte()
                        {pointer} += 1
                    }}
                    "
                );

                results.push(format!("{bytes}.decodeToString()"));
            }

            Instruction::ListLower { element, realloc } => {
                let Block {
                    body,
                    results: block_results,
                    element: block_element,
                    base,
                } = self.blocks.pop().unwrap();
                assert!(block_results.is_empty());

                let op = &operands[0];
                let size = self.gen.gen.sizes.size(element);
                let align = self.gen.gen.sizes.align(element);
                let result_pointer = self.locals.tmp("resultPointer");
                let index = self.locals.tmp("index");

                uwrite!(
                    self.src,
                    "
                    val {result_pointer} = allocator.allocate(({op}).size * {size})
                    for ({index} in 0 until ({op}).size) {{
                        val {block_element} = ({op})[{index}]
                        val {base} = {result_pointer}.address + ({index}.toUInt() * {size}u)
                        {body}
                    }}
                    "
                );

                if realloc.is_none() {
                    self.cleanup.push(Cleanup {
                        address: result_pointer.clone(),
                        size: format!("({op}).size * {size}"),
                        align,
                    });
                }

                results.push(format!("{result_pointer}.address"));
                results.push(format!("({op}).size"));
            }

            Instruction::ListLift { element, .. } => {
                let Block {
                    body,
                    results: block_results,
                    base,
                    ..
                } = self.blocks.pop().unwrap();
                let address = &operands[0];
                let length = &operands[1];
                let array = self.locals.tmp("array");
                let ty = self.gen.type_name(element);
                let size = self.gen.gen.sizes.size(element);
                // let align = self.gen.gen.sizes.align(element);
                let index = self.locals.tmp("index");

                let result = match &block_results[..] {
                    [result] => result,
                    _ => todo!("result count == {}", results.len()),
                };

                uwrite!(
                    self.src,
                    "
                    val {array} = ArrayList<{ty}>({length})
                    for ({index} in 0u until ({length}).toUInt()) {{
                        val {base} = {address}.toUInt() + ({index}.toUInt() * {size}u)
                        {body}
                        {array}.add({result})
                    }}
                    "
                    // TODO might need to free memory once possible
                    // Memory.free(Address.fromInt({address}), ({length}) * {size}, {align})
                );

                results.push(array);
            }

            Instruction::IterElem { .. } => {
                results.push(self.block_storage.last().unwrap().element.clone())
            }

            Instruction::IterBasePointer => {
                results.push(self.block_storage.last().unwrap().base.clone())
            }

            Instruction::CallWasm { sig, .. } => {
                let assignment = match &sig.results[..] {
                    [result] => {
                        let ty = wasm_type(*result);
                        let result = self.locals.tmp("result");
                        let assignment = format!("val {result}: {ty} = ");
                        results.push(result);
                        assignment
                    }

                    [] => String::new(),

                    _ => unreachable!(),
                };

                let func_name = self.func_name.to_upper_camel_case();

                let operands = operands
                    .iter()
                    .zip(&sig.params)
                    .map(|(op, ty)| {
                        let conversion = format!(".to{}()", wasm_type(*ty));
                        format!("({op}){conversion}")
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                uwriteln!(self.src, "{assignment} wasmImport{func_name}({operands})");
            }

            Instruction::CallInterface { func, .. } => {
                let (assignment, destructure) = match func.results.len() {
                    0 => (String::new(), String::new()),
                    1 => {
                        let ty = self
                            .gen
                            .type_name(func.results.iter_types().next().unwrap());
                        let result = self.locals.tmp("result");
                        let assignment = format!("val {result}: {ty} = ");
                        results.push(result);
                        (assignment, String::new())
                    }
                    count => {
                        self.gen.gen.tuple_counts.insert(count);
                        let ty = format!(
                            "{}Tuple{count}<{}>",
                            self.gen.gen.qualifier(),
                            func.results
                                .iter_types()
                                .map(|ty| self.gen.type_name_boxed(ty, false))
                                .collect::<Vec<_>>()
                                .join(", ")
                        );

                        let result = self.locals.tmp("result");
                        let assignment = format!("val {result}: {ty} = ");

                        let destructure = func
                            .results
                            .iter_types()
                            .enumerate()
                            .map(|(index, ty)| {
                                let ty = self.gen.type_name(ty);
                                let my_result = self.locals.tmp("result");
                                let assignment = format!("val {my_result}: {ty} = {result}.f{index}");
                                results.push(my_result);
                                assignment
                            })
                            .collect::<Vec<_>>()
                            .join("\n");

                        (assignment, destructure)
                    }
                };

                // let module = self.gen.name.to_upper_camel_case();
                let module = self.gen.name;
                let name = func.name.to_kotlin_ident();

                let args = operands.join(", ");

                uwrite!(
                    self.src,
                    "
                    {assignment}{module}Impl.{name}({args});
                    {destructure}
                    "
                );
            }

            Instruction::Return { amt, func } => {
                // TODO disabled for now since memory is "freed" once out of method call scope anyways
                // for Cleanup {
                //     address,
                //     size,
                //     align,
                // } in &self.cleanup
                // {
                //     uwriteln!(
                //         self.src,
                //         "Memory.free(Address.fromInt({address}), {size}, {align});"
                //     );
                // }
                //
                // if self.needs_cleanup_list {
                //     uwrite!(
                //         self.src,
                //         "
                //         for ({}Cleanup cleanup : cleanupList) {{
                //             Memory.free(Address.fromInt(cleanup.address), cleanup.size, cleanup.align);
                //         }}
                //         ",
                //         self.gen.gen.qualifier()
                //     );
                // }

                match *amt {
                    0 => (),
                    1 => {
                        let return_value = &operands[0];

                        if self.abi_variant == AbiVariant::GuestImport {
                            uwriteln!(self.src, "{}", return_value)
                        } else {
                            let sig = self.gen.resolve.wasm_signature(AbiVariant::GuestExport, *func);

                            match &sig.results[..] {
                                [] => uwriteln!(self.src, "{}", return_value),
                                [result] => uwriteln!(self.src, "{}.to{}()", return_value, wasm_type(*result)),
                                _ => unreachable!(),
                            }
                        }
                    },
                    count => {
                        let results = operands.join(", ");
                        uwriteln!(
                            self.src,
                            "{}Tuple{count}({results})",
                            self.gen.gen.qualifier()
                        )
                    }
                }
            }

            Instruction::I32Load { offset } => results.push(format!(
                "Pointer(({}) + {offset}u).loadInt()",
                operands[0]
            )),

            Instruction::I32Load8U { offset } => results.push(format!(
                "Pointer(({}) + {offset}u).loadByte().toUByte()",
                operands[0]
            )),

            Instruction::I32Load8S { offset } => results.push(format!(
                "Pointer(({}) + {offset}u).loadByte()",
                operands[0]
            )),

            Instruction::I32Load16U { offset } => results.push(format!(
                "Pointer(({}) + {offset}u).loadShort().toUShort()",
                operands[0]
            )),

            Instruction::I32Load16S { offset } => results.push(format!(
                "Pointer(({}) + {offset}u).loadShort()",
                operands[0]
            )),

            Instruction::I64Load { offset } => results.push(format!(
                "Pointer(({}) + {offset}u).loadLong()",
                operands[0]
            )),

            Instruction::F32Load { offset } => results.push(format!(
                "Float.fromBits(Pointer(({}) + {offset}u).loadInt())",
                operands[0]
            )),

            Instruction::F64Load { offset } => results.push(format!(
                "Double.fromBits(Pointer(({}) + {offset}u).loadLong())",
                operands[0]
            )),

            Instruction::I32Store { offset } => uwriteln!(
                self.src,
                "Pointer(({}) + {offset}u).storeInt(({}).toInt())",
                operands[1],
                operands[0]
            ),

            Instruction::I32Store8 { offset } => uwriteln!(
                self.src,
                "Pointer(({}) + {offset}u).storeByte(({}).toByte())",
                operands[1],
                operands[0]
            ),

            Instruction::I32Store16 { offset } => uwriteln!(
                self.src,
                "Pointer(({}) + {offset}u).storeShort(({}).toShort())",
                operands[1],
                operands[0]
            ),

            Instruction::I64Store { offset } => uwriteln!(
                self.src,
                "Pointer(({}) + {offset}u).storeLong(({}).toLong())",
                operands[1],
                operands[0]
            ),

            Instruction::F32Store { offset } => uwriteln!(
                self.src,
                "Pointer(({}) + {offset}u).storeInt({}.toRawBits())",
                operands[1],
                operands[0]
            ),

            Instruction::F64Store { offset } => uwriteln!(
                self.src,
                "Pointer(({}) + {offset}u).storeLong({}.toRawBits())",
                operands[1],
                operands[0]
            ),

            Instruction::Malloc { .. } => unimplemented!(),

            Instruction::GuestDeallocate { size: _, align: _ } => {
                // TODO disabled for now since memory is "freed" once out of method call scope anyways
                // uwriteln!(
                //     self.src,
                //     "Memory.free(Address.fromInt({}), {size}, {align});",
                //     operands[0]
                // )
            }

            Instruction::GuestDeallocateString => {
                // TODO disabled for now since memory is "freed" once out of method call scope anyways
                // uwriteln!(
                //     self.src,
                //     "Memory.free(Address.fromInt({}), {}, 1);",
                //     operands[0],
                //     operands[1]
                // )
            },

            Instruction::GuestDeallocateVariant { blocks } => {
                let cases = self
                    .blocks
                    .drain(self.blocks.len() - blocks..)
                    .enumerate()
                    .map(|(i, Block { body, results, .. })| {
                        assert!(results.is_empty());

                        format!(
                            "{i} -> {{
                                 {body}
                             }}"
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n");

                let op = &operands[0];

                uwrite!(
                    self.src,
                    "
                    when({op}) {{
                        {cases}
                    }}
                    "
                );
            }

            Instruction::GuestDeallocateList { element } => {
                let Block {
                    body,
                    results,
                    base,
                    ..
                } = self.blocks.pop().unwrap();
                assert!(results.is_empty());

                let address = &operands[0];
                let length = &operands[1];

                let size = self.gen.gen.sizes.size(element);
                let _align = self.gen.gen.sizes.align(element);

                if !body.trim().is_empty() {
                    let index = self.locals.tmp("index");

                    uwrite!(
                        self.src,
                        "
                        for (int {index} = 0; {index} < ({length}); ++{index}) {{
                            int {base} = ({address}) + ({index}.toUInt() * {size}u)
                            {body}
                        }}
                        "
                    );
                }

                // TODO disabled for now since memory is "freed" once out of method call scope anyways
                // uwriteln!(
                //     self.src,
                //     "Memory.free(Address.fromInt({address}), ({length}) * {size}, {align});"
                // );
            }
        }
    }

    fn return_pointer(&mut self, size: usize, _align: usize) -> String {
        // TODO at the time of writing, Kotlin only supports 8-byte alignments

        let return_area = self.locals.tmp("returnArea");

        uwriteln!(
            self.src,
            "
                val {return_area} = allocator.allocate({size})\n
            "
        );
        format!("{}.address", return_area)
        // self.gen.gen.return_area_size = self.gen.gen.return_area_size.max(size);
        // self.gen.gen.return_area_align = self.gen.gen.return_area_align.max(align);
        // format!("{}RETURN_AREA", self.gen.gen.qualifier())
    }

    fn push_block(&mut self) {
        self.block_storage.push(BlockStorage {
            body: mem::take(&mut self.src),
            element: self.locals.tmp("element"),
            base: self.locals.tmp("base"),
            cleanup: mem::take(&mut self.cleanup),
        });
    }

    fn finish_block(&mut self, operands: &mut Vec<String>) {
        let BlockStorage {
            body,
            element,
            base,
            cleanup,
        } = self.block_storage.pop().unwrap();

        // TODO disabled for now since memory is "freed" once out of method call scope anyways
        // if !self.cleanup.is_empty() {
        //     self.needs_cleanup_list = true;
        //
        //     for Cleanup {
        //         address,
        //         size,
        //         align,
        //     } in &self.cleanup
        //     {
        //         uwriteln!(
        //             self.src,
        //             "cleanupList.add(new {}Cleanup({address}, {size}, {align}));",
        //             self.gen.gen.qualifier()
        //         );
        //     }
        // }

        self.cleanup = cleanup;

        self.blocks.push(Block {
            body: mem::replace(&mut self.src, body),
            results: mem::take(operands),
            element,
            base,
        });
    }

    fn sizes(&self) -> &SizeAlign {
        &self.gen.gen.sizes
    }

    fn is_list_canonical(&self, _resolve: &Resolve, element: &Type) -> bool {
        is_primitive(element)
    }
}

fn int_type(int: Int) -> &'static str {
    match int {
        Int::U8 => "UByte",
        Int::U16 => "UShort",
        Int::U32 => "UInt",
        Int::U64 => "ULong",
    }
}

fn wasm_type(ty: WasmType) -> &'static str {
    match ty {
        WasmType::I32 => "Int",
        WasmType::I64 => "Long",
        WasmType::F32 => "Float",
        WasmType::F64 => "Double",
    }
}

fn flags_repr(flags: &Flags) -> Int {
    match flags.repr() {
        FlagsRepr::U8 => Int::U8,
        FlagsRepr::U16 => Int::U16,
        FlagsRepr::U32(1) => Int::U32,
        FlagsRepr::U32(2) => Int::U64,
        repr => panic!("unimplemented flags {repr:?}"),
    }
}

fn store_and_convert(ty: &Type) -> (&'static str, &'static str) {
    match ty {
        Type::U8 => ("Byte", ".toByte()"),
        Type::U16 => ("Short", ".toShort()"),
        Type::U32 => ("Int", ".toInt()"),
        Type::U64 => ("Long", ".toLong()"),
        Type::S8 => ("Byte", ""),
        Type::S16 => ("Short", ""),
        Type::S32 => ("Int", ""),
        Type::S64 => ("Long", ""),
        Type::Float32 => ("Int", ".toRawBits()"),
        Type::Float64 => ("Long", ".toRawBits()"),
        _ => unreachable!(),
    }
}

fn load_and_convert(ty: &Type) -> (&'static str, &'static str) {
    match ty {
        Type::U8 => ("Byte", ".toUByte()"),
        Type::U16 => ("Short", ".toUShort()"),
        Type::U32 => ("Int", ".toUInt()"),
        Type::U64 => ("Long", ".toULong()"),
        Type::S8 => ("Byte", ""),
        Type::S16 => ("Short", ""),
        Type::S32 => ("Int", ""),
        Type::S64 => ("Long", ""),
        Type::Float32 => ("Int", ".let(Float::fromBits)"),
        Type::Float64 => ("Long", ".let(Double::fromBits)"),
        _ => unreachable!(),
    }
}

fn list_element_info(ty: &Type) -> (usize, &'static str) {
    match ty {
        Type::U8 => (1, "UByte"),
        Type::U16 => (2, "UShort"),
        Type::U32 => (4, "UInt"),
        Type::U64 => (8, "ULong"),
        Type::S8 => (1, "Byte"),
        Type::S16 => (2, "Short"),
        Type::S32 => (4, "Int"),
        Type::S64 => (8, "Long"),
        Type::Float32 => (4, "Float"),
        Type::Float64 => (8, "Double"),
        _ => unreachable!(),
    }
}

fn indent(code: &str) -> String {
    let mut indented = String::with_capacity(code.len());
    let mut indent = 0;
    let mut was_empty = false;
    for line in code.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            if was_empty {
                continue;
            }
            was_empty = true;
        } else {
            was_empty = false;
        }

        if trimmed.starts_with('}') && indent > 0 {
            indent -= 1;
        }
        indented.extend(iter::repeat(' ').take(indent * 4));
        indented.push_str(trimmed);
        if trimmed.ends_with('{') || trimmed.ends_with("->") {
            indent += 1;
        }
        indented.push('\n');
    }
    indented
}

fn is_primitive(ty: &Type) -> bool {
    matches!(
        ty,
        Type::U8
            | Type::S8
            | Type::U16
            | Type::S16
            | Type::U32
            | Type::S32
            | Type::U64
            | Type::S64
            | Type::Float32
            | Type::Float64
    )
}

fn package_name() -> String {
    format!("wit.worlds")
}

fn world_name(resolve: &Resolve, world: WorldId) -> String {
    format!(
        "{}",
        resolve.worlds[world].name.to_upper_camel_case()
    )
}

fn interface_name(resolve: &Resolve, name: &WorldKey, direction: Direction) -> String {
    let pkg = match name {
        WorldKey::Name(_) => None,
        WorldKey::Interface(id) => {
            let pkg = resolve.interfaces[*id].package.unwrap();
            Some(resolve.packages[pkg].name.clone())
        }
    };

    let name = match name {
        WorldKey::Name(name) => name,
        WorldKey::Interface(id) => resolve.interfaces[*id].name.as_ref().unwrap(),
    }
        .to_upper_camel_case();

    format!(
        "{name}{}",
        // if let Some(name) = &pkg {
        //     format!(
        //         "{}.{}.",
        //         name.namespace.to_kotlin_ident(),
        //         name.name.to_kotlin_ident()
        //     )
        // } else {
        //     String::new()
        // },
        match direction {
            Direction::Import => "Imports",
            Direction::Export => "Exports",
        },
    )
}

trait ToJavaIdent: ToOwned {
    fn to_kotlin_ident(&self) -> Self::Owned;
}

impl ToJavaIdent for str {
    fn to_kotlin_ident(&self) -> String {
        // Escape Kotlin hard keywords -> maybe need to add soft keyword and modifiers later
        // Source: https://kotlinlang.org/docs/keyword-reference.html#hard-keywords
        match self {
            "as" | "break" | "class" | "continue" | "do" | "else" | "false" | "for"
            | "fun" | "if" | "in" | "interface" | "is" | "null" | "object" | "package"
            | "return" | "super" | "this" | "throw" | "true" | "try" | "typealias"
            | "typeof" | "val" | "var" | "when" | "while" => format!("`{self}`"),
            _ => self.to_lower_camel_case(),
        }
    }
}
