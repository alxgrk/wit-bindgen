use std::path::Path;
use std::process::Command;

macro_rules! codegen_test {
    ($id:ident $name:tt $test:tt) => {
        #[test]
        fn $id() {
            test_helpers::run_world_codegen_test(
                "guest-kotlin",
                $test.as_ref(),
                |resolve, world, files| {
                    wit_bindgen_kotlin::Opts {
                        generate_stub: true,
                    }
                    .build()
                    .generate(resolve, world, files)
                },
                verify,
            )
        }
    };
}
test_helpers::codegen_tests!("*.wit");

fn verify(dir: &Path, name: &str) {
    use std::fs;

    let kotlin_dir = &dir.join("src/wasmJsMain/kotlin");
    let package_dir = &kotlin_dir.join(format!("wit.worlds"));

    fs::create_dir_all(package_dir).unwrap();

    let src_files = fs::read_dir(&dir).unwrap().filter_map(|entry| {
        let path = entry.unwrap().path();
        if let Some("kt") = path.extension().map(|ext| ext.to_str().unwrap()) {
            Some(path)
        } else {
            None
        }
    });

    let dst_files = src_files.map(|src| {
        let dst = package_dir.join(src.file_name().unwrap());
        fs::rename(src, &dst).unwrap();
        dst
    });

    let mut cmd = Command::new("gradle");
    cmd.arg("build");

    // TODO
    // for file in dst_files {
    //     cmd.arg(file);
    // }

    test_helpers::run_command(&mut cmd);
}
