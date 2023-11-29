plugins {
    kotlin("multiplatform") version "1.9.20"
}

group = "xy.wit-bindgen"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

kotlin {
    wasmJs {
        binaries.executable()
        nodejs()
    }
    sourceSets {
        val wasmJsMain by getting
    }
}

tasks.withType<org.jetbrains.kotlin.gradle.dsl.KotlinCompile<*>>().configureEach {
    kotlinOptions {
        freeCompilerArgs += "-opt-in=kotlin.wasm.unsafe.UnsafeWasmMemoryApi"
        freeCompilerArgs += "-opt-in=kotlin.ExperimentalUnsignedTypes"
    }
}
