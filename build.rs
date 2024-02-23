use cmake::Config;
use reqwest::blocking as reqwest;
use tar::Archive;
use xz2::read::XzDecoder;

pub fn main() -> color_eyre::Result<()> {
    color_eyre::install().unwrap();
    // download_llvm()?;

    return Ok(());
}

#[cfg(feature = "llvm")]
fn build_llvm() {
    todo!()
}

fn download_llvm() -> color_eyre::Result<()> {
    let llvm_path = dirs::cache_dir()
        .unwrap_or_else(std::env::temp_dir)
        .join("emunes/llvm17");

    // Download LLVM 17 (if necessary)
    if !llvm_path.exists() {
        let req = reqwest::get("https://github.com/llvm/llvm-project/releases/download/llvmorg-17.0.6/llvm-project-17.0.6.src.tar.xz")?;
        if !req.status().is_success() {
            return Err(color_eyre::Report::msg(req.text()?));
        }

        let tar = XzDecoder::new(req);
        let mut archive = Archive::new(tar);
        archive.unpack(&llvm_path)?;
    }

    let llvm_path = Config::new(llvm_path.join("llvm-project-17.0.6.src/llvm")).build();
    println!(
        "cargo:rustc-env=LLVM_SYS_170_PREFIX={}",
        llvm_path.display()
    );

    return Ok(());
}
