use std::panic::resume_unwind;

#[derive(Debug)]
pub struct FatalError(());

impl FatalError {
    pub fn raise() -> ! {
        resume_unwind(Box::new(FatalError(())))
    }
}

impl std::fmt::Display for FatalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fatal error")
    }
}

impl std::error::Error for FatalError {}

#[macro_export]
macro_rules! fatal_error {
    ($($t:tt)*) => ({
        eprintln!("fatal error: {}", format_args!($($t)*));
        $crate::FatalError::raise()
    });
}
