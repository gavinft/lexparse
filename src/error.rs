use std::{
    backtrace::Backtrace,
    error::Error,
    fmt::{Debug, Display, Pointer},
};

pub struct BacktracedError<T: Display + Debug> {
    err: T,
    backtrace: Backtrace,
}

impl<T: Display + Debug> BacktracedError<T> {
    #[track_caller]
    pub fn new(err: T) -> BacktracedError<T> {
        BacktracedError { 
            err: err, 
            backtrace: Backtrace::capture()
        }
    }
}

impl<T: Display + Debug> Error for BacktracedError<T> {}

impl<T: Display + Debug> Display for BacktracedError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.err, f)?;
        writeln!(f, "\nerror stack trace:")?;
        Display::fmt(&self.backtrace, f)
    }
}

impl<T: Display + Debug> Debug for BacktracedError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.err)?;
        writeln!(f, "error stack trace:")?;
        Display::fmt(&self.backtrace, f)
    }
}
