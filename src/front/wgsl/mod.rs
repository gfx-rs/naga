/*!
Frontend for [WGSL][wgsl] (WebGPU Shading Language).

[wgsl]: https://gpuweb.github.io/gpuweb/wgsl.html
*/

use crate::front::wgsl::lower::Lowerer;
use crate::{SourceLocation, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use termcolor::{ColorChoice, NoColor, StandardStream};
use wgsl::diagnostic::{DiagnosticKind, Diagnostics};
use wgsl::parse::parse;
use wgsl::resolve::resolve;
use wgsl::text::Interner;

mod const_eval;
mod lower;

#[derive(Clone, Debug)]
pub struct WgslError {
    message: String,
    labels: Vec<(Span, String)>,
    notes: Vec<String>,
}

impl From<wgsl::diagnostic::Span> for Span {
    fn from(s: wgsl::diagnostic::Span) -> Self {
        Self::new(s.start, s.end)
    }
}

impl WgslError {
    fn from_wgsl(diag: wgsl::diagnostic::Diagnostic) -> Option<Self> {
        match diag.kind {
            DiagnosticKind::Error => Some(Self {
                message: diag.message,
                labels: diag
                    .labels
                    .into_iter()
                    .map(|l| (l.span.into(), l.message))
                    .collect(),
                notes: diag.notes,
            }),
            _ => None,
        }
    }

    pub fn labels(&self) -> impl Iterator<Item = (Span, &str)> + ExactSizeIterator + '_ {
        self.labels
            .iter()
            .map(|&(ref span, ref msg)| (span.clone(), msg.as_ref()))
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    fn diagnostic(&self) -> Diagnostic<()> {
        let diagnostic = Diagnostic::error()
            .with_message(self.message.to_string())
            .with_labels(
                self.labels
                    .iter()
                    .map(|label| {
                        Label::primary((), label.0.to_range().unwrap())
                            .with_message(label.1.to_string())
                    })
                    .collect(),
            )
            .with_notes(
                self.notes
                    .iter()
                    .map(|note| format!("note: {}", note))
                    .collect(),
            );
        diagnostic
    }

    /// Emits a summary of the error to standard error stream.
    pub fn emit_to_stderr(&self, source: &str) {
        self.emit_to_stderr_with_path(source, "wgsl")
    }

    /// Emits a summary of the error to standard error stream.
    pub fn emit_to_stderr_with_path(&self, source: &str, path: &str) {
        let files = SimpleFile::new(path, source);
        let config = codespan_reporting::term::Config::default();
        let writer = StandardStream::stderr(ColorChoice::Auto);
        term::emit(&mut writer.lock(), &config, &files, &self.diagnostic())
            .expect("cannot write error");
    }

    /// Emits a summary of the error to a string.
    pub fn emit_to_string(&self, source: &str) -> String {
        self.emit_to_string_with_path(source, "wgsl")
    }

    /// Emits a summary of the error to a string.
    pub fn emit_to_string_with_path(&self, source: &str, path: &str) -> String {
        let files = SimpleFile::new(path, source);
        let config = codespan_reporting::term::Config::default();
        let mut writer = NoColor::new(Vec::new());
        term::emit(&mut writer, &config, &files, &self.diagnostic()).expect("cannot write error");
        String::from_utf8(writer.into_inner()).unwrap()
    }

    /// Returns a [`SourceLocation`] for the first label in the error message.
    pub fn location(&self, source: &str) -> Option<SourceLocation> {
        self.labels.get(0).map(|label| label.0.location(source))
    }
}

impl std::fmt::Display for WgslError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for WgslError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

pub fn parse_str(source: &str) -> Result<crate::Module, Vec<WgslError>> {
    let mut intern = Interner::new();
    let mut diags = Diagnostics::new();

    let ast = parse(source, &mut intern, &mut diags);
    let module = resolve(ast, &mut intern, &mut diags);

    if diags.had_error() {
        return Err(diags
            .take()
            .into_iter()
            .filter_map(WgslError::from_wgsl)
            .collect());
    }

    Lowerer::new(&module, &intern).lower()
}
