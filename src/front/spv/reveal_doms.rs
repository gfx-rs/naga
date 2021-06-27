//! Code transformation to reveal concealed dominators.

use std::mem;

/// Transform a `Switch` statement to make dominators visible in the statement tree.
///
///
pub fn reveal_dominators(
    statements: &mut crate::Block,
    mut switch_pos: usize,
    mut default: crate::Block,
) {
    use BreakDisposition::*;

    dbg!(&default);

    // Find the longest prefix of statements that will never break,
    // and move them before the `switch`.
    let count = default
        .iter()
        .take_while(|stmt| stmt.disposition() == Never)
        .count();
    statements.splice(switch_pos..switch_pos, default.drain(..count));
    switch_pos += count;

    // Process the rest of the default block.
    let mut i = 0;
    while i < default.len() {
        match default[i].disposition() {
            Never => {}
            Sometimes => {
                // If we find an `if` statement with a consequent that always
                // breaks, then its other consequent dominates the rest of the
                // default block, so move that out to follow the `if` in the
                // default block.
                if let crate::Statement::If {
                    ref mut accept,
                    ref mut reject,
                    ..
                } = default[i]
                {
                    if accept.disposition() == Always {
                        let reject = mem::take(reject);
                        default.splice(i + 1..i + 1, reject);
                    } else if reject.disposition() == Always {
                        let accept = mem::take(accept);
                        default.splice(i + 1..i + 1, accept);
                    }
                }
            }
            Always => {
                // The rest of the default block is dead.
                default.drain(i + 1..);
                break;
            }
        }
        i += 1;
    }

    dbg!(&default);

    // Patch our modified default back into the `Switch` statement.
    // if it's empty, just drop the switch altogether.
    if default.is_empty() {
        statements.drain(switch_pos..switch_pos + 1);
    } else if let crate::Statement::Switch {
        default: ref mut def,
        ..
    } = statements[switch_pos]
    {
        *def = default;
    } else {
        unreachable!()
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum BreakDisposition {
    Never,
    Sometimes,
    Always,
}

impl BreakDisposition {
    fn or(self, other: BreakDisposition) -> BreakDisposition {
        if self == other {
            self
        } else {
            BreakDisposition::Sometimes
        }
    }
}

trait Disposition {
    fn disposition(&self) -> BreakDisposition;
}

impl Disposition for crate::Statement {
    fn disposition(&self) -> BreakDisposition {
        use crate::Statement as S;
        use BreakDisposition::*;

        match *self {
            S::Break => Always,
            S::If {
                ref accept,
                ref reject,
                ..
            } => accept.disposition().or(reject.disposition()),
            _ => Never,
        }
    }
}

impl Disposition for crate::Block {
    fn disposition(&self) -> BreakDisposition {
        self.iter()
            .fold(None, |prior, statement| {
                let this = statement.disposition();
                prior.map(|p: BreakDisposition| p.or(this)).or(Some(this))
            })
            .unwrap_or(BreakDisposition::Never)
    }
}
