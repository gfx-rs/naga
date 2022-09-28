use lasso::{Rodeo, Spur};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Text(Spur);

pub struct Interner {
	rodeo: Rodeo,
}

impl Interner {
	pub fn new() -> Self { Self { rodeo: Rodeo::new() } }

	pub fn get(&mut self, text: &str) -> Text { Text(self.rodeo.get_or_intern(text)) }

	pub fn get_static(&mut self, text: &'static str) -> Text { Text(self.rodeo.get_or_intern_static(text)) }

	pub fn resolve(&self, text: Text) -> &str { self.rodeo.resolve(&text.0) }
}
