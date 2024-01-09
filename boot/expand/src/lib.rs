#![feature(let_chains)]
#![feature(try_blocks)]

mod module;

use std::{mem, ops::Range};

use dendro_ast::{
    ast::{BlockKind, Expr, ExprKind, Leaf, Let, P},
    id::{NodeId, DUMMY_ID},
    walk_mut::{self, WalkMut},
};
use dendro_error::{DiagCx, Error};
use dendro_span::source::SourceMap;

use self::module::Module;

pub trait Resolver {
    fn next_node_id(&mut self) -> NodeId;

    fn next_node_ids(&mut self, count: usize) -> Range<NodeId>;
}

struct Expander<'a> {
    source_map: &'a SourceMap,
    diag: &'a DiagCx,
    resolver: &'a mut dyn Resolver,

    cur: Module,
}

impl<'a> WalkMut for Expander<'a> {
    const WALK_TOKENS: bool = false;

    fn walk_expr(&mut self, root: &mut P<Expr>) {
        if let ExprKind::Let(Let { pat, ty, expr }) = &mut root.get_mut().kind
            && let Some(ident) = pat.as_path_ident()
            && let ExprKind::Block(block) = &mut expr.get_mut().kind
            && let BlockKind::Unloaded = block.kind
        {
            let (leaf, module) = self::module::parse(self.diag, self.source_map, &self.cur, ident);
            let attrs = leaf.load_block(block.get_mut());
            expr.get_mut().attrs.extend(attrs);

            walk_mut::default_walk_pat(self, pat);
            walk_mut::walk_opt(ty, |ty| walk_mut::default_walk_expr(self, ty));

            let orig = mem::replace(&mut self.cur, module);
            walk_mut::default_walk_expr(self, expr);
            self.cur = orig;

            walk_mut::walk_slice(&mut root.get_mut().attrs, |attr| self.walk_attribute(attr));
            self.walk_id(&mut root.get_mut().id);
            self.walk_span(&mut root.get_mut().span);
        } else {
            walk_mut::default_walk_expr(self, root)
        }
    }

    fn walk_id(&mut self, id: &mut NodeId) {
        if *id == DUMMY_ID {
            *id = self.resolver.next_node_id();
        }
    }
}

pub fn expand(
    source_map: &SourceMap,
    diag: &DiagCx,
    resolver: &mut dyn Resolver,
    mut leaf: Leaf,
) -> Result<Leaf, Error> {
    let sfile = source_map.source_file(leaf.span.start);
    let mut expander = Expander {
        source_map,
        diag,
        resolver,
        cur: Module {
            path: sfile.path.clone(),
            prefix: sfile.path.parent().unwrap_or(&sfile.path).to_owned(),
            file_stack: vec![sfile.path.clone()],
        },
    };
    expander.walk_leaf(&mut leaf);
    Ok(leaf)
}
