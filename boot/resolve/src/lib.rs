use std::{
    collections::{hash_map, HashMap},
    fmt, mem,
};

use dendro_ast::{
    ast::*,
    id::NodeId,
    walk::{self, Walk},
};
use dendro_error::DiagCx;
use dendro_span::{
    fatal_error,
    ident::{kw, Ident},
    span::Span,
};

const NS_NUM: usize = 4;
const NS_NORMAL: usize = 0;
const NS_LIFETIME: usize = 1;
const NS_MUTB: usize = 2;
const NS_OPERATOR: usize = 3;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(NodeId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefIdent {
    Ident(Ident),
    Operator(Operator),
}

#[derive(Debug)]
pub struct Definition {
    pub ident: DefIdent,
    pub id: DefId,

    pub parent: Option<DefId>,
    pub projections: Vec<DefId>,
}

impl DefIdent {
    fn span(&self) -> Span {
        match self {
            Self::Ident(ident) => ident.span,
            Self::Operator(op) => op.span(),
        }
    }
}

impl From<Ident> for DefIdent {
    fn from(ident: Ident) -> Self {
        Self::Ident(ident)
    }
}

impl From<Operator> for DefIdent {
    fn from(op: Operator) -> Self {
        Self::Operator(op)
    }
}

impl fmt::Display for DefIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(ident) => ident.fmt(f),
            Self::Operator(op) => op.fmt(f),
        }
    }
}

#[derive(Debug, Default, Clone)]
struct Scope {
    ident_map: [HashMap<DefIdent, DefId>; NS_NUM],
    can_shadow: bool,
}

#[derive(Debug)]
pub struct Resolver<'diag> {
    next_node_id: NodeId,

    diag: &'diag DiagCx,

    scope: Scope,
    scope_stack: Vec<Scope>,

    parent: Option<DefId>,
    def_map: HashMap<NodeId, DefId>,
    definitions: HashMap<DefId, Definition>,
}

impl<'diag> Resolver<'diag> {
    pub fn new(diag: &'diag DiagCx) -> Self {
        Self {
            next_node_id: NodeId::from_u32(0),
            diag,
            scope: Scope::default(),
            scope_stack: Vec::new(),
            parent: None,
            def_map: HashMap::new(),
            definitions: HashMap::new(),
        }
    }
}

impl dendro_expand::Resolver for Resolver<'_> {
    fn next_node_id(&mut self) -> NodeId {
        self.next_node_ids(1).start
    }

    fn next_node_ids(&mut self, count: usize) -> std::ops::Range<NodeId> {
        let start = self.next_node_id;
        let end = start
            .as_usize()
            .checked_add(count)
            .unwrap_or_else(|| fatal_error!("`NodeId` overflow"));
        self.next_node_id = NodeId::from_usize(end);
        start..self.next_node_id
    }
}

impl<'diag> Resolver<'diag> {
    fn define(&mut self, ident: impl Into<DefIdent>, ns: usize, node_id: NodeId) {
        let ident = ident.into();

        let def_id = match self.scope.ident_map[ns].entry(ident) {
            hash_map::Entry::Occupied(mut entry) => {
                if self.scope.can_shadow {
                    *entry.get_mut() = DefId(node_id);
                }
                *entry.get()
            }
            hash_map::Entry::Vacant(entry) => *entry.insert(DefId(node_id)),
        };

        self.def_map.insert(node_id, def_id);
        self.definitions.insert(def_id, Definition {
            ident,
            id: def_id,
            parent: self.parent,
            projections: Vec::new(),
        });
    }

    fn resolve(&mut self, ident: impl Into<DefIdent>, ns: usize, node_id: NodeId) {
        let ident = ident.into();
        match self.scope.ident_map[ns].get(&ident) {
            Some(&def_id) => {
                self.def_map.insert(node_id, def_id);
            }
            None => {
                self.diag
                    .span_err(ident.span(), format_args!("undefined identifier `{ident}`"));
                // TODO: Make suggestions.
            }
        }
    }

    fn resolve_path(&mut self, path: &[P<Expr>], _node_id: NodeId) {
        let Some((first, next)) = path.split_first() else {
            return;
        };
        self.walk_expr(first);

        let len = next.len();
        if len == 0 {
            return;
        }

        todo!("resolve path")
    }
}

impl<'ast, 'diag> Walk<'ast> for Resolver<'diag> {
    fn walk_leaf(&mut self, leaf: &'ast Leaf) {
        self.def_map.insert(leaf.id, DefId(leaf.id));
        self.definitions.insert(DefId(leaf.id), Definition {
            ident: DefIdent::Ident(Ident::new(kw::LEAF, leaf.span)),
            id: DefId(leaf.id),
            parent: self.parent,
            projections: Vec::new(),
        });

        let parent_def_id = self.parent.replace(DefId(leaf.id));
        walk::default_walk_leaf(self, leaf);
        self.parent = parent_def_id;
    }

    fn walk_expr(&mut self, expr: &'ast P<Expr>) {
        match expr.kind {
            ExprKind::Ident(ident) => self.resolve(ident, NS_NORMAL, expr.id),

            ExprKind::Projection(ref path) => self.resolve_path(path, expr.id),

            ExprKind::Struct(_) => todo!("resolve struct expr"),

            ExprKind::Annotated(lifetime, _) => self.define(lifetime.ident, NS_LIFETIME, expr.id),

            ExprKind::Block(ref block) => {
                let BlockKind::Loaded { is_inline, .. } = block.kind else {
                    unreachable!("unexpanded block")
                };
                let scope = if is_inline {
                    self.scope.clone()
                } else {
                    Scope::default()
                };
                self.scope_stack.push(scope);
                walk::default_walk_block(self, block);
                self.scope = self.scope_stack.pop().unwrap();
            }

            ExprKind::Continue(lifetime) | ExprKind::Break(lifetime, ..) => {
                if let Some(lifetime) = lifetime {
                    self.resolve(lifetime.ident, NS_LIFETIME, expr.id);
                }
            }

            ExprKind::Unary(unop, _) => self.resolve(Operator::Unary(unop), NS_OPERATOR, expr.id),
            ExprKind::Binary(_, binop, _) => {
                self.resolve(Operator::Binary(binop), NS_OPERATOR, expr.id)
            }
            ExprKind::Assign(_, span, _) => {
                self.resolve(Operator::Assign(span), NS_OPERATOR, expr.id)
            }
            ExprKind::AssignOp(_, binop, _) => {
                self.resolve(Operator::AssignBinary(binop), NS_OPERATOR, expr.id)
            }

            ExprKind::Operator(op) => self.resolve(op, NS_OPERATOR, expr.id),

            ExprKind::Literal(_)
            | ExprKind::Prereq(..)
            | ExprKind::Let(_)
            | ExprKind::If(..)
            | ExprKind::While(..)
            | ExprKind::Loop(_)
            | ExprKind::ForLoop(..)
            | ExprKind::Match(..)
            | ExprKind::AddrOf(..)
            | ExprKind::Array(_)
            | ExprKind::ArrayRepeated(..)
            | ExprKind::Tuple(_)
            | ExprKind::Enum(_)
            | ExprKind::Lambda(..)
            | ExprKind::Range(..)
            | ExprKind::Underscore
            | ExprKind::Return(_)
            | ExprKind::Call(..)
            | ExprKind::Try(_)
            | ExprKind::Exists(_)
            | ExprKind::BelongsTo(..)
            | ExprKind::Paren(_)
            | ExprKind::Err => {}
        }
        walk::default_walk_expr(self, expr)
    }

    fn walk_block(&mut self, block: &'ast P<Block>) {
        walk::default_walk_block(self, block)
    }

    fn walk_struct_field(&mut self, field: &'ast StructField) {
        walk::default_walk_struct_field(self, field)
    }

    fn walk_pat(&mut self, pat: &'ast P<Pat>) {
        match pat.kind {
            PatKind::Ident(_mode, mutb, ident, ..) => {
                self.define(ident, NS_NORMAL, pat.id);
                if !mutb.is_kw() {
                    self.define(ident, NS_MUTB, pat.id);
                }
            }

            PatKind::Struct(..) => todo!("handle struct"),
            PatKind::StructGlob => todo!("handle glob"),

            PatKind::Operator(op) => self.define(op, NS_OPERATOR, pat.id),

            PatKind::Or(_) => {
                let can_shadow = mem::replace(&mut self.scope.can_shadow, false);
                walk::default_walk_pat(&mut *self, pat);
                self.scope.can_shadow = can_shadow;
                return;
            }

            PatKind::Deref(lifetime, mutb, ..) => {
                if let Some(lifetime) = lifetime {
                    self.resolve(lifetime.ident, NS_LIFETIME, pat.id);
                }
                if !mutb.is_kw() {
                    self.resolve(mutb.ident, NS_MUTB, pat.id);
                }
            }

            PatKind::Path(_)
            | PatKind::Wildcard
            | PatKind::Array(_)
            | PatKind::Tuple(_)
            | PatKind::Literal(_)
            | PatKind::Range(..)
            | PatKind::Paren(_)
            | PatKind::Call(..)
            | PatKind::Err => {}
        }
        walk::default_walk_pat(self, pat)
    }

    fn walk_pat_field(&mut self, field: &'ast PatField) {
        walk::default_walk_pat_field(self, field)
    }

    fn walk_prereq(&mut self, prereq: &'ast Prerequisites) {
        walk::default_walk_prereq(self, prereq)
    }

    fn walk_attribute(&mut self, attr: &'ast Attribute) {
        walk::default_walk_attribute(self, attr)
    }
}
