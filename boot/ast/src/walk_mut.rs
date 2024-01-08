use std::{mem, sync::Arc};

use dendro_span::{
    fatal_error,
    ident::Ident,
    span::{DelimSpan, Span},
};
use smallvec::{smallvec, SmallVec};

use crate::{
    ast::*,
    token,
    token_stream::{TokenStream, TokenTree},
};

pub trait WalkMut: Sized {
    const WALK_TOKENS: bool;

    fn walk_leaf(&mut self, leaf: &mut Leaf) {
        default_walk_leaf(self, leaf)
    }

    fn flat_map_stmt(&mut self, stmt: Stmt) -> SmallVec<[Stmt; 1]> {
        default_flat_map_stmt(self, stmt)
    }

    fn walk_expr(&mut self, expr: &mut P<Expr>) {
        default_walk_expr(self, expr)
    }

    fn filter_map_expr(&mut self, expr: P<Expr>) -> Option<P<Expr>> {
        default_filter_map_expr(self, expr)
    }

    fn walk_block(&mut self, block: &mut P<Block>) {
        default_walk_block(self, block)
    }

    fn walk_local(&mut self, local: &mut Let) {
        default_walk_local(self, local)
    }

    fn walk_struct_field(&mut self, field: &mut StructField) {
        default_walk_struct_field(self, field)
    }

    fn flat_map_struct_field(&mut self, field: StructField) -> SmallVec<[StructField; 1]> {
        default_flat_map_struct_field(self, field)
    }

    fn walk_enum_field(&mut self, field: &mut EnumField) {
        default_walk_enum_field(self, field)
    }

    fn flat_map_enum_field(&mut self, field: EnumField) -> SmallVec<[EnumField; 1]> {
        default_flat_map_enum_field(self, field)
    }

    fn walk_match_arm(&mut self, arm: &mut MatchArm) {
        default_walk_match_arm(self, arm)
    }

    fn flat_map_match_arm(&mut self, arm: MatchArm) -> SmallVec<[MatchArm; 1]> {
        default_flat_map_match_arm(self, arm)
    }

    fn walk_pat(&mut self, pat: &mut P<Pat>) {
        default_walk_pat(self, pat)
    }

    fn walk_pat_field(&mut self, field: &mut PatField) {
        default_walk_pat_field(self, field)
    }

    fn flat_map_pat_field(&mut self, field: PatField) -> SmallVec<[PatField; 1]> {
        default_flat_map_pat_field(self, field)
    }

    fn walk_prereq(&mut self, prereq: &mut Prerequisites) {
        default_walk_prereq(self, prereq)
    }

    fn walk_lifetime(&mut self, lifetime: &mut Lifetime) {
        default_walk_lifetime(self, lifetime)
    }

    fn walk_mutability(&mut self, mutb: &mut Mutability) {
        default_walk_mutability(self, mutb)
    }

    fn walk_ident(&mut self, ident: &mut Ident) {
        default_walk_ident(self, ident)
    }

    fn walk_operator(&mut self, op: &mut Operator) {
        default_walk_operator(self, op)
    }

    fn walk_attribute(&mut self, attr: &mut Attribute) {
        default_walk_attribute(self, attr)
    }

    fn walk_id(&mut self, id: &mut u32) {
        let _ = id;
    }

    fn walk_span(&mut self, span: &mut Span) {
        let _ = span;
    }
}

pub fn walk_slice<T, F: FnMut(&mut T)>(s: &mut [T], f: F) {
    s.iter_mut().for_each(f);
}

pub fn flat_map_vec<T, F, I>(vec: &mut Vec<T>, f: F)
where
    F: FnMut(T) -> I,
    I: IntoIterator<Item = T>,
{
    *vec = mem::take(vec).into_iter().flat_map(f).collect();
}

pub fn walk_opt<T, F: FnOnce(&mut T)>(opt: &mut Option<T>, f: F) {
    if let Some(t) = opt {
        f(t);
    }
}

pub fn walk_exprs(walk: &mut impl WalkMut, exprs: &mut Vec<P<Expr>>) {
    *exprs = mem::take(exprs)
        .into_iter()
        .filter_map(|expr| walk.filter_map_expr(expr))
        .collect();
}

pub fn walk_attr_args(walk: &mut impl WalkMut, args: &mut AttrArgs) {
    match args {
        AttrArgs::Empty => {}
        AttrArgs::Delimited(span, _, tts) => {
            walk_delim_span(walk, span);
            walk_token_stream(walk, tts);
        }
        AttrArgs::Eq(span, expr) => {
            walk.walk_span(span);
            walk.walk_expr(expr);
        }
    }
}

pub fn walk_delim_span(walk: &mut impl WalkMut, span: &mut DelimSpan) {
    walk.walk_span(&mut span.open);
    walk.walk_span(&mut span.close);
}

pub fn walk_token_stream<W: WalkMut>(walk: &mut W, tts: &mut TokenStream) {
    if W::WALK_TOKENS && !tts.is_empty() {
        let tts = Arc::make_mut(&mut tts.0);
        walk_slice(tts, |tt| walk_token_tree(walk, tt))
    }
}

pub fn walk_token_tree(walk: &mut impl WalkMut, tt: &mut TokenTree) {
    match tt {
        TokenTree::Token(token, _) => walk_token(walk, token),
        TokenTree::Delimited(span, _, _, tts) => {
            walk_delim_span(walk, span);
            walk_token_stream(walk, tts)
        }
    }
}

pub fn walk_token(walk: &mut impl WalkMut, token: &mut token::Token) {
    match &mut token.kind {
        token::Ident(sym, _) | token::Lifetime(sym) => {
            let mut ident = Ident::new(*sym, token.span);
            walk.walk_ident(&mut ident);
            *sym = ident.name;
            token.span = ident.span;
        }
        _ => walk.walk_span(&mut token.span),
    }
}

pub fn default_walk_leaf(walk: &mut impl WalkMut, leaf: &mut Leaf) {
    walk.walk_id(&mut leaf.id);
    walk_slice(&mut leaf.attrs, |attr| walk.walk_attribute(attr));
    flat_map_vec(&mut leaf.stmts, |stmt| walk.flat_map_stmt(stmt));
    walk.walk_span(&mut leaf.span);
}

pub fn default_flat_map_stmt(walk: &mut impl WalkMut, mut stmt: Stmt) -> SmallVec<[Stmt; 1]> {
    walk.walk_id(&mut stmt.id);
    walk.walk_span(&mut stmt.span);
    let stmts: SmallVec<[Stmt; 1]> = default_flat_map_stmt_kind(walk, stmt.kind)
        .into_iter()
        .map(|kind| Stmt { kind, ..stmt })
        .collect();
    if stmts.len() > 1 {
        fatal_error!("cloning node ids directly");
    }
    stmts
}

pub fn default_flat_map_stmt_kind(
    walk: &mut impl WalkMut,
    kind: StmtKind,
) -> SmallVec<[StmtKind; 1]> {
    match kind {
        StmtKind::Expr(expr) => walk
            .filter_map_expr(expr)
            .into_iter()
            .map(StmtKind::Expr)
            .collect(),
        StmtKind::Semi(expr) => walk
            .filter_map_expr(expr)
            .into_iter()
            .map(StmtKind::Semi)
            .collect(),
        StmtKind::Empty => smallvec![StmtKind::Empty],
    }
}

pub fn default_walk_expr(walk: &mut impl WalkMut, expr: &mut P<Expr>) {
    let expr = expr.get_mut();
    match &mut expr.kind {
        ExprKind::Ident(ident) => walk.walk_ident(ident),
        ExprKind::Literal(_) => {}
        ExprKind::Operator(op) => walk.walk_operator(op),
        ExprKind::Projection(proj) => walk_exprs(walk, proj),
        ExprKind::Prereq(prereq, expr) => {
            walk.walk_prereq(prereq);
            walk.walk_expr(expr)
        }
        ExprKind::Let(local) => walk.walk_local(local),
        ExprKind::If(pred, body, other) => {
            walk.walk_expr(pred);
            walk.walk_expr(body);
            walk_opt(other, |other| walk.walk_expr(other))
        }
        ExprKind::While(pred, body) => {
            walk.walk_expr(pred);
            walk.walk_expr(body);
        }
        ExprKind::Loop(body) => walk.walk_expr(body),
        ExprKind::ForLoop(pat, iter, body) => {
            walk.walk_pat(pat);
            walk.walk_expr(iter);
            walk.walk_expr(body);
        }
        ExprKind::Match(expr, arms) => {
            walk.walk_expr(expr);
            flat_map_vec(arms, |arm| walk.flat_map_match_arm(arm))
        }
        ExprKind::AddrOf(lifetime, mutb, expr) => {
            walk_opt(lifetime, |lifetime| walk.walk_lifetime(lifetime));
            walk.walk_mutability(mutb);
            walk.walk_expr(expr);
        }
        ExprKind::Unary(_, expr) => walk.walk_expr(expr),
        ExprKind::Binary(lhs, _, rhs) => {
            walk.walk_expr(lhs);
            walk.walk_expr(rhs);
        }
        ExprKind::Array(exprs) => walk_exprs(walk, exprs),
        ExprKind::ArrayRepeated(expr, count) => {
            walk.walk_expr(expr);
            walk.walk_expr(count);
        }
        ExprKind::Tuple(exprs) => walk_exprs(walk, exprs),
        ExprKind::Struct(fields) => flat_map_vec(fields, |field| walk.flat_map_struct_field(field)),
        ExprKind::Enum(fields) => flat_map_vec(fields, |field| walk.flat_map_enum_field(field)),
        ExprKind::Annotated(lifetime, expr) => {
            walk.walk_lifetime(lifetime);
            walk.walk_expr(expr);
        }
        ExprKind::Block(block) => walk.walk_block(block),
        ExprKind::Lambda(implicit_args, args, ty, expr) => {
            walk_slice(implicit_args, |iarg| walk.walk_pat(iarg));
            walk_slice(args, |arg| walk.walk_pat(arg));
            walk_opt(ty, |ty| walk.walk_expr(ty));
            walk.walk_expr(expr);
        }
        ExprKind::Assign(lhs, span, rhs) => {
            walk.walk_expr(lhs);
            walk.walk_expr(rhs);
            walk.walk_span(span);
        }
        ExprKind::AssignOp(lhs, _, rhs) => {
            walk.walk_expr(lhs);
            walk.walk_expr(rhs);
        }
        ExprKind::Range(lhs, _, rhs) => {
            walk_opt(lhs, |lhs| walk.walk_expr(lhs));
            walk_opt(rhs, |rhs| walk.walk_expr(rhs));
        }
        ExprKind::Underscore => {}
        ExprKind::Break(lifetime, expr) => {
            walk_opt(lifetime, |l| walk.walk_lifetime(l));
            walk_opt(expr, |expr| walk.walk_expr(expr));
        }
        ExprKind::Continue(lifetime) => walk_opt(lifetime, |l| walk.walk_lifetime(l)),
        ExprKind::Return(expr) => walk_opt(expr, |expr| walk.walk_expr(expr)),
        ExprKind::Call(func, implicit, args) => {
            walk.walk_expr(func);
            walk_exprs(walk, implicit);
            walk_exprs(walk, args);
        }
        ExprKind::Try(expr) => walk.walk_expr(expr),
        ExprKind::Exists(expr) => walk.walk_expr(expr),
        ExprKind::BelongsTo(term, ty) => {
            walk.walk_expr(term);
            walk.walk_expr(ty)
        }
        ExprKind::Paren(expr) => walk.walk_expr(expr),
        ExprKind::Err => {}
    }
    walk_slice(&mut expr.attrs, |attr| walk.walk_attribute(attr));
    walk.walk_id(&mut expr.id);
    walk.walk_span(&mut expr.span);
}

pub fn default_filter_map_expr(walk: &mut impl WalkMut, mut expr: P<Expr>) -> Option<P<Expr>> {
    walk.walk_expr(&mut expr);
    Some(expr)
}

pub fn default_walk_block(walk: &mut impl WalkMut, block: &mut P<Block>) {
    let block = block.get_mut();
    walk.walk_id(&mut block.id);
    match &mut block.kind {
        BlockKind::Loaded { stmts, span, .. } => {
            flat_map_vec(stmts, |stmt| walk.flat_map_stmt(stmt));
            walk.walk_span(span);
        }
        BlockKind::Unloaded => {}
    }
}

pub fn default_walk_local(walk: &mut impl WalkMut, local: &mut Let) {
    walk.walk_pat(&mut local.pat);
    walk_opt(&mut local.ty, |ty| walk.walk_expr(ty));
    walk.walk_expr(&mut local.expr);
}

pub fn default_walk_struct_field(walk: &mut impl WalkMut, field: &mut StructField) {
    walk.walk_prereq(&mut field.prerequisites);
    walk_slice(&mut field.attrs, |attr| walk.walk_attribute(attr));
    walk.walk_id(&mut field.id);
    match &mut field.kind {
        StructFieldKind::Simple { pat, expr } => {
            walk.walk_pat(pat);
            walk.walk_expr(expr);
        }
        StructFieldKind::Ident(ident) => walk.walk_ident(ident),
        StructFieldKind::Reuse(reuse) => walk.walk_expr(reuse),
    }
    walk.walk_span(&mut field.span);
}

pub fn default_flat_map_struct_field(
    walk: &mut impl WalkMut,
    mut field: StructField,
) -> SmallVec<[StructField; 1]> {
    walk.walk_struct_field(&mut field);
    smallvec![field]
}

pub fn default_walk_enum_field(walk: &mut impl WalkMut, field: &mut EnumField) {
    walk.walk_prereq(&mut field.prerequisites);
    walk_slice(&mut field.attrs, |attr| walk.walk_attribute(attr));
    walk.walk_id(&mut field.id);
    walk.walk_pat(&mut field.pat);
    walk_opt(&mut field.ty, |ty| walk.walk_expr(ty));
    walk.walk_span(&mut field.span);
}

pub fn default_flat_map_enum_field(
    walk: &mut impl WalkMut,
    mut field: EnumField,
) -> SmallVec<[EnumField; 1]> {
    walk.walk_enum_field(&mut field);
    smallvec![field]
}

pub fn default_walk_match_arm(walk: &mut impl WalkMut, arm: &mut MatchArm) {
    walk.walk_prereq(&mut arm.prerequisites);
    walk_slice(&mut arm.attrs, |attr| walk.walk_attribute(attr));
    walk.walk_id(&mut arm.id);
    walk.walk_pat(&mut arm.pat);
    walk.walk_expr(&mut arm.expr);
    walk.walk_span(&mut arm.span);
}

pub fn default_flat_map_match_arm(
    walk: &mut impl WalkMut,
    mut arm: MatchArm,
) -> SmallVec<[MatchArm; 1]> {
    walk.walk_match_arm(&mut arm);
    smallvec![arm]
}

pub fn default_walk_pat(walk: &mut impl WalkMut, pat: &mut P<Pat>) {
    let pat = pat.get_mut();
    match &mut pat.kind {
        PatKind::Wildcard => {}
        PatKind::Path(expr) => walk.walk_expr(expr),
        PatKind::Operator(op) => walk.walk_operator(op),
        PatKind::Ident(_, _, ident, alias) => {
            walk.walk_ident(ident);
            walk_opt(alias, |alias| walk.walk_pat(alias))
        }
        PatKind::Array(pats) | PatKind::Tuple(pats) | PatKind::Or(pats) => {
            walk_slice(pats, |pat| walk.walk_pat(pat))
        }
        PatKind::Struct(fields, _) => flat_map_vec(fields, |field| walk.flat_map_pat_field(field)),
        PatKind::StructGlob => {}
        PatKind::Deref(_, _, pat) => walk.walk_pat(pat),
        PatKind::Literal(_) => {}
        PatKind::Range(lhs, _, rhs) => {
            walk_opt(lhs, |lhs| walk.walk_pat(lhs));
            walk_opt(rhs, |rhs| walk.walk_pat(rhs));
        }
        PatKind::Paren(pat) => walk.walk_pat(pat),
        PatKind::Call(func, implicit_args, args) => {
            walk.walk_pat(func);
            walk_slice(implicit_args, |arg| walk.walk_pat(arg));
            walk_slice(args, |arg| walk.walk_pat(arg));
        }
        PatKind::Err => {}
    }
}

pub fn default_walk_pat_field(walk: &mut impl WalkMut, field: &mut PatField) {
    walk.walk_id(&mut field.id);
    walk.walk_ident(&mut field.ident);
    walk.walk_pat(&mut field.pat);
    walk.walk_span(&mut field.span);
    walk_slice(&mut field.attrs, |attr| walk.walk_attribute(attr));
}

pub fn default_flat_map_pat_field(
    walk: &mut impl WalkMut,
    mut field: PatField,
) -> SmallVec<[PatField; 1]> {
    walk.walk_pat_field(&mut field);
    smallvec![field]
}

pub fn default_walk_prereq(walk: &mut impl WalkMut, prereq: &mut Prerequisites) {
    walk_slice(&mut prereq.forall, |ident| walk.walk_ident(ident));
    flat_map_vec(&mut prereq.where_clause, |expr| walk.filter_map_expr(expr));
    walk.walk_id(&mut prereq.id);
    walk.walk_span(&mut prereq.span);
}

pub fn default_walk_lifetime(walk: &mut impl WalkMut, lifetime: &mut Lifetime) {
    walk.walk_id(&mut lifetime.id);
    walk.walk_ident(&mut lifetime.ident);
}

pub fn default_walk_mutability(walk: &mut impl WalkMut, mutb: &mut Mutability) {
    walk.walk_id(&mut mutb.id);
    walk.walk_ident(&mut mutb.ident);
}

pub fn default_walk_ident(walk: &mut impl WalkMut, ident: &mut Ident) {
    walk.walk_span(&mut ident.span);
}

pub fn default_walk_operator(walk: &mut impl WalkMut, op: &mut Operator) {
    match op {
        Operator::Binary(op) => walk.walk_span(&mut op.span),
        Operator::Unary(op) => walk.walk_span(&mut op.span),
        Operator::Assign(span) => walk.walk_span(span),
        Operator::AssignBinary(op) => walk.walk_span(&mut op.span),
    }
}

pub fn default_walk_attribute(walk: &mut impl WalkMut, attr: &mut Attribute) {
    match &mut attr.kind {
        AttrKind::Normal(path, args) => {
            walk.walk_expr(path);
            walk_attr_args(walk, args)
        }
        AttrKind::Comment(..) => {}
    }
    walk.walk_span(&mut attr.span)
}
