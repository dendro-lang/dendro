use dendro_span::ident::Ident;

use crate::ast::*;

pub trait Walk<'ast>: Sized {
    fn walk_leaf(&mut self, leaf: &'ast Leaf) {
        default_walk_leaf(self, leaf)
    }

    fn walk_stmt(&mut self, stmt: &'ast Stmt) {
        default_walk_stmt(self, stmt)
    }

    fn walk_expr(&mut self, expr: &'ast P<Expr>) {
        default_walk_expr(self, expr)
    }

    fn walk_block(&mut self, block: &'ast P<Block>) {
        default_walk_block(self, block)
    }

    fn walk_local(&mut self, local: &'ast Let) {
        default_walk_local(self, local)
    }

    fn walk_struct_field(&mut self, field: &'ast StructField) {
        default_walk_struct_field(self, field)
    }

    fn walk_enum_field(&mut self, field: &'ast EnumField) {
        default_walk_enum_field(self, field)
    }

    fn walk_match_arm(&mut self, arm: &'ast MatchArm) {
        default_walk_match_arm(self, arm)
    }

    fn walk_pat(&mut self, pat: &'ast P<Pat>) {
        default_walk_pat(self, pat)
    }

    fn walk_pat_field(&mut self, field: &'ast PatField) {
        default_walk_pat_field(self, field)
    }

    fn walk_prereq(&mut self, prereq: &'ast Prerequisites) {
        default_walk_prereq(self, prereq)
    }

    fn walk_lifetime(&mut self, lifetime: &'ast Lifetime) {
        default_walk_lifetime(self, lifetime)
    }

    fn walk_mutability(&mut self, mutb: &'ast Mutability) {
        default_walk_mutability(self, mutb)
    }

    fn walk_ident(&mut self, ident: &'ast Ident) {
        let _ = ident;
    }

    fn walk_operator(&mut self, op: &'ast Operator) {
        let _ = op;
    }

    fn walk_attribute(&mut self, attr: &'ast Attribute) {
        default_walk_attribute(self, attr)
    }
}

pub fn walk_many<T, F: FnMut(T)>(s: impl IntoIterator<Item = T>, f: F) {
    s.into_iter().for_each(f);
}

pub fn walk_opt<'ast, T: 'ast, F: FnOnce(&'ast T)>(opt: &'ast Option<T>, f: F) {
    if let Some(t) = opt {
        f(t);
    }
}

pub fn walk_attr_args<'ast>(walk: &mut impl Walk<'ast>, args: &'ast AttrArgs) {
    match args {
        AttrArgs::Empty | AttrArgs::Delimited(..) => {}
        AttrArgs::Eq(_, expr) => walk.walk_expr(expr),
    }
}

pub fn default_walk_leaf<'ast>(walk: &mut impl Walk<'ast>, leaf: &'ast Leaf) {
    walk_many(&leaf.attrs, |attr| walk.walk_attribute(attr));
    walk_many(&leaf.stmts, |stmt| walk.walk_stmt(stmt));
}

pub fn default_walk_stmt<'ast>(walk: &mut impl Walk<'ast>, stmt: &'ast Stmt) {
    match &stmt.kind {
        StmtKind::Expr(expr) | StmtKind::Semi(expr) => walk.walk_expr(expr),
        StmtKind::Empty => {}
    }
}

pub fn default_walk_expr<'ast>(walk: &mut impl Walk<'ast>, expr: &'ast P<Expr>) {
    match &expr.kind {
        ExprKind::Ident(ident) => walk.walk_ident(ident),
        ExprKind::Literal(_) => {}
        ExprKind::Operator(op) => walk.walk_operator(op),
        ExprKind::Projection(proj) => walk_many(proj, |expr| walk.walk_expr(expr)),
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
            walk_many(arms, |arm| walk.walk_match_arm(arm))
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
        ExprKind::Array(exprs) => walk_many(exprs, |expr| walk.walk_expr(expr)),
        ExprKind::ArrayRepeated(expr, count) => {
            walk.walk_expr(expr);
            walk.walk_expr(count);
        }
        ExprKind::Tuple(exprs) => walk_many(exprs, |expr| walk.walk_expr(expr)),
        ExprKind::Struct(fields) => walk_many(fields, |field| walk.walk_struct_field(field)),
        ExprKind::Enum(fields) => walk_many(fields, |field| walk.walk_enum_field(field)),
        ExprKind::Annotated(lifetime, expr) => {
            walk.walk_lifetime(lifetime);
            walk.walk_expr(expr);
        }
        ExprKind::Block(block) => walk.walk_block(block),
        ExprKind::Lambda(implicit_args, args, ty, expr) => {
            walk_many(implicit_args, |iarg| walk.walk_pat(iarg));
            walk_many(args, |arg| walk.walk_pat(arg));
            walk_opt(ty, |ty| walk.walk_expr(ty));
            walk.walk_expr(expr);
        }
        ExprKind::Assign(lhs, _, rhs) => {
            walk.walk_expr(lhs);
            walk.walk_expr(rhs);
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
            walk_many(implicit, |expr| walk.walk_expr(expr));
            walk_many(args, |expr| walk.walk_expr(expr));
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
    walk_many(&expr.attrs, |attr| walk.walk_attribute(attr));
}

pub fn default_walk_block<'ast>(walk: &mut impl Walk<'ast>, block: &'ast P<Block>) {
    match &block.kind {
        BlockKind::Loaded { stmts, .. } => {
            walk_many(stmts, |stmt| walk.walk_stmt(stmt));
        }
        BlockKind::Unloaded => {}
    }
}

pub fn default_walk_local<'ast>(walk: &mut impl Walk<'ast>, local: &'ast Let) {
    walk.walk_pat(&local.pat);
    walk_opt(&local.ty, |ty| walk.walk_expr(ty));
    walk.walk_expr(&local.expr);
}

pub fn default_walk_struct_field<'ast>(walk: &mut impl Walk<'ast>, field: &'ast StructField) {
    walk.walk_prereq(&field.prerequisites);
    walk_many(&field.attrs, |attr| walk.walk_attribute(attr));
    match &field.kind {
        StructFieldKind::Simple { pat, expr } => {
            walk.walk_pat(pat);
            walk.walk_expr(expr);
        }
        StructFieldKind::Ident(ident) => walk.walk_ident(ident),
        StructFieldKind::Reuse(reuse) => walk.walk_expr(reuse),
    }
}

pub fn default_walk_enum_field<'ast>(walk: &mut impl Walk<'ast>, field: &'ast EnumField) {
    walk.walk_prereq(&field.prerequisites);
    walk_many(&field.attrs, |attr| walk.walk_attribute(attr));
    walk.walk_pat(&field.pat);
    walk_opt(&field.ty, |ty| walk.walk_expr(ty));
}

pub fn default_walk_match_arm<'ast>(walk: &mut impl Walk<'ast>, arm: &'ast MatchArm) {
    walk.walk_prereq(&arm.prerequisites);
    walk_many(&arm.attrs, |attr| walk.walk_attribute(attr));
    walk.walk_pat(&arm.pat);
    walk.walk_expr(&arm.expr);
}

pub fn default_walk_pat<'ast>(walk: &mut impl Walk<'ast>, pat: &'ast P<Pat>) {
    match &pat.kind {
        PatKind::Wildcard => {}
        PatKind::Path(expr) => walk.walk_expr(expr),
        PatKind::Operator(op) => walk.walk_operator(op),
        PatKind::Ident(_, mutb, ident, alias) => {
            walk.walk_mutability(mutb);
            walk.walk_ident(ident);
            walk_opt(alias, |alias| walk.walk_pat(alias))
        }
        PatKind::Array(pats) | PatKind::Tuple(pats) | PatKind::Or(pats) => {
            walk_many(pats, |pat| walk.walk_pat(pat))
        }
        PatKind::Struct(fields, _) => walk_many(fields, |field| walk.walk_pat_field(field)),
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
            walk_many(implicit_args, |arg| walk.walk_pat(arg));
            walk_many(args, |arg| walk.walk_pat(arg));
        }
        PatKind::Err => {}
    }
}

pub fn default_walk_pat_field<'ast>(walk: &mut impl Walk<'ast>, field: &'ast PatField) {
    walk.walk_ident(&field.ident);
    walk.walk_pat(&field.pat);
    walk_many(&field.attrs, |attr| walk.walk_attribute(attr));
}

pub fn default_walk_prereq<'ast>(walk: &mut impl Walk<'ast>, prereq: &'ast Prerequisites) {
    walk_many(&prereq.forall, |ident| walk.walk_ident(ident));
    walk_many(&prereq.where_clause, |expr| walk.walk_expr(expr));
}

pub fn default_walk_lifetime<'ast>(walk: &mut impl Walk<'ast>, lifetime: &'ast Lifetime) {
    walk.walk_ident(&lifetime.ident);
}

pub fn default_walk_mutability<'ast>(walk: &mut impl Walk<'ast>, mutb: &'ast Mutability) {
    walk.walk_ident(&mutb.ident);
}

pub fn default_walk_attribute<'ast>(walk: &mut impl Walk<'ast>, attr: &'ast Attribute) {
    match &attr.kind {
        AttrKind::Normal(path, args) => {
            walk.walk_expr(path);
            walk_attr_args(walk, args)
        }
        AttrKind::Comment(..) => {}
    }
}
