use dendro_ast::id::NodeId;

pub struct Resolver {
    next_node_id: NodeId,
}

impl dendro_expand::Resolver for Resolver {
    fn next_node_id(&mut self) -> NodeId {
        let id = self.next_node_id;
        let next = id.as_u32().checked_add(1).expect("`NodeId` overflow");
        self.next_node_id = NodeId::from_u32(next);
        id
    }

    fn next_node_ids(&mut self, count: usize) -> std::ops::Range<NodeId> {
        let start = self.next_node_id;
        let end = start
            .as_usize()
            .checked_add(count)
            .expect("`NodeId` overflow");
        self.next_node_id = NodeId::from_usize(end);
        start..self.next_node_id
    }
}
