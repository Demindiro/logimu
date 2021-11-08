use super::*;
use crate::arena::{Arena, Handle};
use core::iter;
use core::mem;

/// A graph of connected components
pub struct Graph<C, Uc, Un>
where
    C: Component,
{
    /// All components in this graph.
    nodes: Arena<Node<C, Uc>>,
    /// All nexuses in this graph with a list of connected nodes.
    nexuses: Arena<Nexus<Un>>,
    outputs: Vec<Handle>,
    inputs: Vec<Handle>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct GraphNodeHandle(Handle);

/// A nexus is a single point connecting multiple ports. It can have only one value at any time.
///
/// It is equivalent to a collection of interconnected wires in a circuit.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NexusHandle(Handle);

impl NexusHandle {
    pub fn index(self) -> usize {
        self.0.index()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Port {
    Input { node: GraphNodeHandle, port: usize },
    Output { node: GraphNodeHandle, port: usize },
}

impl Port {
    pub fn node(self) -> GraphNodeHandle {
        match self {
            Self::Input { node, .. } | Self::Output { node, .. } => node,
        }
    }
}

pub struct Node<C, Uc>
where
    C: Component,
{
    pub component: C,
    inputs: Box<[Option<NexusHandle>]>,
    outputs: Box<[Option<NexusHandle>]>,
    pub userdata: Uc,
}

pub struct Nexus<Un> {
    /// A list of components feeding into this nexus.
    inputs: Vec<GraphNodeHandle>,
    /// A list of components reading from this nexus.
    outputs: Vec<GraphNodeHandle>,
    pub userdata: Un,
}

impl<C, Uc, Un> Graph<C, Uc, Un>
where
    C: Component,
{
    pub fn new() -> Self {
        Self {
            nodes: Default::default(),
            nexuses: Default::default(),
            inputs: Default::default(),
            outputs: Default::default(),
        }
    }

    pub fn add(&mut self, component: C, userdata: Uc) -> GraphNodeHandle {
        let ic = component.input_count();
        let oc = component.output_count();
        let node = Node {
            component,
            inputs: iter::repeat(None).take(ic).collect(),
            outputs: iter::repeat(None).take(oc).collect(),
            userdata,
        };

        let i = self.nodes.insert(node);
        // FIXME unreliable
        assert!(ic != 0 || oc != 0, "impossible");
        if ic == 0 {
            self.inputs.push(i);
        }
        if oc == 0 {
            self.outputs.push(i);
        }
        GraphNodeHandle(i)
    }

    pub fn new_nexus(&mut self, userdata: Un) -> NexusHandle {
        let nexus = Nexus {
            inputs: Vec::new(),
            outputs: Vec::new(),
            userdata,
        };
        let h = NexusHandle(self.nexuses.insert(nexus));
        h
    }

    pub fn get(&self, handle: GraphNodeHandle) -> Option<(&C, &Uc)> {
        self.nodes
            .get(handle.0)
            .map(|n| (&n.component, &n.userdata))
    }

    pub fn get_mut(&mut self, handle: GraphNodeHandle) -> Option<(&mut C, &mut Uc)> {
        self.nodes
            .get_mut(handle.0)
            .map(|n| (&mut n.component, &mut n.userdata))
    }

    pub fn remove(&mut self, component: GraphNodeHandle) -> Result<(), RemoveError> {
        // Remove the component itself
        let node = self
            .nodes
            .remove(component.0)
            .ok_or(RemoveError::InvalidNode)?;

        // Disconnect from any nexuses
        for i in node.inputs.into_iter().filter_map(|i| *i) {
            let outp = &mut self.nexuses[i.0].outputs;
            outp.remove(outp.iter().position(|e| *e == component).unwrap());
        }
        for o in node.outputs.into_iter().filter_map(|i| *i) {
            let inp = &mut self.nexuses[o.0].inputs;
            inp.remove(inp.iter().position(|e| *e == component).unwrap());
        }

        // Remove from inputs and/or outputs list if it is one.
        self.inputs
            .iter()
            .position(|e| *e == component.0)
            .map(|i| self.inputs.remove(i));
        self.outputs
            .iter()
            .position(|e| *e == component.0)
            .map(|i| self.outputs.remove(i));

        Ok(())
    }

    pub fn remove_nexus(&mut self, handle: NexusHandle) -> Result<(), RemoveNexusError> {
        // Remove the nexus itself
        let nexus = self
            .nexuses
            .remove(handle.0)
            .ok_or(RemoveNexusError::InvalidNexus)?;

        // Disconnect from any components
        for i in nexus.inputs {
            let outp = &mut self.nodes[i.0].outputs;
            outp[outp.iter().position(|e| *e == Some(handle)).unwrap()] = None;
        }
        for o in nexus.outputs {
            let inp = &mut self.nodes[o.0].inputs;
            inp[inp.iter().position(|e| *e == Some(handle)).unwrap()] = None;
        }

        Ok(())
    }

    pub fn connect(
        &mut self,
        port: Port,
        nexus: NexusHandle,
    ) -> Result<Option<NexusHandle>, ConnectError> {
        let nod = self
            .nodes
            .get_mut(port.node().0)
            .ok_or(ConnectError::InvalidNode)?;
        let nex = self
            .nexuses
            .get_mut(nexus.0)
            .ok_or(ConnectError::InvalidNexus)?;
        match port {
            Port::Input { port, node } => {
                nex.outputs.push(node);
                let e = nod.inputs.get_mut(port).ok_or(ConnectError::InvalidPort)?;
                if let Some(e) = e {
                    let n = &mut self.nexuses[e.0];
                    n.outputs
                        .remove(n.outputs.iter().position(|e| *e == node).unwrap());
                }
                Ok(e.replace(nexus))
            }
            Port::Output { port, node } => {
                nex.inputs.push(node);
                let e = nod.outputs.get_mut(port).ok_or(ConnectError::InvalidPort)?;
                if let Some(e) = e {
                    let n = &mut self.nexuses[e.0];
                    n.inputs
                        .remove(n.inputs.iter().position(|e| *e == node).unwrap());
                }
                Ok(e.replace(nexus))
            }
        }
    }

    pub fn disconnect(&mut self, port: Port) -> Result<(), ConnectError> {
        todo!()
    }

    pub fn generate_ir(&self) -> (Vec<ir::IrOp>, usize) {
        let mut ir = Vec::new();
        let mut nexus_visited = core::iter::repeat(false)
            .take(
                self.nexuses
                    .iter()
                    .map(|e| e.0.index() + 1)
                    .max()
                    .unwrap_or(0),
            )
            .collect();
        let mut mem_size = 0;

        for &o in self.outputs.iter() {
            let out = &self.nodes[o];
            assert_eq!(out.inputs.len(), 1);
            for nexus in out.inputs.iter().filter_map(|n| *n) {
                self.gen(&mut ir, nexus, &mut mem_size, &mut nexus_visited);
                let inp = out
                    .inputs
                    .iter()
                    .filter_map(|n| *n)
                    .map(|n| n.0.index())
                    .collect::<Box<_>>();
                let outp = out
                    .outputs
                    .iter()
                    .filter_map(|n| *n)
                    .map(|n| n.0.index())
                    .collect::<Box<_>>();
                mem_size += out
                    .component
                    .generate_ir(&inp, &outp, &mut |op| ir.push(op), mem_size);
            }
        }

        (ir, mem_size)
    }

    pub fn nodes(&self) -> GraphIter<C, Uc> {
        GraphIter {
            iter: self.nodes.iter(),
        }
    }

    pub fn nexus_mut(&mut self, nexus: NexusHandle) -> Option<&mut Nexus<Un>> {
        self.nexuses.get_mut(nexus.0)
    }

    fn gen(
        &self,
        ir: &mut Vec<ir::IrOp>,
        nexus: NexusHandle,
        mem_size: &mut usize,
        nexus_visited: &mut Box<[bool]>,
    ) {
        if nexus_visited[nexus.0.index()] {
            return;
        }
        nexus_visited[nexus.0.index()] = true;
        *mem_size = (*mem_size).max(nexus.0.index() + 1);
        let nexus = &self.nexuses[nexus.0];
        for node in nexus.inputs.iter() {
            let node = &self.nodes[node.0];
            for nexus in node.inputs.iter().filter_map(|n| *n) {
                self.gen(ir, nexus, mem_size, nexus_visited);
            }
            let inp = node
                .inputs
                .iter()
                .filter_map(|n| *n)
                .map(|n| n.0.index())
                .collect::<Box<_>>();
            let outp = node
                .outputs
                .iter()
                .filter_map(|n| *n)
                .map(|n| n.0.index())
                .collect::<Box<_>>();
            *mem_size += node
                .component
                .generate_ir(&inp, &outp, &mut |op| ir.push(op), *mem_size);
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum RemoveError {
    NotConnected,
    InvalidNode,
}

#[derive(Clone, Copy, Debug)]
pub enum RemoveNexusError {
    NotConnected,
    InvalidNexus,
}

#[derive(Clone, Copy, Debug)]
pub enum ConnectError {
    InvalidPort,
    InvalidNode,
    InvalidNexus,
}

pub struct GraphIter<'a, C, Uc>
where
    C: Component,
{
    iter: crate::arena::Iter<'a, Node<C, Uc>>,
}

impl<'a, C, Uc> Iterator for GraphIter<'a, C, Uc>
where
    C: Component,
{
    type Item = (&'a C, GraphNodeHandle, &'a Uc);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(h, n)| (&n.component, GraphNodeHandle(h), &n.userdata))
    }
}

#[cfg(test)]
mod test {
    use super::super::ir::interpreter;
    use super::*;
    use core::num::NonZeroU8;

    /// ```
    /// i0 --+-------v
    ///      |      AND --> NOT
    /// i1 --|--+----^       |
    ///      |  |            v
    ///      +--|----v      AND --> o0
    ///      |  |    OR -----^
    ///      |  +----^
    ///      |  |
    ///      +--|----v
    ///         |   XOR ----------> o1
    ///         +----^
    /// ```
    #[test]
    fn manual_xor() {
        let mut graph = Graph::<Box<dyn Component>, (), ()>::new();

        let bits = NonZeroU8::new(1).unwrap();
        let i0 = graph.add(Box::new(In::new(bits, 0)), ());
        let i1 = graph.add(Box::new(In::new(bits, 1)), ());
        let l0 = graph.add(
            Box::new(AndGate::new(NonZeroOneU8::new(2).unwrap(), bits)),
            (),
        );
        let l1 = graph.add(Box::new(NotGate::new(bits)), ());
        let r0 = graph.add(
            Box::new(OrGate::new(NonZeroOneU8::new(2).unwrap(), bits)),
            (),
        );
        let lr = graph.add(
            Box::new(AndGate::new(NonZeroOneU8::new(2).unwrap(), bits)),
            (),
        );
        let cp = graph.add(
            Box::new(XorGate::new(NonZeroOneU8::new(2).unwrap(), bits)),
            (),
        );
        let o0 = graph.add(Box::new(Out::new(bits, 0)), ());
        let o1 = graph.add(Box::new(Out::new(bits, 1)), ());

        let i0n = graph.new_nexus(());
        let i1n = graph.new_nexus(());
        let l0n = graph.new_nexus(());
        let l1n = graph.new_nexus(());
        let r0n = graph.new_nexus(());
        let lrn = graph.new_nexus(());
        let cpn = graph.new_nexus(());

        graph
            .connect(Port::Output { node: i0, port: 0 }, i0n)
            .unwrap();
        graph
            .connect(Port::Output { node: i1, port: 0 }, i1n)
            .unwrap();

        graph
            .connect(Port::Input { node: l0, port: 0 }, i0n)
            .unwrap();
        graph
            .connect(Port::Input { node: l0, port: 1 }, i1n)
            .unwrap();
        graph
            .connect(Port::Output { node: l0, port: 0 }, l0n)
            .unwrap();
        graph
            .connect(Port::Input { node: r0, port: 0 }, i0n)
            .unwrap();
        graph
            .connect(Port::Input { node: r0, port: 1 }, i1n)
            .unwrap();
        graph
            .connect(Port::Output { node: r0, port: 0 }, r0n)
            .unwrap();
        graph
            .connect(Port::Input { node: l1, port: 0 }, l0n)
            .unwrap();
        graph
            .connect(Port::Output { node: l1, port: 0 }, l1n)
            .unwrap();
        graph
            .connect(Port::Input { node: lr, port: 0 }, l1n)
            .unwrap();
        graph
            .connect(Port::Input { node: lr, port: 1 }, r0n)
            .unwrap();
        graph
            .connect(Port::Output { node: lr, port: 0 }, lrn)
            .unwrap();
        graph
            .connect(Port::Input { node: o0, port: 0 }, lrn)
            .unwrap();

        graph
            .connect(Port::Input { node: cp, port: 0 }, i0n)
            .unwrap();
        graph
            .connect(Port::Input { node: cp, port: 1 }, i1n)
            .unwrap();
        graph
            .connect(Port::Output { node: cp, port: 0 }, cpn)
            .unwrap();
        graph
            .connect(Port::Input { node: o1, port: 0 }, cpn)
            .unwrap();

        let (ir, _) = graph.generate_ir();
        let (a, b) = (0b1100, 0b0110);
        let mut out = [0; 2];
        interpreter::run(&ir, &mut [0; 32], &[a, b], &mut out);
        assert_eq!(out, [a ^ b; 2]);
    }
}
