use super::*;
use core::iter;
use core::mem;

/// A graph of connected components
pub struct Graph<C, Uc, Un>
where
	C: Component,
{
	/// All components in this graph.
	nodes: Vec<Entry<Node<C, Uc>>>,
	/// The next free component slot, if any.
	free_node: Option<usize>,
	/// All nexuses in this graph with a list of connected nodes.
	nexuses: Vec<Entry<Nexus<Un>>>,
	/// The next free nexus slot, if any.
	free_nexus: Option<usize>,
	outputs: Vec<usize>,
	inputs: Vec<usize>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct GraphNodeHandle(usize);

impl GraphNodeHandle {
	pub fn into_raw(self) -> usize {
		self.0
	}

	pub fn from_raw(raw: usize) -> Self {
		Self(raw)
	}
}

/// A nexus is a single point connecting multiple ports. It can have only one value at any time.
///
/// It is equivalent to a collection of interconnected wires in a circuit.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NexusHandle(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Port {
	Input  { node: GraphNodeHandle, port: usize },
	Output { node: GraphNodeHandle, port: usize },
}

impl Port {
	pub fn node(self) -> GraphNodeHandle {
		match self {
			Self::Input { node, .. } | Self::Output { node, .. } => node,
		}
	}
}

enum Entry<T> {
	Free { next: Option<usize> },
	Occupied { value: T },
}

impl<T> Entry<T> {
	fn as_occupied(&self) -> Option<&T> {
		match self {
			Self::Occupied { value } => Some(value),
			_ => None,
		}
	}

	fn as_occupied_mut(&mut self) -> Option<&mut T> {
		match self {
			Self::Occupied { value } => Some(value),
			_ => None,
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
			nodes: Vec::new(),
			free_node: None,
			nexuses: Vec::new(),
			free_nexus: None,
			inputs: Vec::new(),
			outputs: Vec::new()
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

		let i = if let Some(free_node) = self.free_node {
			if let Some(Entry::Free { next }) = self.nodes.get(free_node) {
				self.free_node = *next;
				self.nodes[free_node] = Entry::Occupied { value: node };
				free_node
			} else {
				unreachable!()
			}
		} else {
			self.nodes.push(Entry::Occupied { value: node });
			self.nodes.len() - 1
		};
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
		let nexus = Entry::Occupied {
			value: Nexus { inputs: Vec::new(), outputs: Vec::new(), userdata }
		};
		let i = if let Some(free) = self.free_nexus {
			if let Some(Entry::Free { next }) = self.nexuses.get(free) {
				self.free_node = *next;
				self.nexuses[free] = nexus;
				free
			} else {
				unreachable!()
			}
		} else {
			self.nexuses.push(nexus);
			self.nexuses.len() - 1
		};
		NexusHandle(i)
	}

	pub fn get(&self, handle: GraphNodeHandle) -> Option<(&C, &Uc)> {
		self.nodes.get(handle.0).and_then(|n| n.as_occupied()).map(|n| (&n.component, &n.userdata))
	}

	pub fn remove(&mut self, component: GraphNodeHandle) -> Result<(), RemoveError> {
		let component = component.0;
		let mut node = if let Some(e) = self.nodes.get_mut(component) {
			match e {
				Entry::Occupied { .. } => {
					let node = mem::replace(e, Entry::Free { next: self.free_node });
					self.free_node = Some(component);
					node
				}
				_ => return Err(RemoveError::InvalidNode),
			}
		} else {
			return Err(RemoveError::InvalidNode);
		};
		let node = node.as_occupied_mut().unwrap();

		todo!();

		/*
		for (port, conns) in node.inputs.iter_mut().enumerate() {
			let con_in = Connection { node: component, port };
			for con_out in conns {
				let node = self.nodes[con_out.node].as_occupied_mut().unwrap();
				let i = node.outputs[con_out.port].iter().position(|e| *e == con_in).unwrap();
				node.outputs[con_out.port].remove(i);
			}
		}

		for (port, conns) in node.inputs.iter_mut().enumerate() {
			let con_out = Connection { node: component, port };
			for con_in in conns {
				let node = self.nodes[con_in.node].as_occupied_mut().unwrap();
				let i = node.inputs[con_in.port].iter().position(|e| *e == con_out).unwrap();
				node.outputs[con_in.port].remove(i);
			}
		}
		*/

		Ok(())
	}

	pub fn connect(&mut self, port: Port, nexus: NexusHandle) -> Result<Option<NexusHandle>, ConnectError> {
		let nod = self.nodes.get_mut(port.node().0).and_then(Entry::as_occupied_mut).ok_or(ConnectError::InvalidNode)?;
		let nex = self.nexuses.get_mut(nexus.0).and_then(Entry::as_occupied_mut).ok_or(ConnectError::InvalidNexus)?;
		let e = match port {
			Port::Input { port, node } => {
				nex.outputs.push(node);
				nod.inputs.get_mut(port)
			}
			Port::Output { port, node } => {
				nex.inputs.push(node);
				nod.outputs.get_mut(port)
			}
		}.ok_or(ConnectError::InvalidPort)?;
		Ok(e.replace(nexus))
	}

	pub fn disconnect(&mut self, port: Port) -> Result<(), ConnectError> {
		todo!()
	}

	pub fn generate_ir(&self) -> (Vec<ir::IrOp>, usize) {

		let mut ir = Vec::new();
		let mut nexus_visited = core::iter::repeat(false).take(self.nexuses.len()).collect();
		let mut mem_size = 0;

		for &o in self.outputs.iter() {
			let out = self.nodes[o].as_occupied().unwrap();
			assert_eq!(out.inputs.len(), 1);
			for nexus in out.inputs.iter().filter_map(|n| *n) {
				self.gen(&mut ir, nexus, &mut mem_size, &mut nexus_visited);
				let inp  = out.inputs .iter().filter_map(|n| *n).map(|n| n.0).collect::<Box<_>>();
				let outp = out.outputs.iter().filter_map(|n| *n).map(|n| n.0).collect::<Box<_>>();
				out.component.generate_ir(&inp, &outp, &mut |op| ir.push(op));
			}
		}

		(ir, mem_size)
	}

	pub fn nodes(&self) -> GraphIter<C, Uc, Un> {
		GraphIter { graph: self, index: 0 }
	}

	pub fn nexus_mut(&mut self, nexus: NexusHandle) -> Option<&mut Nexus<Un>> {
		self.nexuses.get_mut(nexus.0).and_then(Entry::as_occupied_mut)
	}

	fn gen(&self, ir: &mut Vec<ir::IrOp>, nexus: NexusHandle, mem_size: &mut usize, nexus_visited: &mut Box<[bool]>) {
		if nexus_visited[nexus.0] {
			return;
		}
		nexus_visited[nexus.0] = true;
		*mem_size = (*mem_size).max(nexus.0 + 1);
		let nexus = self.nexuses[nexus.0].as_occupied().unwrap();
		for node in nexus.inputs.iter() {
			let node = self.nodes[node.0].as_occupied().unwrap();
			for nexus in node.inputs.iter().filter_map(|n| *n) {
				self.gen(ir, nexus, mem_size, nexus_visited);
			}
			let inp  = node.inputs .iter().filter_map(|n| *n).map(|n| n.0).collect::<Box<_>>();
			let outp = node.outputs.iter().filter_map(|n| *n).map(|n| n.0).collect::<Box<_>>();
			node.component.generate_ir(&inp, &outp, &mut |op| ir.push(op));
		}
	}
}

#[derive(Clone, Copy, Debug)]
pub enum RemoveError {
	NotConnected,
	InvalidNode,
}

#[derive(Clone, Copy, Debug)]
pub enum ConnectError {
	InvalidPort,
	InvalidNode,
	InvalidNexus,
}

pub struct GraphIter<'a, C, Uc, Un>
where
	C: Component,
{
	graph: &'a Graph<C, Uc, Un>,
	index: usize,
}

impl<'a, C, Uc, Un> Iterator for GraphIter<'a, C, Uc, Un>
where
	C: Component,
{
	type Item = (&'a C, GraphNodeHandle, &'a Uc);

	fn next(&mut self) -> Option<Self::Item> {
		while let Some(node) = self.graph.nodes.get(self.index) {
			let handle = GraphNodeHandle(self.index);
			self.index += 1;
			if let Some(node) = node.as_occupied() {
				return Some((&node.component, handle, &node.userdata));
			}
		}
		None
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use super::super::ir::interpreter;
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
		let l0 = graph.add(Box::new(AndGate::new(NonZeroOneU8::new(2).unwrap(), bits)), ());
		let l1 = graph.add(Box::new(NotGate::new(bits)), ());
		let r0 = graph.add(Box::new(OrGate::new(NonZeroOneU8::new(2).unwrap(), bits)), ());
		let lr = graph.add(Box::new(AndGate::new(NonZeroOneU8::new(2).unwrap(), bits)), ());
		let cp = graph.add(Box::new(XorGate::new(NonZeroOneU8::new(2).unwrap(), bits)), ());
		let o0 = graph.add(Box::new(Out::new(bits, 0)), ());
		let o1 = graph.add(Box::new(Out::new(bits, 1)), ());

		let i0n = graph.new_nexus(());
		let i1n = graph.new_nexus(());
		let l0n = graph.new_nexus(());
		let l1n = graph.new_nexus(());
		let r0n = graph.new_nexus(());
		let lrn = graph.new_nexus(());
		let cpn = graph.new_nexus(());

		graph.connect(Port::Output { node: i0, port: 0 }, i0n).unwrap();
		graph.connect(Port::Output { node: i1, port: 0 }, i1n).unwrap();

		graph.connect(Port::Input  { node: l0, port: 0 }, i0n).unwrap();
		graph.connect(Port::Input  { node: l0, port: 1 }, i1n).unwrap();
		graph.connect(Port::Output { node: l0, port: 0 }, l0n).unwrap();
		graph.connect(Port::Input  { node: r0, port: 0 }, i0n).unwrap();
		graph.connect(Port::Input  { node: r0, port: 1 }, i1n).unwrap();
		graph.connect(Port::Output { node: r0, port: 0 }, r0n).unwrap();
		graph.connect(Port::Input  { node: l1, port: 0 }, l0n).unwrap();
		graph.connect(Port::Output { node: l1, port: 0 }, l1n).unwrap();
		graph.connect(Port::Input  { node: lr, port: 0 }, l1n).unwrap();
		graph.connect(Port::Input  { node: lr, port: 1 }, r0n).unwrap();
		graph.connect(Port::Output { node: lr, port: 0 }, lrn).unwrap();
		graph.connect(Port::Input  { node: o0, port: 0 }, lrn).unwrap();

		graph.connect(Port::Input  { node: cp, port: 0 }, i0n).unwrap();
		graph.connect(Port::Input  { node: cp, port: 1 }, i1n).unwrap();
		graph.connect(Port::Output { node: cp, port: 0 }, cpn).unwrap();
		graph.connect(Port::Input  { node: o1, port: 0 }, cpn).unwrap();

		let (ir, _) = graph.generate_ir();
		let (a, b) = (0b1100, 0b0110);
		let mut out = [0; 2];
		interpreter::run(&ir, &mut [0; 32], &[a, b], &mut out);
		assert_eq!(out, [a ^ b; 2]);
	}
}
