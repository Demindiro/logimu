use super::*;
use core::mem;

/// A graph of connected components
pub struct Graph<C>
where
	C: Component,
{
	nodes: Vec<Entry<Node<C>>>,
	free: Option<usize>,
	outputs: Vec<usize>,
	inputs: Vec<usize>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GraphNodeHandle(usize);

impl GraphNodeHandle {
	pub fn into_raw(self) -> usize {
		self.0
	}

	pub fn from_raw(raw: usize) -> Self {
		Self(raw)
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

struct Node<C>
where
	C: Component,
{
	component: C,
	inputs: Box<[Vec<Connection>]>,
	outputs: Box<[Vec<Connection>]>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Connection {
	node: usize,
	port: usize,
}

impl<C> Graph<C>
where
	C: Component,
{
	pub fn new() -> Self {
		Self { nodes: Vec::new(), free: None, inputs: Vec::new(), outputs: Vec::new() }
	}

	pub fn add(&mut self, component: C) -> GraphNodeHandle {

		let ic = component.input_count();
		let oc = component.output_count();

		let mut inputs = Vec::new();
		let mut outputs = Vec::new();
		(0..component.input_count()).for_each(|_| inputs.push(Vec::new()));
		(0..component.output_count()).for_each(|_| outputs.push(Vec::new()));
		let node = Node {
			component,
			inputs: inputs.into(),
			outputs: outputs.into(),
		};
		let i = if let Some(free) = self.free {
			if let Some(Entry::Free { next }) = self.nodes.get(free) {
				self.free = *next;
				free
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

	pub fn get(&self, handle: GraphNodeHandle) -> Option<&C> {
		self.nodes.get(handle.0).and_then(|n| n.as_occupied()).map(|n| &n.component)
	}

	pub fn remove(&mut self, component: GraphNodeHandle) -> Result<(), RemoveError> {
		let component = component.0;
		let mut node = if let Some(e) = self.nodes.get_mut(component) {
			match e {
				Entry::Occupied { .. } => {
					let node = mem::replace(e, Entry::Free { next: self.free });
					self.free = Some(component);
					node
				}
				_ => return Err(RemoveError::InvalidNode),
			}
		} else {
			return Err(RemoveError::InvalidNode);
		};
		let node = node.as_occupied_mut().unwrap();

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

		Ok(())
	}

	pub fn connect(&mut self, from: (GraphNodeHandle, usize), to: (GraphNodeHandle, usize)) -> Result<(), ConnectError> {
		let (f, t) = (from.0, to.0);
		let (from, to) = ((from.0.0, from.1), (to.0.0, to.1));
		if from.0 >= self.nodes.len() {
			return Err(ConnectError::InvalidNode(f));
		}
		if to.0 >= self.nodes.len() {
			return Err(ConnectError::InvalidNode(t));
		}
		let (from_node, to_node) = if from.0 < to.0 {
			let (l, r) = self.nodes.split_at_mut(to.0);
			(&mut l[from.0], &mut r[0])
		} else {
			let (l, r) = self.nodes.split_at_mut(from.0);
			(&mut r[0], &mut l[to.0])
		};
		let from_node = from_node.as_occupied_mut().ok_or(ConnectError::InvalidNode(f))?;
		let to_node = to_node.as_occupied_mut().ok_or(ConnectError::InvalidNode(t))?;
		let o = from_node.outputs.get_mut(from.1).ok_or(ConnectError::InvalidPort(f, from.1))?;
		let i = to_node.inputs.get_mut(to.1).ok_or(ConnectError::InvalidPort(t, to.1))?;
		o.push(Connection { node: to.0, port: to.1 });
		i.push(Connection { node: from.0, port: from.1 });
		Ok(())
	}

	pub fn disconnect(&mut self, from: (GraphNodeHandle, usize), to: (GraphNodeHandle, usize)) -> Result<(), ConnectError> {
		let (from, to) = ((from.0.0, from.1), (to.0.0, to.1));
		todo!()
	}

	pub fn generate_ir(&self) -> (Vec<ir::IrOp>, Box<[(GraphNodeHandle, usize)]>, Box<[(GraphNodeHandle, usize)]>, usize) {

		let mut ir = Vec::new();
		let mut node_io_map = core::iter::repeat(None).take(self.nodes.len()).collect();
		let mut outputs = Vec::new();
		let mut mem_size = 0;

		for &o in self.outputs.iter() {
			let out = self.nodes[o].as_occupied().unwrap();
			assert_eq!(out.inputs.len(), 1);
			// FIXME multiple inputs (also deal with short circuits!)
			for con in out.inputs[0].iter() {
				outputs.push((GraphNodeHandle(o), Self::gen(&mut ir, &self.nodes, *con, &mut mem_size, &mut node_io_map)));
			}
		}

		let inputs = self.inputs.iter().filter_map(|&i| node_io_map[i].as_ref().map(|io| (GraphNodeHandle(i), io.1[0]))).collect();

		(ir, inputs, outputs.into(), mem_size)
	}

	fn gen(ir: &mut Vec<ir::IrOp>, nodes: &[Entry<Node<C>>], con: Connection, mem_size: &mut usize, node_io_map: &mut Box<[Option<(Box<[usize]>, Box<[usize]>)>]>) -> usize {
		if let Some(io) = &node_io_map[con.node] {
			return io.1[con.port];
		}
		let node = nodes[con.node].as_occupied().unwrap();
		let mut inp = Vec::new();
		// FIXME deal with short circuits
		if node.inputs.len() > 0 {
			for i in node.inputs.iter() {
				for c in i.iter() {
					inp.push(Self::gen(ir, nodes, *c, mem_size, node_io_map));
				}
			}
		}
		let outp = (*mem_size..*mem_size + node.outputs.len()).collect::<Box<_>>();
		let out = outp[con.port];
		*mem_size += node.outputs.len();
		node.component.generate_ir(&inp, &outp, &mut |op| ir.push(op));
		node_io_map[con.node] = Some((inp.into(), outp));
		out
	}
}

#[derive(Clone, Copy, Debug)]
pub enum RemoveError {
	InvalidNode,
}

#[derive(Clone, Copy, Debug)]
pub enum ConnectError {
	InvalidPort(GraphNodeHandle, usize),
	InvalidNode(GraphNodeHandle),
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
		let mut graph = Graph::<Box<dyn Component>>::new();

		let bits = NonZeroU8::new(1).unwrap();
		let i0 = graph.add(Box::new(In::new(bits)));
		let i1 = graph.add(Box::new(In::new(bits)));
		let l0 = graph.add(Box::new(AndGate::new(NonZeroOneU8::new(2).unwrap(), bits)));
		let l1 = graph.add(Box::new(NotGate::new(bits)));
		let r0 = graph.add(Box::new(OrGate::new(NonZeroOneU8::new(2).unwrap(), bits)));
		let lr = graph.add(Box::new(AndGate::new(NonZeroOneU8::new(2).unwrap(), bits)));
		let cp = graph.add(Box::new(XorGate::new(NonZeroOneU8::new(2).unwrap(), bits)));
		let o0 = graph.add(Box::new(Out::new(bits)));
		let o1 = graph.add(Box::new(Out::new(bits)));

		graph.connect((i0, 0), (l0, 0)).unwrap();
		graph.connect((i1, 0), (l0, 1)).unwrap();
		graph.connect((i0, 0), (r0, 0)).unwrap();
		graph.connect((i1, 0), (r0, 1)).unwrap();
		graph.connect((l0, 0), (l1, 0)).unwrap();
		graph.connect((r0, 0), (lr, 0)).unwrap();
		graph.connect((l1, 0), (lr, 1)).unwrap();
		graph.connect((lr, 0), (o0, 0)).unwrap();

		graph.connect((i0, 0), (cp, 0)).unwrap();
		graph.connect((i1, 0), (cp, 1)).unwrap();
		graph.connect((cp, 0), (o1, 0)).unwrap();

		let (ir, inputs, outputs, mem_size) = graph.generate_ir();

		let (a, b) = (0b1100, 0b0110);
		let mut mem = [0; 32];
		let mem = &mut mem[..mem_size];

		let ai = inputs.iter().find(|v| v.0 == i0).unwrap().1;
		let bi = inputs.iter().find(|v| v.0 == i1).unwrap().1;
		mem[ai] = a;
		mem[bi] = b;

		let xi = outputs.iter().find(|v| v.0 == o0).unwrap().1;
		let yi = outputs.iter().find(|v| v.0 == o1).unwrap().1;

		interpreter::run(&ir, mem);

		assert_eq!(mem[ai], a);
		assert_eq!(mem[bi], b);
		assert_eq!(mem[xi], a ^ b);
		assert_eq!(mem[yi], a ^ b);
	}
}
