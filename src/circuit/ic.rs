use super::*;
use crate::simulator::ir;
use std::error::Error;
use std::rc::Rc;

#[derive(Clone)]
pub struct Ic {
    ir: Rc<[ir::IrOp]>,
    memory_size: usize,
    inputs: Box<[PointOffset]>,
    outputs: Box<[PointOffset]>,
    pub path: Box<str>,
}

impl Ic {
    pub fn from_circuit<C>(mut circuit: Circuit<C>, path: impl Into<Box<str>>) -> Self
    where
        C: CircuitComponent,
    {
        let (mut inputs, mut outputs) = (Vec::new(), Vec::new());

        for (c, ..) in circuit.components(Aabb::ALL) {
            if let Some(i) = c.external_input() {
                inputs.push(PointOffset::new(-(i as i8 + 1), 0));
            }
            if let Some(o) = c.external_output() {
                outputs.push(PointOffset::new(-(o as i8 + 1), 2));
            }
        }

        let (ir, memory_size) = circuit.generate_ir();

        Self {
            ir: ir.into(),
            memory_size,
            inputs: inputs.into(),
            outputs: outputs.into(),
            path: path.into(),
        }
    }
}

impl Component for Ic {
    fn input_count(&self) -> usize {
        self.inputs.len()
    }

    fn input_type(&self, input: usize) -> Option<InputType> {
        // TODO don't hardcode bits.
        self.inputs.get(input).map(|_| InputType {
            bits: core::num::NonZeroU8::new(1).unwrap(),
        })
    }

    fn output_count(&self) -> usize {
        self.outputs.len()
    }

    fn output_type(&self, output: usize) -> Option<OutputType> {
        // TODO don't hardcode bits.
        self.outputs.get(output).map(|_| OutputType {
            bits: core::num::NonZeroU8::new(1).unwrap(),
        })
    }

    fn generate_ir(
        &self,
        inputs: &[usize],
        outputs: &[usize],
        out: &mut dyn FnMut(ir::IrOp),
        memory_size: usize,
    ) -> usize {
        out(ir::IrOp::RunIc {
            ic: self.ir.clone().into(),
            offset: memory_size,
            inputs: inputs.into(),
            outputs: outputs.into(),
        });
        self.memory_size
    }

	fn properties(&self) -> Box<[Property]> {
		Box::default()
	}

	fn set_property(&mut self, name: &str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		Err("no properties".into())
	}
}

impl CircuitComponent for Ic {
    fn inputs(&self) -> Box<[PointOffset]> {
        self.inputs.clone()
    }

    fn outputs(&self) -> Box<[PointOffset]> {
        self.outputs.clone()
    }

    fn aabb(&self) -> RelativeAabb {
		let (inp, outp) = (self.inputs(), self.outputs());
        let mut iter = inp.iter().chain(outp.iter());
        let s = *iter.next().unwrap();
        let mut aabb = RelativeAabb::new(s, s);
        iter.for_each(|p| aabb = aabb.expand(*p));
        aabb.min.x -= 1;
        aabb.max.x += 1;
        aabb
    }
}
