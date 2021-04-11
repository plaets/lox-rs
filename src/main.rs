use std::convert::TryFrom;
use std::fmt;

type Value = f64;

struct Chunk {
    code: Vec<u8>,
    values: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            values: Vec::new(),
            lines: Vec::new(),
        }
    }

    fn write(&mut self, data: &[u8], line: usize) {
        self.code.extend(data);
        self.lines.push(line);
    }

    fn add_constant(&mut self, value: Value) {
        self.values.push(value)
    }

    fn len(&self) -> usize {
        self.code.len()
    }
}

fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0usize;
    while offset < chunk.len() {
        let instruction = OpCode::from(&chunk.code[offset..]);
        print!("{:04} ", offset);

        if offset > 0 && chunk.lines[offset-1] == chunk.lines[offset-1] {
            print!("   | ");
        } else {
            print!("{:>4} ", chunk.lines[offset]);
        }

        match instruction {
            OpCode::Return => println!("OP_RETURN"),
            OpCode::Constant(offset) => println!("{:<16} {:>4} '{}'", "OP_CONSTANT", offset, chunk.values[offset as usize]),
            OpCode::Unknown(data) => println!("Unknown {}", data),
        }

        offset += instruction.len();
    }
}

enum OpCode {
    Return,
    Constant(u8),
    Unknown(u8),
}

impl From<&[u8]> for OpCode {
    fn from(data: &[u8]) -> Self {
        match data[0] {
            0 => OpCode::Return,
            1 => OpCode::Constant(data[1]), 
            _ => OpCode::Unknown(data[0])
        }
    }
}

impl From<&OpCode> for Vec<u8> {
    fn from(data: &OpCode) -> Vec<u8> {
        match data {
            OpCode::Return => vec![0],
            OpCode::Constant(offset) => vec![1, *offset],
            OpCode::Unknown(data) => vec![*data],
        }
    }
}

impl OpCode {
    fn len(&self) -> usize {
        match self {
            OpCode::Return | OpCode::Unknown(_) => 1,
            OpCode::Constant(_) => 2,
        }
    }
}

fn main() {
    let mut chunk = Chunk::new();
    chunk.add_constant(1.2);
    //couldnt get more verbose
    chunk.write(&Into::<Vec<u8>>::into(&OpCode::Constant(0)), 123);
    chunk.write(&Into::<Vec<u8>>::into(&OpCode::Return), 123);
    disassemble_chunk(&chunk, "test chunk");
}
