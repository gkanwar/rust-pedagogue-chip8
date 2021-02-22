type Addr = u16;
type RegIdx = u8;
const MEM_SIZE: usize = 0x1000;
const NUM_GP_REGS: usize = 16;
const STACK_SIZE: usize = 16;
const ENTRY_ADDR: Addr = 0x200;

enum OpCode {
  CallMachine(Addr),
  ClearDisplay,
  Return,
  Jump(Addr),
  Call(Addr),
  IfEqImm(RegIdx, u8),
  IfNeqImm(RegIdx, u8),
  IfEq(RegIdx, RegIdx),
  AssignImm(RegIdx, u8),
  AddImm(RegIdx, u8),
  Assign(RegIdx, RegIdx),
  BitOr(RegIdx, RegIdx),
  BitAnd(RegIdx, RegIdx),
  BitXor(RegIdx, RegIdx),
  Add(RegIdx, RegIdx),
  Sub(RegIdx, RegIdx),
  ShiftR(RegIdx),
  SubNeg(RegIdx, RegIdx),
  ShiftL(RegIdx),
  IfNeq(RegIdx, RegIdx),
  Pointer(Addr),
  JumpOff(Addr),
  Rand(RegIdx, u8),
  Draw(RegIdx, RegIdx, u8),
  IfKeyEq(RegIdx),
  IfKeyNeq(RegIdx),
  GetDelay(RegIdx),
  GetKey(RegIdx),
  SetDelayTimer(RegIdx),
  SetSoundTimer(RegIdx),
  AddPointer(RegIdx),
  LoadSprite(RegIdx),
  StoreBCD(RegIdx),
  RegDump(RegIdx),
  RegLoad(RegIdx),
}

fn parse_opcode(code: u16) -> Option<OpCode> {
  let N0 = (code >> 12) & 0xf;
  let N1 = (code >> 8) & 0xf;
  let N1b: u8 = N1 as u8;
  let N2 = (code >> 4) & 0xf;
  let N2b: u8 = N2 as u8;
  let N3 = code & 0xf;
  let nib: u8 = (code & 0xf) as u8;
  let imm: u8 = (code & 0xff) as u8;
  let addr: Addr = code & 0xfff;
  return match N0 {
    0 => {
      if N1 == 0 && N2 == 0xE && N3 == 0 {
        Some(OpCode::ClearDisplay)
      } else if N1 == 0 && N2 == 0xE && N3 == 0xE {
        Some(OpCode::Return)
      } else {
        None // ignore jump to machine code
      }
    }
    1 => Some(OpCode::Jump(addr)),
    2 => Some(OpCode::Call(addr)),
    3 => Some(OpCode::IfEqImm(N1b, imm)),
    4 => Some(OpCode::IfNeqImm(N1b, imm)),
    5 => {
      if N3 == 0 {
        Some(OpCode::IfEq(N1b, N2b))
      } else {
        None
      }
    }
    6 => Some(OpCode::AssignImm(N1b, imm)),
    7 => Some(OpCode::AddImm(N1b, imm)),
    8 => match N3 {
      0 => Some(OpCode::Assign(N1b, N2b)),
      1 => Some(OpCode::BitOr(N1b, N2b)),
      2 => Some(OpCode::BitAnd(N1b, N2b)),
      3 => Some(OpCode::BitXor(N1b, N2b)),
      4 => Some(OpCode::Add(N1b, N2b)),
      5 => Some(OpCode::Sub(N1b, N2b)),
      6 => Some(OpCode::ShiftR(N1b)),
      7 => Some(OpCode::SubNeg(N1b, N2b)),
      0xe => Some(OpCode::ShiftL(N1b)),
      _ => None,
    },
    9 => {
      if N3 == 0 {
        Some(OpCode::IfNeq(N1b, N2b))
      } else {
        None
      }
    }
    0xa => Some(OpCode::Pointer(addr)),
    0xb => Some(OpCode::JumpOff(addr)),
    0xc => Some(OpCode::Rand(N1b, imm)),
    0xd => Some(OpCode::Draw(N1b, N2b, nib)),
    0xe => {
      if N2 == 9 && N3 == 0xe {
        Some(OpCode::IfKeyEq(N1b))
      } else if N2 == 0xa && N3 == 0x1 {
        Some(OpCode::IfKeyNeq(N1b))
      } else {
        None
      }
    }
    0xf => {
      if N2 == 0 && N3 == 7 {
        Some(OpCode::GetDelay(N1b))
      } else if N2 == 0 && N3 == 0xa {
        Some(OpCode::GetKey(N1b))
      } else if N2 == 1 && N3 == 5 {
        Some(OpCode::SetDelayTimer(N1b))
      } else if N2 == 1 && N3 == 8 {
        Some(OpCode::SetSoundTimer(N1b))
      } else if N2 == 1 && N3 == 0xe {
        Some(OpCode::AddPointer(N1b))
      } else if N2 == 2 && N3 == 9 {
        Some(OpCode::LoadSprite(N1b))
      } else if N2 == 3 && N3 == 3 {
        Some(OpCode::StoreBCD(N1b))
      } else if N2 == 5 && N3 == 5 {
        Some(OpCode::RegDump(N1b))
      } else if N2 == 6 && N3 == 5 {
        Some(OpCode::RegLoad(N1b))
      } else {
        None
      }
    },
    _ => {
      panic!("Invalid N0");
    }
  };
}

struct MachineState {
  memory: [u8; MEM_SIZE],
  vx: [u8; NUM_GP_REGS],
  i: u16,
  // TODO: timers
  pc: Addr,
  sp: usize,
  stack: [u16; STACK_SIZE], // TODO: display
}

fn get_next_pc(pc: Addr) -> Addr {
  let next_pc = pc + 2;
  assert!((next_pc as usize) < MEM_SIZE);
  return next_pc;
}

fn step_machine(state: &mut MachineState) -> () {
  assert!(state.pc % 2 == 0);
  assert!((state.pc as usize) < MEM_SIZE);

  let pc: usize = state.pc as usize;
  let code: u16 = u16::from_be_bytes([
    state.memory[pc],
    state.memory[pc+1]]);
  // TODO: better error handling
  let opcode: OpCode = parse_opcode(code).unwrap();
  match opcode {
    OpCode::Jump(addr) => {
      state.pc = addr;
    },
    OpCode::Call(addr) => {
      if state.sp as usize == STACK_SIZE-1 {
        panic!("Stack overflow");
      }
      state.stack[state.sp] = get_next_pc(state.pc);
      state.sp += 1;
      if addr % 2 != 0 || (addr as usize) >= MEM_SIZE || addr < ENTRY_ADDR {
        panic!("Invalid jump address");
      }
      state.pc = addr;
    },
    _ => panic!("Unsupported opcode")
  }
}

fn main() {
  println!("Hello, world!");
  let mut state = MachineState {
    memory: [0; MEM_SIZE],
    vx: [0; NUM_GP_REGS],
    i: 0,
    pc: ENTRY_ADDR,
    sp: 0,
    stack: [0; STACK_SIZE]
  };
  loop {
    step_machine(&mut state);
  }
}
