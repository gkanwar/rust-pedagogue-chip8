use clap::{App, Arg};
use pixels::{Error, Pixels, SurfaceTexture};
use std::fs::File;
use std::io::Read;
use std::{io, panic, thread, time};
use winit::{
  dpi::LogicalSize,
  event::{Event, WindowEvent},
  event_loop::{ControlFlow, EventLoop},
};

type Addr = usize;
type RegIdx = u8;
const MEM_SIZE: usize = 0x1000;
const NUM_GP_REGS: usize = 16;
const FLAG_REG: usize = NUM_GP_REGS - 1;
const STACK_SIZE: usize = 16;
const ENTRY_ADDR: Addr = 0x200;
const DISPLAY_WIDTH: usize = 64;
const DISPLAY_HEIGHT: usize = 32;
const DISPLAY_PIXELS: usize = DISPLAY_WIDTH * DISPLAY_HEIGHT;
const SPRITE_HEIGHT: usize = 5;
const SPRITE_ADDR: usize = 0;

#[derive(Debug)]
enum OpCode {
  _CallMachine(Addr), // Unsupported
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
  let addr: Addr = (code & 0xfff) as Addr;
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
    }
    _ => {
      panic!("Invalid N0");
    }
  };
}

struct MachineState {
  memory: [u8; MEM_SIZE],
  vx: [u8; NUM_GP_REGS],
  i: Addr,
  // TODO: timers
  pc: Addr,
  sp: usize,
  stack: [Addr; STACK_SIZE],
  display: [u8; DISPLAY_PIXELS],
}

fn get_next_pc(pc: Addr) -> Addr {
  let next_pc = pc + 2;
  assert!(next_pc < MEM_SIZE);
  return next_pc;
}

fn is_valid_pc(addr: Addr) -> bool {
  addr % 2 == 0 && addr < MEM_SIZE && addr >= ENTRY_ADDR
}

fn _debug_print_display(state: &MachineState) {
  for i in 0..DISPLAY_HEIGHT {
    for j in 0..DISPLAY_WIDTH {
      let ind = i * DISPLAY_WIDTH + j;
      let pix = state.display[ind as usize];
      print!("{}", if pix == 1 { "#" } else { " " });
    }
    println!();
  }
}

fn step_machine(state: &mut MachineState) -> bool {
  assert!(state.pc % 2 == 0);
  assert!((state.pc as usize) < MEM_SIZE);

  let pc: usize = state.pc as usize;
  let code: u16 = u16::from_be_bytes([state.memory[pc], state.memory[pc + 1]]);
  // TODO: better error handling
  let opcode: OpCode = parse_opcode(code).expect(&format!("Unknown opcode {:04x}", code));
  println!("STEP: opcode {:04x} {:?}", code, opcode);
  let mut jumped: bool = false;
  let mut drawn: bool = false;
  match opcode {
    OpCode::ClearDisplay => {
      state.display = [0; DISPLAY_PIXELS];
      drawn = true;
    }
    OpCode::Jump(addr) => {
      state.pc = addr;
      assert!(is_valid_pc(addr));
      jumped = true;
    }
    OpCode::JumpOff(addr) => {
      state.pc = addr + (state.vx[0] as usize);
      assert!(is_valid_pc(addr));
      jumped = true;
    }
    OpCode::Call(addr) => {
      if state.sp as usize == STACK_SIZE - 1 {
        panic!("Stack overflow");
      }
      state.stack[state.sp] = get_next_pc(state.pc);
      state.sp += 1;
      assert!(is_valid_pc(addr));
      state.pc = addr;
      jumped = true;
    }
    OpCode::Return => {
      if state.sp == 0 {
        panic!("Return from non-existant subroutine");
      }
      state.sp -= 1;
      state.pc = state.stack[state.sp];
      jumped = true;
    }
    OpCode::AssignImm(reg, imm) => {
      state.vx[reg as usize] = imm;
    }
    OpCode::Assign(r1, r2) => {
      state.vx[r1 as usize] = state.vx[r2 as usize];
    }
    OpCode::BitOr(r1, r2) => {
      state.vx[r1 as usize] |= state.vx[r2 as usize];
    }
    OpCode::BitAnd(r1, r2) => {
      state.vx[r1 as usize] &= state.vx[r2 as usize];
    }
    OpCode::BitXor(r1, r2) => {
      state.vx[r1 as usize] ^= state.vx[r2 as usize];
    }
    OpCode::Pointer(addr) => {
      state.i = addr;
    }
    OpCode::Draw(rx, ry, n_byte) => {
      let x0 = state.vx[rx as usize] as usize;
      let y0 = state.vx[ry as usize] as usize;
      let n = n_byte as usize;
      let sprite = &state.memory[state.i..state.i + n];
      let mut collision: bool = false;
      for i in 0..n {
        let ymod = (y0 + i) % DISPLAY_HEIGHT;
        let row = sprite[i];
        for j in 0..8 {
          let xmod = (x0 + j) % DISPLAY_WIDTH;
          let ind = ymod * DISPLAY_WIDTH + xmod;
          let bit = (row >> (8 - j - 1)) & 1;
          if bit == 1 && state.display[ind] > 0 {
            collision = true;
          }
          state.display[ind] ^= bit;
        }
      }
      //_debug_print_display(state);
      state.vx[FLAG_REG] = collision as u8;
      drawn = true;
    }
    OpCode::IfEqImm(reg, imm) => {
      if state.vx[reg as usize] == imm {
        state.pc = get_next_pc(state.pc);
      }
    }
    OpCode::IfNeqImm(reg, imm) => {
      if state.vx[reg as usize] != imm {
        state.pc = get_next_pc(state.pc);
      }
    }
    OpCode::IfEq(r1, r2) => {
      if state.vx[r1 as usize] == state.vx[r2 as usize] {
        state.pc = get_next_pc(state.pc);
      }
    }
    OpCode::IfNeq(r1, r2) => {
      if state.vx[r1 as usize] != state.vx[r2 as usize] {
        state.pc = get_next_pc(state.pc);
      }
    }
    OpCode::AddImm(reg, imm) => {
      state.vx[reg as usize] = state.vx[reg as usize].wrapping_add(imm);
    }
    OpCode::Add(r1, r2) => {
      let v1: u8 = state.vx[r1 as usize];
      let v2: u8 = state.vx[r2 as usize];
      let (new_v1, overflow) = v1.overflowing_add(v2);
      state.vx[r1 as usize] = new_v1;
      state.vx[FLAG_REG] = overflow as u8;
    }
    OpCode::Sub(r1, r2) => {
      let v1: u8 = state.vx[r1 as usize];
      let v2: u8 = state.vx[r2 as usize];
      let (new_v1, overflow) = v1.overflowing_sub(v2);
      state.vx[r1 as usize] = new_v1;
      state.vx[FLAG_REG] = overflow as u8;
    }
    OpCode::SubNeg(r1, r2) => {
      let v1: u8 = state.vx[r1 as usize];
      let v2: u8 = state.vx[r2 as usize];
      let (new_v1, overflow) = v2.overflowing_sub(v1);
      state.vx[r1 as usize] = new_v1;
      state.vx[FLAG_REG] = overflow as u8;
    }
    OpCode::ShiftL(reg) => {
      state.vx[FLAG_REG] = state.vx[reg as usize] >> 7;
      state.vx[reg as usize] <<= 1;
    }
    OpCode::ShiftR(reg) => {
      state.vx[FLAG_REG] = state.vx[reg as usize] & 1;
      state.vx[reg as usize] >>= 1;
    }
    OpCode::LoadSprite(reg) => {
      let v = state.vx[reg as usize];
      let sprite_addr = SPRITE_ADDR + (v as usize)*SPRITE_HEIGHT;
      state.i = sprite_addr;
    }
    OpCode::StoreBCD(reg) => {
      let v = state.vx[reg as usize];
      let d0: u8 = (v / 100) % 10;
      let d1: u8 = (v / 10) % 10;
      let d2: u8 = v % 10;
      state.memory[state.i] = d0;
      state.memory[state.i+1] = d1;
      state.memory[state.i+2] = d2;
    }
    OpCode::RegDump(reg) => {
      for i in 0..reg+1 {
        let off: usize = i as usize;
        state.memory[state.i + off] = state.vx[off];
      }
    }
    OpCode::RegLoad(reg) => {
      for i in 0..reg+1 {
        let off: usize = i as usize;
        state.vx[off] = state.memory[state.i + off];
      }
    }
    _ => panic!("Unsupported opcode"),
  }
  if !jumped {
    state.pc = get_next_pc(state.pc);
  }
  assert!(is_valid_pc(state.pc));
  return drawn;
}

fn load_rom(rom_path: &str, state: &mut MachineState) -> Result<(), io::Error> {
  let mut f = File::open(&rom_path)?;
  f.read(&mut state.memory[(ENTRY_ADDR as usize)..])?;
  return Ok(());
}

fn create_window(
  title: &str, w: u32, h: u32,  scale: f64, event_loop: &EventLoop<()>
) -> winit::window::Window {
  let window = winit::window::WindowBuilder::new()
    .with_title(title)
    .with_resizable(false)
    .build(&event_loop)
    .unwrap();
  let size = LogicalSize::new((w as f64) * scale, (h as f64) * scale);
  window.set_inner_size(size);
  return window;
}

fn draw_display(state: &MachineState, screen: &mut [u8]) {
  for (c, pix) in state.display.iter().zip(screen.chunks_exact_mut(4)) {
    let color: [u8; 4] = if *c == 1 {
      [0xff, 0xff, 0xff, 0xff]
    } else {
      [0, 0, 0, 0xff]
    };
    pix.copy_from_slice(&color);
  }
}

fn init_sprites(state: &mut MachineState) {
  const SPRITE_DATA: [u8; 16*SPRITE_HEIGHT] = [
    0xf0, 0x90, 0x90, 0x90, 0x90,
    0x20, 0x60, 0x20, 0x20, 0x70,
    0xf0, 0x10, 0xf0, 0x80, 0xf0,
    0xf0, 0x10, 0xf0, 0x10, 0xf0,
    0x90, 0x90, 0xf0, 0x10, 0x10,
    0xf0, 0x80, 0xf0, 0x10, 0xf0,
    0xf0, 0x80, 0xf0, 0x90, 0xf0,
    0xf0, 0x10, 0x20, 0x40, 0x40,
    0xf0, 0x90, 0xf0, 0x90, 0xf0,
    0xf0, 0x90, 0xf0, 0x10, 0xf0,
    0xf0, 0x90, 0xf0, 0x90, 0x90,
    0xe0, 0x90, 0xe0, 0x90, 0xe0,
    0xf0, 0x80, 0x80, 0x80, 0xf0,
    0xe0, 0x90, 0x90, 0x90, 0xe0,
    0xf0, 0x80, 0xf0, 0x80, 0xf0,
    0xf0, 0x80, 0xf0, 0x80, 0x80
  ];
  const SPRITE_START: usize = SPRITE_ADDR;
  const SPRITE_END: usize = SPRITE_ADDR + 16 * SPRITE_HEIGHT;
  state.memory[SPRITE_START .. SPRITE_END].copy_from_slice(&SPRITE_DATA);
}

fn main() -> Result<(), Error> {
  let matches = App::new("rust-pedagogue-chip8")
    .about("CHIP-8 Emulator")
    .arg(
      Arg::with_name("rom_path")
        .long("rom_path")
        .value_name("rom_path")
        .required(true),
    )
    .get_matches();
  let rom_path = matches.value_of("rom_path").unwrap();

  let mut state = MachineState {
    memory: [0; MEM_SIZE],
    vx: [0; NUM_GP_REGS],
    i: 0,
    pc: ENTRY_ADDR,
    sp: 0,
    stack: [0; STACK_SIZE],
    display: [0; DISPLAY_PIXELS],
  };
  init_sprites(&mut state);
  println!("Loading ROM from {}", rom_path);
  load_rom(rom_path, &mut state).expect("Failed to load ROM");

  let event_loop = EventLoop::new();
  const WINDOW_WIDTH: u32 = DISPLAY_WIDTH as u32;
  const WINDOW_HEIGHT: u32 = DISPLAY_HEIGHT as u32;
  let scale: f64 = 10.0;
  let window = create_window("CHIP-8 Emulator", WINDOW_WIDTH, WINDOW_HEIGHT, scale, &event_loop);
  let window_size = window.inner_size();
  let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
  let mut pixels = Pixels::new(WINDOW_WIDTH, WINDOW_HEIGHT, surface_texture)?;

  event_loop.run(move |event, _, control_flow| {
    *control_flow = ControlFlow::Poll;
    println!("event: {:?}", event);
    match event {
      Event::RedrawRequested(_) => {
        println!("Redrawing");
        draw_display(&state, pixels.get_frame());
        if pixels.render().map_err(|e| {println!("err {}", e)}).is_err() {
          *control_flow = ControlFlow::Exit;
          return;
        }
      },
      Event::WindowEvent { event: WindowEvent::CloseRequested, .. } => {
        *control_flow = ControlFlow::Exit;
        return;
      },
      Event::MainEventsCleared => {
        // thread::sleep(time::Duration::from_millis(30));
        let drawn = step_machine(&mut state);
        if drawn {
          println!("Requesting redraw");
          window.request_redraw();
        }
      },
      _ => ()
    };
  });
}
