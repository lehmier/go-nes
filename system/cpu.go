package system

import (
	"reflect"
)

// Test function for tests
func Sum(x int, y int) int {
	return x + y
}

const (
	FLAG_C = 1 << iota // Carry Bit
	FLAG_Z = 1 << iota // Zero
	FLAG_I = 1 << iota // Disable Interrupts
	FLAG_D = 1 << iota // Decimal Mode (unused in this implementation)
	FLAG_B = 1 << iota // Break
	FLAG_U = 1 << iota // Unused
	FLAG_V = 1 << iota // Overflow
	FLAG_N = 1 << iota // Negative
)

type Instruction struct {
	name     string
	operate  func() uint8 //Operate
	addrmode func() uint8 // AddressMode
	cycles   uint8
}

type CPU struct {
	bus *Bus

	// Registers
	A      uint8  // Accumulator Register
	X      uint8  // X Register
	Y      uint8  // Y Register
	Stkp   uint8  // Stack Pointer (points to location on bus)
	Pc     uint16 // Program Counter
	Status uint8  // Status Register

	// Assistive variables to facilitate emulation
	fetched     uint8  // Represents the working input value to the ALU
	temp        uint16 // A convenience variable used everywhere
	addr_abs    uint16 // All used memory addresses end up in here
	addr_rel    uint16 // Represents absolute address following a branch
	opcode      uint8  // Is the instruction byte
	cycles      uint8  // Counts how many cycles the instruction has remaining
	clock_count uint32 // A global accumulation of the number of clocks

	instructions []Instruction
}

func NewCPU() CPU {
	cpu := CPU{}

	// Public registers
	cpu.A = 0x00
	cpu.X = 0x00
	cpu.Y = 0x00
	cpu.Stkp = 0x00
	cpu.Pc = 0x0000
	cpu.Status = 0x00

	// Internal vars
	cpu.fetched = 0x00
	cpu.temp = 0x0000
	cpu.addr_abs = 0x0000
	cpu.addr_rel = 0x00
	cpu.opcode = 0x00
	cpu.cycles = 0
	cpu.clock_count = 0
	cpu.instructions = []Instruction{
		Instruction{"BRK", cpu.BRK, cpu.IMM, 7}, Instruction{"ORA", cpu.ORA, cpu.IZX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"???", cpu.NOP, cpu.IMP, 3}, Instruction{"ORA", cpu.ORA, cpu.ZP0, 3}, Instruction{"ASL", cpu.ASL, cpu.ZP0, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 5}, Instruction{"PHP", cpu.PHP, cpu.IMP, 3}, Instruction{"ORA", cpu.ORA, cpu.IMM, 2}, Instruction{"ASL", cpu.ASL, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"ORA", cpu.ORA, cpu.ABS, 4}, Instruction{"ASL", cpu.ASL, cpu.ABS, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6},
		Instruction{"BPL", cpu.BPL, cpu.REL, 2}, Instruction{"ORA", cpu.ORA, cpu.IZY, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"ORA", cpu.ORA, cpu.ZPX, 4}, Instruction{"ASL", cpu.ASL, cpu.ZPX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6}, Instruction{"CLC", cpu.CLC, cpu.IMP, 2}, Instruction{"ORA", cpu.ORA, cpu.ABY, 4}, Instruction{"???", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 7}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"ORA", cpu.ORA, cpu.ABX, 4}, Instruction{"ASL", cpu.ASL, cpu.ABX, 7}, Instruction{"???", cpu.XXX, cpu.IMP, 7},
		Instruction{"JSR", cpu.JSR, cpu.ABS, 6}, Instruction{"AND", cpu.AND, cpu.IZX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"BIT", cpu.BIT, cpu.ZP0, 3}, Instruction{"AND", cpu.AND, cpu.ZP0, 3}, Instruction{"ROL", cpu.ROL, cpu.ZP0, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 5}, Instruction{"PLP", cpu.PLP, cpu.IMP, 4}, Instruction{"AND", cpu.AND, cpu.IMM, 2}, Instruction{"ROL", cpu.ROL, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"BIT", cpu.BIT, cpu.ABS, 4}, Instruction{"AND", cpu.AND, cpu.ABS, 4}, Instruction{"ROL", cpu.ROL, cpu.ABS, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6},
		Instruction{"BMI", cpu.BMI, cpu.REL, 2}, Instruction{"AND", cpu.AND, cpu.IZY, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"AND", cpu.AND, cpu.ZPX, 4}, Instruction{"ROL", cpu.ROL, cpu.ZPX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6}, Instruction{"SEC", cpu.SEC, cpu.IMP, 2}, Instruction{"AND", cpu.AND, cpu.ABY, 4}, Instruction{"???", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 7}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"AND", cpu.AND, cpu.ABX, 4}, Instruction{"ROL", cpu.ROL, cpu.ABX, 7}, Instruction{"???", cpu.XXX, cpu.IMP, 7},
		Instruction{"RTI", cpu.RTI, cpu.IMP, 6}, Instruction{"EOR", cpu.EOR, cpu.IZX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"???", cpu.NOP, cpu.IMP, 3}, Instruction{"EOR", cpu.EOR, cpu.ZP0, 3}, Instruction{"LSR", cpu.LSR, cpu.ZP0, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 5}, Instruction{"PHA", cpu.PHA, cpu.IMP, 3}, Instruction{"EOR", cpu.EOR, cpu.IMM, 2}, Instruction{"LSR", cpu.LSR, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"JMP", cpu.JMP, cpu.ABS, 3}, Instruction{"EOR", cpu.EOR, cpu.ABS, 4}, Instruction{"LSR", cpu.LSR, cpu.ABS, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6},
		Instruction{"BVC", cpu.BVC, cpu.REL, 2}, Instruction{"EOR", cpu.EOR, cpu.IZY, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"EOR", cpu.EOR, cpu.ZPX, 4}, Instruction{"LSR", cpu.LSR, cpu.ZPX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6}, Instruction{"CLI", cpu.CLI, cpu.IMP, 2}, Instruction{"EOR", cpu.EOR, cpu.ABY, 4}, Instruction{"???", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 7}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"EOR", cpu.EOR, cpu.ABX, 4}, Instruction{"LSR", cpu.LSR, cpu.ABX, 7}, Instruction{"???", cpu.XXX, cpu.IMP, 7},
		Instruction{"RTS", cpu.RTS, cpu.IMP, 6}, Instruction{"ADC", cpu.ADC, cpu.IZX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"???", cpu.NOP, cpu.IMP, 3}, Instruction{"ADC", cpu.ADC, cpu.ZP0, 3}, Instruction{"ROR", cpu.ROR, cpu.ZP0, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 5}, Instruction{"PLA", cpu.PLA, cpu.IMP, 4}, Instruction{"ADC", cpu.ADC, cpu.IMM, 2}, Instruction{"ROR", cpu.ROR, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"JMP", cpu.JMP, cpu.IND, 5}, Instruction{"ADC", cpu.ADC, cpu.ABS, 4}, Instruction{"ROR", cpu.ROR, cpu.ABS, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6},
		Instruction{"BVS", cpu.BVS, cpu.REL, 2}, Instruction{"ADC", cpu.ADC, cpu.IZY, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"ADC", cpu.ADC, cpu.ZPX, 4}, Instruction{"ROR", cpu.ROR, cpu.ZPX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6}, Instruction{"SEI", cpu.SEI, cpu.IMP, 2}, Instruction{"ADC", cpu.ADC, cpu.ABY, 4}, Instruction{"???", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 7}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"ADC", cpu.ADC, cpu.ABX, 4}, Instruction{"ROR", cpu.ROR, cpu.ABX, 7}, Instruction{"???", cpu.XXX, cpu.IMP, 7},
		Instruction{"???", cpu.NOP, cpu.IMP, 2}, Instruction{"STA", cpu.STA, cpu.IZX, 6}, Instruction{"???", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 6}, Instruction{"STY", cpu.STY, cpu.ZP0, 3}, Instruction{"STA", cpu.STA, cpu.ZP0, 3}, Instruction{"STX", cpu.STX, cpu.ZP0, 3}, Instruction{"???", cpu.XXX, cpu.IMP, 3}, Instruction{"DEY", cpu.DEY, cpu.IMP, 2}, Instruction{"???", cpu.NOP, cpu.IMP, 2}, Instruction{"TXA", cpu.TXA, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"STY", cpu.STY, cpu.ABS, 4}, Instruction{"STA", cpu.STA, cpu.ABS, 4}, Instruction{"STX", cpu.STX, cpu.ABS, 4}, Instruction{"???", cpu.XXX, cpu.IMP, 4},
		Instruction{"BCC", cpu.BCC, cpu.REL, 2}, Instruction{"STA", cpu.STA, cpu.IZY, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 6}, Instruction{"STY", cpu.STY, cpu.ZPX, 4}, Instruction{"STA", cpu.STA, cpu.ZPX, 4}, Instruction{"STX", cpu.STX, cpu.ZPY, 4}, Instruction{"???", cpu.XXX, cpu.IMP, 4}, Instruction{"TYA", cpu.TYA, cpu.IMP, 2}, Instruction{"STA", cpu.STA, cpu.ABY, 5}, Instruction{"TXS", cpu.TXS, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 5}, Instruction{"???", cpu.NOP, cpu.IMP, 5}, Instruction{"STA", cpu.STA, cpu.ABX, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 5},
		Instruction{"LDY", cpu.LDY, cpu.IMM, 2}, Instruction{"LDA", cpu.LDA, cpu.IZX, 6}, Instruction{"LDX", cpu.LDX, cpu.IMM, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 6}, Instruction{"LDY", cpu.LDY, cpu.ZP0, 3}, Instruction{"LDA", cpu.LDA, cpu.ZP0, 3}, Instruction{"LDX", cpu.LDX, cpu.ZP0, 3}, Instruction{"???", cpu.XXX, cpu.IMP, 3}, Instruction{"TAY", cpu.TAY, cpu.IMP, 2}, Instruction{"LDA", cpu.LDA, cpu.IMM, 2}, Instruction{"TAX", cpu.TAX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"LDY", cpu.LDY, cpu.ABS, 4}, Instruction{"LDA", cpu.LDA, cpu.ABS, 4}, Instruction{"LDX", cpu.LDX, cpu.ABS, 4}, Instruction{"???", cpu.XXX, cpu.IMP, 4},
		Instruction{"BCS", cpu.BCS, cpu.REL, 2}, Instruction{"LDA", cpu.LDA, cpu.IZY, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 5}, Instruction{"LDY", cpu.LDY, cpu.ZPX, 4}, Instruction{"LDA", cpu.LDA, cpu.ZPX, 4}, Instruction{"LDX", cpu.LDX, cpu.ZPY, 4}, Instruction{"???", cpu.XXX, cpu.IMP, 4}, Instruction{"CLV", cpu.CLV, cpu.IMP, 2}, Instruction{"LDA", cpu.LDA, cpu.ABY, 4}, Instruction{"TSX", cpu.TSX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 4}, Instruction{"LDY", cpu.LDY, cpu.ABX, 4}, Instruction{"LDA", cpu.LDA, cpu.ABX, 4}, Instruction{"LDX", cpu.LDX, cpu.ABY, 4}, Instruction{"???", cpu.XXX, cpu.IMP, 4},
		Instruction{"CPY", cpu.CPY, cpu.IMM, 2}, Instruction{"CMP", cpu.CMP, cpu.IZX, 6}, Instruction{"???", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"CPY", cpu.CPY, cpu.ZP0, 3}, Instruction{"CMP", cpu.CMP, cpu.ZP0, 3}, Instruction{"DEC", cpu.DEC, cpu.ZP0, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 5}, Instruction{"INY", cpu.INY, cpu.IMP, 2}, Instruction{"CMP", cpu.CMP, cpu.IMM, 2}, Instruction{"DEX", cpu.DEX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"CPY", cpu.CPY, cpu.ABS, 4}, Instruction{"CMP", cpu.CMP, cpu.ABS, 4}, Instruction{"DEC", cpu.DEC, cpu.ABS, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6},
		Instruction{"BNE", cpu.BNE, cpu.REL, 2}, Instruction{"CMP", cpu.CMP, cpu.IZY, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"CMP", cpu.CMP, cpu.ZPX, 4}, Instruction{"DEC", cpu.DEC, cpu.ZPX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6}, Instruction{"CLD", cpu.CLD, cpu.IMP, 2}, Instruction{"CMP", cpu.CMP, cpu.ABY, 4}, Instruction{"NOP", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 7}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"CMP", cpu.CMP, cpu.ABX, 4}, Instruction{"DEC", cpu.DEC, cpu.ABX, 7}, Instruction{"???", cpu.XXX, cpu.IMP, 7},
		Instruction{"CPX", cpu.CPX, cpu.IMM, 2}, Instruction{"SBC", cpu.SBC, cpu.IZX, 6}, Instruction{"???", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"CPX", cpu.CPX, cpu.ZP0, 3}, Instruction{"SBC", cpu.SBC, cpu.ZP0, 3}, Instruction{"INC", cpu.INC, cpu.ZP0, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 5}, Instruction{"INX", cpu.INX, cpu.IMP, 2}, Instruction{"SBC", cpu.SBC, cpu.IMM, 2}, Instruction{"NOP", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.SBC, cpu.IMP, 2}, Instruction{"CPX", cpu.CPX, cpu.ABS, 4}, Instruction{"SBC", cpu.SBC, cpu.ABS, 4}, Instruction{"INC", cpu.INC, cpu.ABS, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6},
		Instruction{"BEQ", cpu.BEQ, cpu.REL, 2}, Instruction{"SBC", cpu.SBC, cpu.IZY, 5}, Instruction{"???", cpu.XXX, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 8}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"SBC", cpu.SBC, cpu.ZPX, 4}, Instruction{"INC", cpu.INC, cpu.ZPX, 6}, Instruction{"???", cpu.XXX, cpu.IMP, 6}, Instruction{"SED", cpu.SED, cpu.IMP, 2}, Instruction{"SBC", cpu.SBC, cpu.ABY, 4}, Instruction{"NOP", cpu.NOP, cpu.IMP, 2}, Instruction{"???", cpu.XXX, cpu.IMP, 7}, Instruction{"???", cpu.NOP, cpu.IMP, 4}, Instruction{"SBC", cpu.SBC, cpu.ABX, 4}, Instruction{"INC", cpu.INC, cpu.ABX, 7}, Instruction{"???", cpu.XXX, cpu.IMP, 7}}
	return cpu
}

/**
 * Public Methods
 */
func (cpu *CPU) Reset() {

}

func (cpu *CPU) Irq() {

}

func (cpu *CPU) Nmi() {

}

func (cpu *CPU) Clock() {
	if cpu.cycles == 0 {
		cpu.opcode = cpu.read(cpu.Pc)
		cpu.Pc++

		cpu.cycles = cpu.instructions[cpu.opcode].cycles
		additional_cycle1 := cpu.instructions[cpu.opcode].addrmode()
		additional_cycle2 := cpu.instructions[cpu.opcode].operate()
		cpu.cycles += (additional_cycle1 & additional_cycle2)

	}
	cpu.cycles--
}

func (cpu *CPU) Complete() bool {
	return false
}

func (cpu *CPU) ConnectBus(bus *Bus) {
	cpu.bus = bus
}

/**
 * Private internal methods
 *
 */
func (cpu CPU) read(addr uint16) uint8 {
	return cpu.bus.Read(addr, false)
}

func (cpu CPU) write(addr uint16, data uint8) {
	cpu.bus.Write(addr, data)
}

func (cpu *CPU) GetFlag(f uint8) uint8 {
	if cpu.Status&f > 0 {
		return 1
	} else {
		return 0
	}
}

// Sets or clears a specific bit of the Status register
func (cpu *CPU) SetFlag(f uint8, v bool) {
	if v {
		cpu.Status |= f
	} else {
		cpu.Status &= ^f
	}
}

/*****************/
/*			 	 */
/* Address Modes */
/*				 */
/*****************/
func (cpu *CPU) IMP() uint8 {
	cpu.fetched = cpu.A

	return 0
}
func (cpu *CPU) IMM() uint8 {
	cpu.addr_abs = cpu.Pc
	cpu.Pc++
	return 0
}
func (cpu *CPU) ZP0() uint8 {
	cpu.addr_abs = uint16(cpu.read(cpu.Pc))
	cpu.Pc++
	cpu.addr_abs &= 0x00FF
	return 0
}
func (cpu *CPU) ZPX() uint8 {
	cpu.addr_abs = uint16(cpu.read(cpu.Pc) + cpu.X)
	cpu.Pc++
	cpu.addr_abs &= 0x00FF
	return 0
}
func (cpu *CPU) ZPY() uint8 {
	cpu.addr_abs = uint16(cpu.read(cpu.Pc) + cpu.Y)
	cpu.Pc++
	cpu.addr_abs &= 0x00FF
	return 0
}
func (cpu *CPU) REL() uint8 {
	cpu.addr_rel = uint16(cpu.read(cpu.Pc))
	cpu.Pc++
	if cpu.addr_rel&0x80 > 0x00 {
		cpu.addr_rel |= 0xFF00
	}
	return 0
}
func (cpu *CPU) ABS() uint8 {
	lo := uint16(cpu.read(cpu.Pc))
	cpu.Pc++
	hi := uint16(cpu.read(cpu.Pc))
	cpu.Pc++

	cpu.addr_abs = (hi << 8) | lo

	return 0
}
func (cpu *CPU) ABX() uint8 {
	lo := uint16(cpu.read(cpu.Pc))
	cpu.Pc++
	hi := uint16(cpu.read(cpu.Pc))
	cpu.Pc++

	cpu.addr_abs = (hi << 8) | lo
	cpu.addr_abs += uint16(cpu.X)

	if (cpu.addr_abs & 0xFF00) != (hi << 8) {
		return 1
	} else {
		return 0
	}
}
func (cpu *CPU) ABY() uint8 {
	lo := uint16(cpu.read(cpu.Pc))
	cpu.Pc++
	hi := uint16(cpu.read(cpu.Pc))
	cpu.Pc++

	cpu.addr_abs = (hi << 8) | lo
	cpu.addr_abs += uint16(cpu.Y)

	if (cpu.addr_abs & 0xFF00) != (hi << 8) {
		return 1
	} else {
		return 0
	}
}
func (cpu *CPU) IND() uint8 {
	ptr_lo := uint16(cpu.read(cpu.Pc))
	cpu.Pc++
	ptr_hi := uint16(cpu.read(cpu.Pc))
	cpu.Pc++

	ptr := (ptr_hi << 8) | ptr_lo

	if ptr_lo == 0x00FF {
		cpu.addr_abs = uint16((cpu.read(ptr&0xFF00) << 8) | cpu.read(ptr+0))
	} else {
		cpu.addr_abs = uint16((cpu.read(ptr+1) << 8) | cpu.read(ptr+0))
	}

	return 0
}
func (cpu *CPU) IZX() uint8 {
	t := uint16(cpu.read(cpu.Pc))
	cpu.Pc++

	lo := uint16(cpu.read(uint16((t + uint16(cpu.X))) & 0x00FF))
	hi := uint16(cpu.read(uint16((t + uint16(cpu.X) + 1)) & 0x00FF))

	cpu.addr_abs = (hi << 8) | lo
	return 0
}
func (cpu *CPU) IZY() uint8 {
	t := uint16(cpu.read(cpu.Pc))
	cpu.Pc++

	lo := uint16(cpu.read(t & 0x00FF))
	hi := uint16(cpu.read((t + 1) & 0x00FF))

	cpu.addr_abs = (hi << 8) | lo
	cpu.addr_abs += uint16(cpu.Y)

	if (cpu.addr_abs & 0xFF00) != (hi << 8) {
		return 1
	} else {
		return 0
	}
}

// This function sources the data used by the instruction into
// a convenient numeric variable. Some instructions dont have to
// fetch data as the source is implied by the instruction. For example
// "INX" increments the X register. There is no additional data
// required. For all other addressing modes, the data resides at
// the location held within addr_abs, so it is read from there.
// Immediate adress mode exploits this slightly, as that has
// set addr_abs = pc + 1, so it fetches the data from the
// next byte for example "LDA $FF" just loads the accumulator with
// 256, i.e. no far reaching memory fetch is required. "fetched"
// is a variable global to the CPU, and is set by calling this
// function. It also returns it for convenience.
func (cpu *CPU) Fetch() uint8 {
	if reflect.ValueOf(cpu.instructions[cpu.opcode].addrmode).Pointer() != reflect.ValueOf(cpu.IMP).Pointer() {
		cpu.fetched = cpu.read(cpu.addr_abs)
	}
	return cpu.fetched
}

/*************/
/*			 */
/* Operators */
/*			 */
/*************/
func (cpu *CPU) ADC() uint8 {
	return 15
}
func (cpu *CPU) AND() uint8 {
	return 15
}
func (cpu *CPU) ASL() uint8 {
	return 15
}
func (cpu *CPU) BCC() uint8 {
	return 15
}
func (cpu *CPU) BCS() uint8 {
	return 15
}
func (cpu *CPU) BEQ() uint8 {
	return 15
}
func (cpu *CPU) BIT() uint8 {
	return 15
}
func (cpu *CPU) BMI() uint8 {
	return 15
}
func (cpu *CPU) BNE() uint8 {
	return 15
}
func (cpu *CPU) BPL() uint8 {
	return 15
}
func (cpu *CPU) BRK() uint8 {
	return 15
}
func (cpu *CPU) BVC() uint8 {
	return 15
}
func (cpu *CPU) BVS() uint8 {
	return 15
}
func (cpu *CPU) CLC() uint8 {
	return 15
}
func (cpu *CPU) CLD() uint8 {
	return 15
}
func (cpu *CPU) CLI() uint8 {
	return 15
}
func (cpu *CPU) CLV() uint8 {
	return 15
}
func (cpu *CPU) CMP() uint8 {
	return 15
}
func (cpu *CPU) CPX() uint8 {
	return 15
}
func (cpu *CPU) CPY() uint8 {
	return 15
}
func (cpu *CPU) DEC() uint8 {
	return 15
}
func (cpu *CPU) DEX() uint8 {
	return 15
}
func (cpu *CPU) DEY() uint8 {
	return 15
}
func (cpu *CPU) EOR() uint8 {
	return 15
}
func (cpu *CPU) INC() uint8 {
	return 15
}
func (cpu *CPU) INX() uint8 {
	return 15
}
func (cpu *CPU) INY() uint8 {
	return 15
}
func (cpu *CPU) JMP() uint8 {
	return 15
}
func (cpu *CPU) JSR() uint8 {
	return 15
}
func (cpu *CPU) LDA() uint8 {
	return 15
}
func (cpu *CPU) LDX() uint8 {
	return 15
}
func (cpu *CPU) LDY() uint8 {
	return 15
}
func (cpu *CPU) LSR() uint8 {
	return 15
}
func (cpu *CPU) NOP() uint8 {
	return 15
}
func (cpu *CPU) ORA() uint8 {
	return 15
}
func (cpu *CPU) PHA() uint8 {
	return 15
}
func (cpu *CPU) PHP() uint8 {
	return 15
}
func (cpu *CPU) PLA() uint8 {
	return 15
}
func (cpu *CPU) PLP() uint8 {
	return 15
}
func (cpu *CPU) ROL() uint8 {
	return 15
}
func (cpu *CPU) ROR() uint8 {
	return 15
}
func (cpu *CPU) RTI() uint8 {
	return 15
}
func (cpu *CPU) RTS() uint8 {
	return 15
}
func (cpu *CPU) SBC() uint8 {
	return 15
}
func (cpu *CPU) SEC() uint8 {
	return 15
}
func (cpu *CPU) SED() uint8 {
	return 15
}
func (cpu *CPU) SEI() uint8 {
	return 15
}
func (cpu *CPU) STA() uint8 {
	return 15
}
func (cpu *CPU) STX() uint8 {
	return 15
}
func (cpu *CPU) STY() uint8 {
	return 15
}
func (cpu *CPU) TAX() uint8 {
	return 15
}
func (cpu *CPU) TAY() uint8 {
	return 15
}
func (cpu *CPU) TSX() uint8 {
	return 15
}
func (cpu *CPU) TXA() uint8 {
	return 15
}
func (cpu *CPU) TXS() uint8 {
	return 15
}
func (cpu *CPU) TYA() uint8 {
	return 15
}
func (cpu *CPU) XXX() uint8 {
	return 15
}

func (cpu *CPU) DoWork() uint8 {
	return cpu.instructions[0].operate()
}
