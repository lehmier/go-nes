// CPU and Bus stuff
//
package system

type Bus struct {
	cpu CPU
	ram []uint8
	Name string
}

func NewBus() Bus {
	bus := Bus{}
	bus.ram = buildRAM()
	bus.cpu = NewCPU()
	bus.cpu.ConnectBus(&bus)
	bus.Name = "Bus"
	return bus
}

func buildRAM() []uint8 {
 	// Build and reset the ram
	 ram := make([]uint8, 64 * 1024)
	 for i, _ := range ram {
		 ram[i] = 0x00
	 }
	 return ram
}

func (bus Bus) GetCPU() CPU {
	return bus.cpu
}
func (bus Bus) Read(addr uint16, bReadOnly bool) uint8 {
	if addr >= 0x0000 && addr <= 0xFFFF {
		return bus.ram[addr]
	}
	return 0x00
}

func (bus Bus) Write(addr uint16, data uint8) {
	if addr >= 0x0000 && addr <= 0xFFFF {
		bus.ram[addr] = data
	}
}
