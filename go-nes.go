package main

import (
	"fmt"
	"lehmier.com/projects/go-nes/system"
)

func main() {
	bus := system.NewBus()
	fmt.Println(bus.Name)

	// bus.Write(0x00, 45)
	// bus.Write(362, 23)
	// fmt.Println(bus.Read(0x16A, true))

	cpu := bus.GetCPU()
	// fmt.Println(cpu.Read(0x16A, true))

	//fmt.Println(system.FLAG_N)
	//fmt.Println(cpu.DoWork())

	fmt.Println(cpu.GetFlag(system.FLAG_N))
	cpu.SetFlag(system.FLAG_N, true)
	fmt.Println(cpu.GetFlag(system.FLAG_N))
	cpu.SetFlag(system.FLAG_N, false)
	fmt.Println(cpu.GetFlag(system.FLAG_N))

	fmt.Println(cpu.Fetch())

}