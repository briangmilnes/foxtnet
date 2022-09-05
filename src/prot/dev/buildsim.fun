(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A functor to build several simulators of ethernet devices.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Build_Simulators

		iii.	RCS Log
	
$Log: buildsim.fun,v $
Revision 1.9  1995/09/25  16:51:20  cline
added Sly_Meter and Snow_Meter

Revision 1.8  1995/06/20  16:48:58  esb
converted to new protocol signature.

Revision 1.7  1995/03/07  23:54:29  esb
updated tracing.

Revision 1.6  1995/01/06  01:34:34  esb
adapted to new Test_Addresses.

Revision 1.5  1994/07/01  02:34:06  danwang
Moved control structures into Fox_Basis.

Revision 1.4  1994/06/16  16:45:05  danwang
Updated to use functorized Fox_Basis

Revision 1.3  1994/06/07  16:31:52  robby
Added a signature

Revision 1.2  94/01/18  04:16:23  esb
major revision.

Revision 1.1  1994/01/17  23:13:56  esb
Initial revision


		1.	functor Build_Simulators
*)


functor Build_Simulators (structure B: FOX_BASIS
			  val xmeter_pathname: string
			  val debug_level: int ref option): SIMULATORS =
 struct
  local
   structure Wire = Wire (structure B = B
			  val debug_level = debug_level)

   val sly_eth = Test_Addresses.name_eth "sly"
   val snow_eth = Test_Addresses.name_eth "snow"
   val quick_eth = Test_Addresses.name_eth "quick"
   val crafty_eth = Test_Addresses.name_eth "crafty"
   val fortran_eth = Test_Addresses.name_eth "fortran"
   val wagosh_eth = Test_Addresses.name_eth "wagosh"
   val cobol_eth = Test_Addresses.name_eth "cobol"
   val basic_eth = Test_Addresses.name_eth "basic"
   val maclisp_eth = Test_Addresses.name_eth "maclisp"
   val vixen_eth = Test_Addresses.name_eth "vixen"

   structure Sly_Raw =
      Ethernet_Device_Simulator (val local_address = sly_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "sly")
   structure Snow_Raw =
      Ethernet_Device_Simulator (val local_address = snow_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "snow")
   structure Quick_Raw =
      Ethernet_Device_Simulator (val local_address = quick_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "quick")
   structure Crafty_Raw =
      Ethernet_Device_Simulator (val local_address = crafty_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "crafty")
   structure Fortran_Raw =
      Ethernet_Device_Simulator (val local_address = fortran_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "fortran")
   structure Wagosh_Raw =
      Ethernet_Device_Simulator (val local_address = wagosh_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "wagosh")
   structure Cobol_Raw =
      Ethernet_Device_Simulator (val local_address = cobol_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "cobol")
   structure Basic_Raw =
      Ethernet_Device_Simulator (val local_address = basic_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "basic")
   structure Maclisp_Raw =
      Ethernet_Device_Simulator (val local_address = maclisp_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "maclisp")
   structure Vixen_Raw =
      Ethernet_Device_Simulator (val local_address = vixen_eth
				 structure Wire = Wire
				 structure B = B
				 val debug_level = debug_level
				 val name = "vixen")
  in (* local *)
   structure Sly = Device_To_Protocol (structure Device = Sly_Raw
				       structure B = B
				       val debug_level = debug_level)
   structure Snow = Device_To_Protocol (structure Device = Snow_Raw
					structure B = B
					val debug_level = debug_level)
   structure Quick = Device_To_Protocol (structure Device = Quick_Raw
					 structure B = B
					 val debug_level = debug_level)
   structure Crafty = Device_To_Protocol (structure Device = Crafty_Raw
					  structure B = B
					  val debug_level = debug_level)
   structure Fortran = Device_To_Protocol (structure Device = Fortran_Raw
					   structure B = B
					   val debug_level = debug_level)
   structure Wagosh = Device_To_Protocol (structure Device = Wagosh_Raw
					  structure B = B
					  val debug_level = debug_level)
   structure Cobol = Device_To_Protocol (structure Device = Cobol_Raw
					 structure B = B
					 val debug_level = debug_level)
   structure Basic = Device_To_Protocol (structure Device = Basic_Raw
					 structure B = B
					 val debug_level = debug_level)
   structure Maclisp = Device_To_Protocol (structure Device = Maclisp_Raw
					   structure B = B
					   val debug_level = debug_level)
   structure Vixen = Device_To_Protocol (structure Device = Vixen_Raw
					 structure B = B
					 val debug_level = debug_level)
   structure Sly_Meter = Device_Meter (structure Lower = Sly
				       val xmeter_pathname = xmeter_pathname
				       val label = "Sly (simulator)"
				       structure B = B
				       val debug_level = debug_level)
   structure Snow_Meter = Device_Meter (structure Lower = Snow
					val xmeter_pathname = xmeter_pathname
					val label = "Snow (simulator)"
					structure B = B
					val debug_level = debug_level)
  end (* local *)
 end (* struct *)



