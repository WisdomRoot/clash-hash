# clash-hash OpenSTA Common Setup

source [file join [file dirname [info script]] sta_utils.tcl]

# Read environment variables
set liberty_file $::env(STA_LIBERTY_FILE)
set netlist_file $::env(STA_NETLIST_FILE)
set sdc_file $::env(STA_SDC_FILE)
set top_module $::env(STA_TOP_MODULE)
set reports_dir $::env(STA_OUTPUT_DIR_FULL)/reports

puts "clash-hash OpenSTA Analysis"
puts "==========================="
puts "Liberty File: $liberty_file"
puts "Netlist File: $netlist_file"
puts "SDC File: $sdc_file"
puts "Top Module: $top_module"
puts "Reports Dir: $reports_dir"
puts ""

# Read liberty file
puts "Reading liberty file..."
read_liberty $liberty_file

# Read netlist
puts "Reading netlist..."
read_verilog $netlist_file

# Link design
puts "Linking design..."
link_design $top_module

# Read timing constraints
puts "Reading timing constraints..."
if {[file exists $sdc_file]} {
    read_sdc -echo $sdc_file
} else {
    puts "Warning: SDC file not found: $sdc_file"
    puts "Continuing with default constraints..."
}

# Check if this is a pure combinational design
set flops_count [llength [all_registers -edge_triggered]]
set is_combinational [expr {$flops_count == 0}]

if {$is_combinational} {
    puts ""
    puts "==================================================================="
    puts "PURE COMBINATIONAL DESIGN DETECTED"
    puts "==================================================================="
    puts "No registers found in design. This is a pure combinational module."
    puts "Adding input/output delay constraints for in2out path analysis..."
    puts ""

    # Get all clocks (should have at least one from SDC)
    set clocks [all_clocks]
    if {[llength $clocks] > 0} {
        set main_clock [lindex $clocks 0]
        set clock_name [get_property $main_clock name]
        puts "Using clock '$clock_name' for I/O delay constraints"

        # Set input delays to 0 (data available at clock edge)
        set input_ports [all_inputs]
        foreach port $input_ports {
            set port_name [get_property $port full_name]
            # Skip clock ports
            if {![string match "*CLK*" $port_name] && ![string match "*clk*" $port_name]} {
                set_input_delay 0.0 -clock $clock_name $port
            }
        }

        # Set output delays to 0 (data should be ready by end of cycle)
        set output_ports [all_outputs]
        foreach port $output_ports {
            set_output_delay 0.0 -clock $clock_name $port
        }

        puts "Input/output delay constraints added for combinational analysis"
    } else {
        puts "Warning: No clocks found, cannot add I/O delay constraints"
    }
    puts "==================================================================="
    puts ""
}

puts "Design setup completed."
puts ""
