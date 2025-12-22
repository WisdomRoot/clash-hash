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

puts "Design setup completed."
puts ""
