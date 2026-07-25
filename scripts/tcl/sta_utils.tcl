# clash-hash OpenSTA Utility Functions

proc setup_path_groups {input_list output_list path_group_list_name} {
    upvar $path_group_list_name path_group_list

    set flops_in [all_registers -edge_triggered -data_pins]
    set flops_out [all_registers -edge_triggered -clock_pins]

    if {[llength $flops_in] > 0 && [llength $flops_out] > 0} {
        group_path -name reg2reg -from $flops_out -to $flops_in
        lappend path_group_list reg2reg
    }

    if {[llength $output_list] > 0} {
        set outputs_list {}
        foreach output $output_list {
            set output_name [lindex $output 0]
            if {[llength [get_ports $output_name]] > 0} {
                lappend outputs_list [get_ports $output_name]
            }
        }

        if {[llength $outputs_list] > 0} {
            if {[llength $flops_out] > 0} {
                group_path -name reg2out -from $flops_out -to $outputs_list
                lappend path_group_list reg2out
            }
        }
    }

    if {[llength $input_list] > 0} {
        set inputs_list {}
        foreach input $input_list {
            set input_name [lindex $input 0]
            if {[llength [get_ports $input_name]] > 0} {
                lappend inputs_list [get_ports $input_name]
            }
        }

        if {[llength $inputs_list] > 0} {
            if {[llength $flops_in] > 0} {
                group_path -name in2reg -from $inputs_list -to $flops_in
                lappend path_group_list in2reg
            }

            if {[llength $output_list] > 0} {
                set outputs_list {}
                foreach output $output_list {
                    set output_name [lindex $output 0]
                    if {[llength [get_ports $output_name]] > 0} {
                        lappend outputs_list [get_ports $output_name]
                    }
                }

                if {[llength $outputs_list] > 0} {
                    group_path -name in2out -from $inputs_list -to $outputs_list
                    lappend path_group_list in2out
                }
            }
        }
    }
}

proc timing_report {path_group rpt_out path_count} {
    set sta_report_out_filename "${rpt_out}.rpt"
    set sta_csv_out_filename "${rpt_out}.csv.rpt"

    puts "Reporting $path_group to $sta_report_out_filename and $sta_csv_out_filename"
    report_checks -group_path_count $path_count -path_group $path_group > $sta_report_out_filename

    if {[catch {find_timing_paths -group_path_count $path_count -path_group $path_group} paths]} {
        puts "Warning: Could not find timing paths for group $path_group"
        return
    }

    set sta_csv_out [open $sta_csv_out_filename "w"]
    puts $sta_csv_out "startpoint,endpoint,slack"

    foreach path $paths {
        if {[catch {
            set startpoint_name [get_property [get_property $path startpoint] full_name]
            set endpoint_name [get_property [get_property $path endpoint] full_name]
            set slack [get_property $path slack]
            puts $sta_csv_out [format "$startpoint_name,$endpoint_name,%.4f" $slack]
        } error]} {
            puts "Warning: Error processing path in $path_group: $error"
        }
    }

    close $sta_csv_out
}
