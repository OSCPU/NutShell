set device xczu19eg-ffvc1760-2-i

set script_dir  [file dirname [info script]]

# Add files for system top
set src_files [list \
  "[file normalize "${script_dir}/rtl/system_top.v"]" \
  "[file normalize "${script_dir}/rtl/addr_mapper.v"]" \
]

# Add files for constraint
set xdc_files [list \
  "[file normalize "${script_dir}/constr/pcie.xdc"]" \
]

source ${script_dir}/../common.tcl
