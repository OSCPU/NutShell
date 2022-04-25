set device xc7z020-1-clg400
set board tul.com.tw:pynq-z2:part0:1.0

set script_dir  [file dirname [info script]]

# Add files for system top
set src_files [list \
]

# Add files for constraint
#set xdc_files [list \
#  "[file normalize "${script_dir}/constr/constr.xdc"]" \
#  "[file normalize "${script_dir}/constr/vga.xdc"]" \
#]

source ${script_dir}/../common.tcl
