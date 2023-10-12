#!/bin/bash

# Check if input file is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <input-file>"
    exit 1
fi

input_file="$1"

# Check if input file exists
if [ ! -f "$input_file" ]; then
    echo "Error: File '$input_file' not found!"
    exit 1
fi

# Set initial state to false
extract=false
output_file=""
temp_file=$(mktemp)

# Read input file line by line
while IFS= read -r line; do
    # Check if line contains the marker
    if [[ $line == *'// ----- 8< ----- FILE'* ]]; then
        # Set extract state to true
        extract=true
        # Extract filename from the line
        output_file=$(echo "$line" | grep -o '".*"' | tr -d '"')
        # Check if output file is not empty
        if [ -z "$output_file" ]; then
            echo "Error: Could not extract filename!"
            exit 1
        fi
        # Create/overwrite the output file
        : > "$output_file"
    elif [ "$extract" = true ]; then
        # Write line to output file
        echo "$line" >> "$output_file"
    else
        # Write line to temporary file
        echo "$line" >> "$temp_file"
    fi

    # Reset extract flag if endmodule is encountered
    if [[ $line == "endmodule" ]]; then
        extract=false
    fi
done < "$input_file"

# Replace the original file with the temporary file
mv "$temp_file" "$input_file"