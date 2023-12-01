module File_utils = struct
  let read_file_to_list_of_strings file_name =
    let ic = In_channel.open_text file_name in
    let lines = In_channel.input_lines ic in
    In_channel.close ic;

    lines
end
