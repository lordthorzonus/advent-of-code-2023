let file_name = "./inputs/day01.txt"

module Day01 = struct 
    let solve () = 
        let lines = File_utils.read_file_to_list_of_strings file_name in
        String.concat "\n" lines
end 

