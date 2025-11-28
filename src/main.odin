package main

import "core:c"
import "core:fmt"
import "core:log"
import "core:math"
import "core:mem"
import os1 "core:os"
import os "core:os/os2"
import "core:slice"
import "core:strings"
import "core:unicode/utf8"

main :: proc() {
	when ODIN_DEBUG {
		// setup debug logging
		log_file, err := os1.open("./logs.txt", mode = os1.O_WRONLY)
		if err != nil {
			os.exit(1)
		}
		logger := log.create_file_logger(log_file)
		context.logger = logger

		// setup tracking allocator for making sure all memory is cleaned up
		default_allocator := context.allocator
		tracking_allocator: mem.Tracking_Allocator
		mem.tracking_allocator_init(&tracking_allocator, default_allocator)
		context.allocator = mem.tracking_allocator(&tracking_allocator)

		reset_tracking_allocator :: proc(a: ^mem.Tracking_Allocator) -> bool {
			err := false

			for _, value in a.allocation_map {
				fmt.printfln("%v: Leaked %v bytes", value.location, value.size)
				err = true
			}

			mem.tracking_allocator_clear(a)

			return err
		}

		defer reset_tracking_allocator(&tracking_allocator)
	}

	enable_raw_mode()

	input := make([]u8, 1)
	defer delete(input)

	for {
		len_read, err := os.read(os.stdin, input)

		log.debug(err)
		log.debug(len_read)
		if err != nil {
			break
		}

		if input[0] == 'q' {
			break
		}
	}
}

enable_raw_mode :: proc() {
	termios: rawptr
}
