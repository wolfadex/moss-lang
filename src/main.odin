package main

import "core:c"
import "core:c/libc"
import "core:fmt"
import "core:log"
import "core:math"
import "core:mem"
import os1 "core:os"
import os "core:os/os2"
import "core:slice"
import "core:strings"
import "core:sys/posix"
import "core:unicode"
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

	orig_termios: posix.termios

	raw_termios := enable_raw_mode(&orig_termios)
	defer disable_raw_mode(&orig_termios)
	defer free(raw_termios)

	input := make([]u8, 1)
	defer delete(input)

	for {
		len_read, err := os.read(os.stdin, input)
		c := utf8.string_to_runes(string(input), allocator = context.temp_allocator)[0]

		if err != nil {
			break
		}

		if c == 'q' {
			break
		}

		if unicode.is_control(c) {
			fmt.printf("%d\r\n", c)
		} else {
			fmt.printf("%d ('%c')\r\n", c, c)
		}
	}
}

enable_raw_mode :: proc(orig_termios: ^posix.termios) -> (raw: ^posix.termios) {
	posix.tcgetattr(posix.STDIN_FILENO, orig_termios)

	raw = new_clone(orig_termios^)
	raw.c_iflag -= {.IXON, .BRKINT, .ICRNL, .INPCK, .ISTRIP}
	raw.c_oflag -= {.OPOST}
	raw.c_cflag += {.CS8}
	raw.c_lflag -= {.ECHO, .ICANON, .ISIG, .IEXTEN}

	posix.tcsetattr(posix.STDIN_FILENO, .TCSAFLUSH, raw)

	return
}

disable_raw_mode :: proc(orig_termios: ^posix.termios) {
	posix.tcsetattr(posix.STDIN_FILENO, .TCSAFLUSH, orig_termios)
}
