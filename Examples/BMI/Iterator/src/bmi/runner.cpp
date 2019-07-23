#include <cstring>
#include <iostream>
#include <fstream>
#include <string>

#include "bmi.h"

static Level log_level = LEVEL_INFO;

CALLCONV void log(Level level, const char* msg) {
    if (level >= log_level) {
        std::cout << "[" << level << "] " << msg << std::endl;
    }
}

int main(int argc, char* argv[]) {
	if (argc < 2) {
		std::cout << "Usage: bmi_it_runner <config.yml> [<log_level>] ";
		return 1;
	}
	if (argc == 3) {
		auto level = argv[2];
		if (!strcmp(level, "debug")) log_level = LEVEL_DEBUG;
		if (!strcmp(level, "info")) log_level = LEVEL_INFO;
		if (!strcmp(level, "warning")) log_level = LEVEL_WARNING;
		if (!strcmp(level, "error")) log_level = LEVEL_ERROR;
	}

	set_logger((Logger) log);

	auto cfg = argv[1];

	int err = 0;
	err = initialize(cfg);
	if (err != 0) {
		log(LEVEL_FATAL, "runner: initialization failed");
		return 2;
	}

	log(LEVEL_INFO, "Calling update");
	err = update(0);
	if (err == 0) log(LEVEL_INFO, "runner - update success");
	if (err != 0) log(LEVEL_WARNING, "runner - update failure");

	err = finalize();
	if (err != 0) {
		log(LEVEL_WARNING, "runner - finalization failed");
		return 2;
	}
}
