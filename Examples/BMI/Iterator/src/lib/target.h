#ifndef BMIT_TARGET_H
#define BMIT_TARGET_H

#include <string>
#include <vector>

namespace bmit {

	class Target {
	public:
		explicit Target(const std::string &lib_path);
		Target(const Target &) = delete;
		Target(Target &&) noexcept = delete;
		Target& operator=(const Target &) = delete;
		Target& operator=(Target &&) noexcept = delete;
		~Target();

		int initialize(const std::string &cfg_path);
		int update();
		int finalize();
		void get_start_time(double* t);
		void get_end_time(double* t);
		void get_current_time(double* t);
		void get_time_step(double* dt);
		int get_input_var_name_count();
		int get_output_var_name_count();
		std::vector<std::string> get_input_var_names();
		std::vector<std::string> get_output_var_names();
		std::string get_var_type(const std::string& name);
		std::string get_var_units(const std::string& name);
		int get_var_itemsize(const std::string& name);
		int get_var_rank(const std::string& name);
		void get_var_size(const char* name, int* size);
		void get_var_nbytes(const char* name, int* nbytes);
		void get_value(const std::string& name, void* valptr);
		void set_value(const std::string& name, void* valptr);

	private:
		void* m_lib = nullptr;
		void* m_initialize = nullptr;
		void* m_update = nullptr;
		void* m_finalize = nullptr;
		void* m_get_start_time = nullptr;
		void* m_get_end_time = nullptr;
		void* m_get_current_time = nullptr;
		void* m_get_time_step = nullptr;
		void* m_get_input_var_name_count = nullptr;
		void* m_get_output_var_name_count = nullptr;
		void* m_get_input_var_names = nullptr;
		void* m_get_output_var_names = nullptr;
		void* m_get_var_units = nullptr;
		void* m_get_var_type = nullptr;
		void* m_get_var_itemsize = nullptr;
		void* m_get_var_rank = nullptr;
		void* m_get_var_size = nullptr;
		void* m_get_var_nbytes = nullptr;
		void* m_get_value = nullptr;
		void* m_set_value = nullptr;
	};

}

#endif // BMIT_TARGET_H
