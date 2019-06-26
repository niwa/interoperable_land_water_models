#ifndef BMIT_TARGET_H
#define BMIT_TARGET_H

#include <array>
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
		int update(double dt);
		int finalize();
		void get_start_time(double* t);
		void get_end_time(double* t);
		void get_current_time(double* t);
		void get_time_step(double* dt);
		std::vector<int> get_var_shape(const std::string& name);
	    int get_var_rank(const std::string& name);
	    std::string get_var_type(const std::string& name);
	    int get_var_count();
	    std::string get_var_name(int index);
	    void get_var(const std::string& name, void** ptr);
		void set_var(const std::string& name, const void* ptr);

	private:
		void* m_lib = nullptr;
		void* m_initialize = nullptr;
		void* m_update = nullptr;
		void* m_finalize = nullptr;
		void* m_get_start_time = nullptr;
		void* m_get_end_time = nullptr;
		void* m_get_current_time = nullptr;
		void* m_get_time_step = nullptr;
		void* m_get_var_shape = nullptr;
	    void* m_get_var_rank = nullptr;
	    void* m_get_var_type = nullptr;
	    void* m_get_var_count = nullptr;
	    void* m_get_var_name = nullptr;
	    void* m_get_var = nullptr;
		void* m_set_var = nullptr;
	};

}

#endif // BMIT_TARGET_H
