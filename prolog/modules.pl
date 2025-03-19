% module(ModuleName, Difficulty, Prerequisites, Field)
module(introduction_to_programming, 0.1, [], general).
module(introduction_to_computer_science, 0.1, [], general).
module(web_development, 0.1, [introduction_to_programming, introduction_to_computer_science], web_dev).
module(intro_to_data_structures_algorithms, 0.2, [introduction_to_programming, introduction_to_computer_science], general).
module(object_oriented_programming, 0.2, [introduction_to_programming, introduction_to_computer_science], general).
module(intro_to_ai, 0.2, [introduction_to_computer_science], ai).
module(maths_for_computer_science, 0.3, [], general).
module(computer_architecture, 0.3, [introduction_to_computer_science], general).
module(software_engineering, 0.3, [introduction_to_programming], general).
module(computer_networks, 0.3, [introduction_to_computer_science], networking).
module(advanced_algorithms, 0.4, [intro_to_data_structures_algorithms], general).
module(databases_and_sql, 0.4, [introduction_to_programming, introduction_to_computer_science], data).
module(cloud_computing, 0.4, [computer_networks, databases_and_sql], networking).
module(theory_of_computation, 0.5, [maths_for_computer_science], general).
module(operating_systems, 0.5, [computer_architecture, software_engineering], general).
module(big_data, 0.5, [cloud_computing, computer_networks], data).
module(data_science, 0.5, [software_engineering, maths_for_computer_science], data).
module(embedded_programming, 0.6, [operating_systems, software_engineering], embedded).
module(machine_learning, 0.6, [intro_to_ai], ai).
module(cybersecurity, 0.6, [software_engineering, maths_for_computer_science], cybersecurity).
module(parallel_and_distributed_systems, 0.7, [computer_architecture, software_engineering, computer_networks], networking).
module(compilers_and_languages, 0.7, [theory_of_computation, software_engineering], general).
module(penetration_testing, 0.7, [cybersecurity], cybersecurity).
module(safety_critical_programming, 0.8, [software_engineering, embedded_programming, compilers_and_languages], embedded).
module(advanced_networks, 0.8, [computer_networks], networking).
module(advanced_programming_paradigms, 0.8, [software_engineering, compilers_and_languages], general).
module(computational_finance, 0.9, [data_science, machine_learning], data).
module(real_time_operating_systems, 0.9, [operating_systems, embedded_programming, cybersecurity, penetration_testing], embedded).
module(quantum_computing, 1.0, [advanced_programming_paradigms, computer_architecture, theory_of_computation], experimental).