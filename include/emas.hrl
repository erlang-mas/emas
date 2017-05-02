-record(sim_params, {genetic_ops             :: atom(),
                     problem_size            :: pos_integer(),
                     initial_energy          :: integer(),
                     reproduction_threshold  :: integer(),
                     migration_threshold     :: integer(),
                     reproduction_transfer   :: integer(),
                     fight_transfer          :: integer(),
                     mutation_rate           :: float(),
                     mutation_range          :: float(),
                     mutation_chance         :: float(),
                     recombination_chance    :: float(),
                     migration_probability   :: float(),
                     fight_number            :: pos_integer(),
                     extra                   :: term()}).

-type agent()        :: any().
-type sim_params()   :: #sim_params{}.
-type solution()     :: any().
