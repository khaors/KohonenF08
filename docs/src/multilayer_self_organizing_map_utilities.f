!! author: Oscar Garcia-Cabrejo
!! date: 03/05/2025
!! version: 0.1
!! This module defines a class that represents a multilayer self_organized_map 
!! defined using several kohonen layers 
module multilayer_self_organizing_map_utilities
!! This module defines a class that represents a multilayer self_organized_map
use error_handling, only: error_stop, error_t;
use precision_utilities, only: wp;
use constants_utilities, only: NUMCHAR;
use random_generator_base_utilities, only: random_generator_base;
use rkiss05_generator_utilities, only: rkiss05_generator;
use kohonen_layer_parameters_utilities, only: kohonen_layer_parameters;
use kohonen_map_base_utilities, only: kohonen_map_base;
use kohonen_prototype_utilities, only: kohonen_prototype;
use kohonen_pattern_utilities, only: kohonen_pattern;
use distance_base_utilities, only: distance_base;
use factory_distance_utilities, only: factory_distance;
!use influence_function_utilities;
end module multilayer_self_organizing_map_utilities