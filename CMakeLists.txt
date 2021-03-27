cmake_minimum_required(VERSION 3.17)
project(Json)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
set(CMAKE_EXE_LINKER_FLAGS " -static")

# add_compile_definitions(BUILD_DEBUG)

add_library(Json libjson/json.h)

set_target_properties(Json PROPERTIES LINKER_LANGUAGE CXX)