cmake_minimum_required(VERSION 3.17)
project(Json)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
set(CMAKE_EXE_LINKER_FLAGS " -static")

# add_compile_definitions(BUILD_DEBUG)

#add_library(Json)
add_executable(JsonTest libjson/json.h test.cpp)

target_link_libraries(JsonTest -static-libgcc -static-libstdc++)
#target_link_libraries(JsonTest Json)
