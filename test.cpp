//
// Created by Michael on 22.03.2021.
//

#include "libjson/json.h"

#include <iostream>

int main(int, const char **) {
    auto parsed = JSON::object_from_file(R"(F:\Projects\CPPJson\example.json)");

    std::cout << parsed->dump(false);
    return 0;
}