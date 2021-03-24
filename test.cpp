//
// Created by Michael on 22.03.2021.
//

#include "libjson/json.h"

#include <iostream>

int main(int, const char **) {
    auto parsed = JSON::object(R"(
{
  "key3": "value3",
  "key1": [
    0.01,
    false,
    null
  ],
  "key2": {
    "key3": [
      {
        "a": 2
      },
      {
        "a": 3
      }
    ]
  }
})");

    std::cout << parsed->get_array("key1")->get_float(6) << std::endl;
    return 0;
}