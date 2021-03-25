//
// Created by Michael on 22.03.2021.
//

//TODO: Add file exporting
//TODO: Add Beautifying

#pragma once

#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <streambuf>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

class JSON {
private:
    class Parser;
    class Lexer;

    class Dumpable;

    template<typename T>
    using Shared = std::shared_ptr<T>;

    template<typename T>
    using Unique = std::unique_ptr<T>;

    template<typename T, typename... Args>
    static constexpr Shared<T> create(Args &&...args) {
        return std::make_shared<T>(std::forward<Args>(args)...);
    }

    template<typename T, typename... Args>
    static constexpr Unique<T> create_scoped(Args &&...args) {
        return std::make_unique<T>(std::forward<Args>(args)...);
    }

public:
    class Object;
    class Array;
    class Literal;

    [[nodiscard]] static Shared<Object> object() { return create<Object>(); }

    [[nodiscard]] static Shared<Array> array() { return create<Array>(); }
    [[nodiscard]] static Shared<Array> array(const std::vector<Shared<Dumpable>> &values) {
        auto array = create<Array>();
        for (auto &value : values) { array->insert(value); }
        return array;
    }

    [[nodiscard]] static Shared<Literal> literal() { return create<Literal>(); }
    [[nodiscard]] static Shared<Literal> literal(bool value) { return create<Literal>((bool) value); }
    [[nodiscard]] static Shared<Literal> literal(int value) { return create<Literal>((int) value); }
    [[nodiscard]] static Shared<Literal> literal(float value) { return create<Literal>((float) value); }
    [[nodiscard]] static Shared<Literal> literal(const char *value) { return create<Literal>((const char *) value); }

    [[nodiscard]] static Shared<Object> object(const std::string &json) {
        auto lexer = create<Lexer>(json);
        auto parser = create<Parser>(lexer);

        return parser->parse_obj();
    }

    [[nodiscard]] static Shared<Array> array(const std::string &json) {
        auto lexer = create<Lexer>(json);
        auto parser = create<Parser>(lexer);

        return parser->parse_arr();
    }

    [[nodiscard]] static Shared<Object> object_from_file(const std::string &file_path) {
        std::ifstream t(file_path);
        std::string str;

        t.seekg(0, std::ios::end);
        str.reserve(t.tellg());
        t.seekg(0, std::ios::beg);

        str.assign((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());

        return object(str);
    }

    [[nodiscard]] static Shared<Array> array_from_file(const std::string &file_path) {
        std::ifstream t(file_path);
        std::string str;

        t.seekg(0, std::ios::end);
        str.reserve(t.tellg());
        t.seekg(0, std::ios::beg);

        str.assign((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());

        return array(str);
    }

    static void set_indentation_chars(const std::string &indentation) { m_indententation_chars = indentation; }

    static const std::string &get_indentation_chars() { return m_indententation_chars; }

private:
    inline static std::string m_indententation_chars{"  "};// NOLINT(cert-err58-cpp)

    [[nodiscard]] static std::string repeat(uint32_t amount, const std::string &string) {
        std::stringstream result;
        for (uint32_t i = 0; i < amount; i++) result << string;
        return result.str();
    }

    class Dumpable {
    public:
        virtual ~Dumpable() = default;

        virtual std::string dump(bool beautify) = 0;

        void dump_to_file(const std::string &path, bool beatify = false) {
            std::ofstream file;
            file.open(path);
            file << dump(beatify);
            file.close();
        }

    protected:
        inline static uint32_t m_indent_level = 0;
    };

    class Token {
    public:
        enum class Type {
            Null,
            Bool,
            Int,
            String,

            Dot,
            Minus,
            Plus,
            Exponent,
            CurlyOpen,
            CurlyClose,
            SquareOpen,
            SquareClose,
            Comma,
            Colon,
            Whitespace,
            EndOfFile,
            Invalid,
        };

        Token() : m_token_type(Type::Invalid) {}
        ~Token() = default;

        explicit Token(Type type) : m_token_type(type) {}
        Token(Type type, std::string raw) : m_token_type(type), m_raw(std::move(raw)) {}

        bool operator==(const Token &rhs) const { return m_token_type == rhs.m_token_type; }
        bool operator==(const Token::Type &rhs) const { return m_token_type == rhs; }

        bool operator!=(const Token &rhs) const { return m_token_type != rhs.m_token_type; }
        bool operator!=(const Token::Type &rhs) const { return m_token_type != rhs; }

        static std::string to_string(Token::Type type) {
            std::stringstream string;

            string << "Token <";

            switch (type) {
                case Type::Null: string << "Null"; break;
                case Type::Int: string << "Int"; break;
                case Type::Bool: string << "Bool"; break;
                case Type::String: string << "String"; break;
                case Type::CurlyOpen: string << "CurlyOpen"; break;
                case Type::CurlyClose: string << "CurlyClose"; break;
                case Type::SquareOpen: string << "SquareOpen"; break;
                case Type::SquareClose: string << "SquareClose"; break;
                case Type::Comma: string << "Comma"; break;
                case Type::Colon: string << "Colon"; break;
                case Type::Whitespace: string << "Whitespace"; break;
                case Type::EndOfFile: string << "EndOfFile"; break;
                case Type::Invalid: string << "Invalid"; break;
                case Type::Minus: string << "Minus"; break;
                case Type::Plus: string << "Plus"; break;
                case Type::Dot: string << "Dot"; break;
                case Type::Exponent: string << "Exponent"; break;
                default: string << "Unknown"; break;
            }
            string << ">";
            return string.str();
        }

        [[nodiscard]] Type token_type() const { return m_token_type; }
        [[maybe_unused]] [[nodiscard]] const std::string &raw() const { return m_raw; }

    private:
        std::string m_raw;
        Type m_token_type;
    };
    class Lexer {
    public:
        explicit Lexer(const std::string &json_raw) {
            m_json_size = json_raw.size();
            m_json_raw = new char[m_json_size + 1];
            json_raw.copy(m_json_raw, m_json_size);
            m_json_raw[m_json_size] = 0;

            next();
        }

        ~Lexer() { delete[] m_json_raw; }

        Token next_token() {
            skip_whitespace();

            if (isdigit(m_current)) { return next_number(); }

            switch (m_current) {
                case '.': return next_with_char(Token::Type::Dot);
                case '-': return next_with_char(Token::Type::Minus);
                case '+': return next_with_char(Token::Type::Plus);

                case 'F':
                case 'f': {
                    next();
                    if (!(m_current == 'a' || m_current == 'A')) return Token(Token::Type::Invalid);
                    next();
                    if (!(m_current == 'l' || m_current == 'L')) return Token(Token::Type::Invalid);
                    next();
                    if (!(m_current == 's' || m_current == 'S')) return Token(Token::Type::Invalid);
                    next();
                    if (!(m_current == 'e' || m_current == 'E')) return Token(Token::Type::Invalid);
                    next();
                    return Token(Token::Type::Bool, "false");
                }

                case 't':
                case 'T': {
                    next();
                    if (!(m_current == 'r' || m_current == 'R')) return Token(Token::Type::Invalid);
                    next();
                    if (!(m_current == 'u' || m_current == 'U')) return Token(Token::Type::Invalid);
                    next();
                    if (!(m_current == 'e' || m_current == 'E')) return Token(Token::Type::Invalid);
                    next();
                    return Token(Token::Type::Bool, "true");
                }

                case 'n':
                case 'N': {
                    next();
                    if (!(m_current == 'u' || m_current == 'L')) return Token(Token::Type::Invalid);
                    next();
                    if (!(m_current == 'l' || m_current == 'L')) return Token(Token::Type::Invalid);
                    next();
                    if (!(m_current == 'l' || m_current == 'L')) return Token(Token::Type::Invalid);
                    next();
                    return Token(Token::Type::Null, "null");
                }
                case 'e':
                case 'E': return next_with_char(Token::Type::Exponent);

                case ':': return next_with_char(Token::Type::Colon);
                case ',': return next_with_char(Token::Type::Comma);

                case '{': return next_with_char(Token::Type::CurlyOpen);
                case '}': return next_with_char(Token::Type::CurlyClose);

                case '[': return next_with_char(Token::Type::SquareOpen);
                case ']': return next_with_char(Token::Type::SquareClose);

                case '"': {
                    next();
                    std::stringstream string;

                    while (m_current != '"') {
                        string << m_current;

                        if (m_current == '\\' && peek() == '"') {
                            string << "\"";
                            next();
                        }

                        next();
                    }
                    next();

                    return Token(Token::Type::String, string.str());
                }
                case '\0': return Token(Token::Type::EndOfFile);
            }

            return Token();
        }

    private:
        void skip_whitespace() {
            while (m_current == '\t' || m_current == ' ' || m_current == '\r' || m_current == '\n') { next(); }
        }

        Token next_number() {
            std::stringstream number;

            while (isdigit(m_current)) {
                number << m_current;
                next();
            }

            return Token(Token::Type::Int, number.str());
        }

        Token next_with_char(Token::Type type) {
            Token tok{type, std::string(1, m_current)};
            next();
            return tok;
        }

        Token next_with_string(Token::Type type, const std::string &str) {
            next();
            return {type, str};
        }

        void next(int amount = 1) {
            if (amount == 1) {
                m_current_index++;
                assert(m_current_index <= m_json_size);

                m_current = m_json_raw[m_current_index];
                return;
            }
            for (int i = 0; i < amount; i++) { next(); }
        }

        [[nodiscard]] char peek(int amount = 1) const {
            if (m_current_index + amount < m_json_size) {
                return m_json_raw[m_current_index + amount];
            } else {
                return m_json_raw[m_json_size];
            }
        }

    private:
        char *m_json_raw;
        size_t m_json_size;

        char m_current{0};
        int m_current_index{-1};
    };
    class Parser {
    public:
        explicit Parser(Shared<Lexer> lexer) : m_lexer(std::move(lexer)) {}

        Shared<JSON::Object> parse_obj() {
            m_current_token = m_lexer->next_token();
            if (m_current_token == Token::Type::CurlyOpen) return parse_object();
            return JSON::object();
        }

        Shared<JSON::Array> parse_arr() {
            m_current_token = m_lexer->next_token();
            if (m_current_token == Token::Type::SquareOpen) return parse_array();
            return JSON::array();
        }

    private:
        void expect(Token::Type token_id) { m_expected = token_id; }
        void dont_expect(Token::Type token_id) { m_not_expected = token_id; }

        Token eat(Token::Type token_id) {
            if (m_not_expected != Token::Type::Invalid && m_current_token == m_not_expected) {
                std::cerr << "Didn't expected " << Token::to_string(m_expected) << ", but got " << Token::to_string(m_current_token.token_type())
                          << std::endl;
                assert(false);
            }

            if (m_expected != Token::Type::Invalid && m_current_token != m_expected) {
                std::cerr << "[Expected] Expected " << Token::to_string(m_expected) << ", but got " << Token::to_string(m_current_token.token_type())
                          << std::endl;
                assert(false);
            }

            if (token_id != m_current_token.token_type()) {
                std::cerr << "Expected " << Token::to_string(token_id) << ", but got " << Token::to_string(m_current_token.token_type()) << std::endl;
                assert(false);
            }

            m_expected = Token::Type::Invalid;
            m_not_expected = Token::Type::Invalid;

            m_current_token = m_lexer->next_token();
            return m_current_token;
        }

        Shared<JSON::Object> parse_object() {
            eat(Token::Type::CurlyOpen);

            auto object = create<Object>();

            while (m_current_token != Token::Type::CurlyClose) {
                auto key = m_current_token.raw();
                eat(Token::Type::String);
                eat(Token::Type::Colon);

                if (m_current_token == Token::Type::CurlyOpen) {
                    //Object
                    object->set(key, parse_object());
                } else if (m_current_token == Token::Type::SquareOpen) {
                    //Array
                    object->set(key, parse_array());
                } else {
                    object->set(key, parse_literal());
                }

                if (m_current_token == Token::Type::Comma) {
                    eat(Token::Type::Comma);
                    expect(Token::Type::String);
                }
            }

            eat(Token::Type::CurlyClose);
            return object;
        }

        Shared<JSON::Array> parse_array() {
            eat(Token::Type::SquareOpen);
            auto array = create<Array>();

            while (m_current_token != Token::Type::SquareClose) {
                if (m_current_token == Token::Type::CurlyOpen) {
                    //Object
                    array->insert(parse_object());
                } else if (m_current_token == Token::Type::SquareOpen) {
                    //Array
                    array->insert(parse_array());
                } else {
                    array->insert(parse_literal());
                }

                if (m_current_token == Token::Type::Comma) {
                    eat(Token::Type::Comma);
                    dont_expect(Token::Type::SquareClose);
                }
            }

            eat(Token::Type::SquareClose);
            return array;
        }

        Shared<JSON::Literal> parse_literal() {
            Shared<JSON::Literal> literal;
            if (m_current_token == Token::Type::Null) {
                //Null
                literal = JSON::literal();
                eat(Token::Type::Null);
            } else if (m_current_token == Token::Type::String) {
                //String
                literal = JSON::literal(m_current_token.raw().c_str());
                eat(Token::Type::String);
            } else if (m_current_token == Token::Type::Minus || m_current_token == Token::Type::Int) {
                //Int
                literal = parse_number();
            } else if (m_current_token == Token::Type::Bool) {
                //Bool
                literal = JSON::literal(m_current_token.raw().size() == std::string("true").size());
                eat(Token::Type::Bool);
            } else {
                literal = JSON::literal();
            }
            return literal;
        }

        Shared<JSON::Literal> parse_number() {
            bool is_negative = m_current_token == Token::Type::Minus;
            if (is_negative) eat(Token::Type::Minus);

            bool is_float = false;
            std::stringstream number;

            number << m_current_token.raw();
            eat(Token::Type::Int);

            if (m_current_token == Token::Type::Dot) {
                is_float = true;
                eat(Token::Type::Dot);
                number << "." << m_current_token.raw();
                eat(Token::Type::Int);

                if (m_current_token == Token::Type::Exponent) {
                    eat(Token::Type::Exponent);
                    number << "e";
                    if (m_current_token == Token::Type::Plus || m_current_token == Token::Type::Minus) {
                        number << m_current_token.raw();
                        eat(m_current_token.token_type());
                    }
                    number << m_current_token.raw();
                    eat(Token::Type::Int);
                }
            }

            if (is_float) { return JSON::literal(std::stof(number.str())); }
            return JSON::literal(std::stoi(number.str()));
        }

    private:
        Token::Type m_expected = Token::Type::Invalid;
        Token::Type m_not_expected = Token::Type::Invalid;

        Shared<Lexer> m_lexer;
        Token m_current_token{};
    };

public:
    class Object : public Dumpable, public std::enable_shared_from_this<Object> {
    public:
        ~Object() override = default;

        std::string dump(bool beautify) override {
            m_indent_level++;
            std::stringstream result;
            result << "{";
            if (beautify) result << "\n";
            size_t i = 0;
            for (auto &property : m_properties) {
                i++;
                if (beautify) { result << repeat(m_indent_level, JSON::m_indententation_chars); }
                result << "\"" << property.first << "\"";
                result << ":";
                if (beautify) result << " ";
                result << property.second->dump(beautify);
                if (i != size()) result << ",";
                if (beautify) result << "\n";
            }
            m_indent_level--;
            if (beautify) { result << repeat(m_indent_level, JSON::m_indententation_chars); }
            result << "}";
            return result.str();
        }

        //Let it return a reference to itself
        Shared<Object> set(const std::string &name, const Shared<Dumpable> &property) {
            m_properties[name] = property;
            return shared_from_this();
        }

        template<typename T>
        Shared<T> get(const std::string &name) {
            if (!has(name)) return nullptr;

            if (std::is_base_of_v<Dumpable, T>) { return std::static_pointer_cast<T>(m_properties[name]); }
            return nullptr;
        }

        Shared<JSON::Literal> get_literal(const std::string &name) { return get<JSON::Literal>(name); }
        Shared<JSON::Object> get_object(const std::string &name) { return get<JSON::Object>(name); }
        Shared<JSON::Array> get_array(const std::string &name) { return get<JSON::Array>(name); }

        [[nodiscard]] bool has(const std::string &name) const { return m_properties.find(name) != m_properties.end(); }

        int get_int(const std::string &name) {
            auto literal = get_literal(name);
            if (!literal) return 0;

            if (literal->is_int()) return literal->as_int();
            return 0;
        }

        float get_float(const std::string &name) {
            auto literal = get_literal(name);
            if (!literal) return 0.0f;

            if (literal->is_float()) return literal->as_float();
            return 0.0f;
        }

        bool get_bool(const std::string &name) {
            auto literal = get_literal(name);
            if (!literal) return false;

            if (literal->is_bool()) return literal->as_bool();
            return false;
        }

        const char *get_string(const std::string &name) {
            auto literal = get_literal(name);
            if (!literal) return "";

            if (literal->is_string()) return literal->as_string();
            return "";
        }

        std::unordered_map<std::string, Shared<Dumpable>>::iterator begin() { return m_properties.begin(); }
        std::unordered_map<std::string, Shared<Dumpable>>::iterator end() { return m_properties.end(); }

        std::unordered_map<std::string, Shared<Dumpable>>::const_iterator cbegin() { return m_properties.cbegin(); }
        std::unordered_map<std::string, Shared<Dumpable>>::const_iterator cend() { return m_properties.cend(); }

        size_t size() const { return m_properties.size(); }

    private:
        std::unordered_map<std::string, Shared<Dumpable>> m_properties;
    };

    class Array : public Dumpable, public std::enable_shared_from_this<Array> {
    public:
        ~Array() override = default;

        std::string dump(bool beautify) override {
            m_indent_level++;
            std::stringstream result;
            result << "[";
            if (beautify) result << "\n";
            for (size_t i = 0; i < count(); i++) {
                if (beautify) { result << repeat(m_indent_level, JSON::m_indententation_chars); }
                result << m_values[i]->dump(beautify);

                if (i != count() - 1) { result << ","; }
                if (beautify) result << "\n";
            }
            m_indent_level--;
            if (beautify) { result << repeat(m_indent_level, JSON::m_indententation_chars); }
            result << "]";
            return result.str();
        }

        Shared<Array> insert(const Shared<Dumpable> &value) {
            m_values.emplace_back(value);
            return shared_from_this();
        }

        Shared<Array> insert(const std::vector<Shared<Dumpable>> &values) {
            m_values.insert(end(), values.begin(), values.end());
            return shared_from_this();
        }

        template<typename T>
        Shared<T> get(int index) {
            if (index > count()) return nullptr;

            if (std::is_base_of_v<Dumpable, T>) { return std::static_pointer_cast<T>(m_values[index]); }
            return nullptr;
        }

        Shared<JSON::Literal> get_literal(int index) { return get<JSON::Literal>(index); }
        Shared<JSON::Object> get_object(int index) { return get<JSON::Object>(index); }
        Shared<JSON::Array> get_array(int index) { return get<JSON::Array>(index); }

        int get_int(int index) {
            auto literal = get_literal(index);
            if (!literal) return 0;

            if (literal->is_int()) return literal->as_int();
            return 0;
        }
        float get_float(int index) {
            auto literal = get_literal(index);
            if (!literal) return 0.0f;

            if (literal->is_float()) return literal->as_float();
            return 0.0f;
        }

        bool get_bool(int index) {
            auto literal = get_literal(index);
            if (!literal) return false;

            if (literal->is_bool()) return literal->as_bool();
            return false;
        }

        const char *get_string(int index) {
            auto literal = get_literal(index);
            if (!literal) return "";

            if (literal->is_string()) return literal->as_string();
            return "";
        }

        [[nodiscard]] size_t count() const { return m_values.size(); }

        std::vector<Shared<Dumpable>>::iterator begin() { return m_values.begin(); }
        std::vector<Shared<Dumpable>>::iterator end() { return m_values.end(); }

        std::vector<Shared<Dumpable>>::const_iterator cbegin() { return m_values.cbegin(); }
        std::vector<Shared<Dumpable>>::const_iterator cend() { return m_values.cend(); }

    private:
        std::vector<Shared<Dumpable>> m_values;
    };

    class Literal : public Dumpable {
    public:
        ~Literal() override = default;

        enum class Type { Bool, Int, Float, String, Null };

        std::string dump(bool beautify) override {
            std::stringstream result;
            if (is_null()) { result << "null"; }
            if (is_bool()) { result << (as_bool() ? "true" : "false"); }
            if (is_int()) { result << std::to_string(as_int()); }
            if (is_float()) { result << std::to_string(as_float()); }
            if (is_string()) {
                result << "\"";
                result << as_string();
                result << "\"";
            }
            return result.str();
        }

        explicit Literal() : m_raw("null"), m_type(Type::Null) {}
        explicit Literal(bool value) : m_raw((value ? "true" : "false")), m_type(Type::Bool) { m_literal_data.as_bool = value; }
        explicit Literal(int value) : m_raw(std::to_string(value)), m_type(Type::Int) { m_literal_data.as_int = value; }
        explicit Literal(float value) : m_raw(std::to_string(value)), m_type(Type::Float) { m_literal_data.as_float = value; }
        explicit Literal(const char *value) : m_raw(value), m_type(Type::String) {}

        [[nodiscard]] const std::string &get_raw() const { return m_raw; }

        [[nodiscard]] bool is_bool() const { return m_type == Type::Bool; }
        [[nodiscard]] bool is_int() const { return m_type == Type::Int; }
        [[nodiscard]] bool is_float() const { return m_type == Type::Float; }
        [[nodiscard]] bool is_string() const { return m_type == Type::String; }
        [[nodiscard]] bool is_null() const { return m_type == Type::Null; }

        [[nodiscard]] int as_int() const {
            assert(is_int());
            return m_literal_data.as_int;
        }

        [[nodiscard]] bool as_bool() const {
            assert(is_bool());
            return m_literal_data.as_bool;
        }

        [[nodiscard]] float as_float() const {
            assert(is_float());
            return m_literal_data.as_float;
        }

        [[nodiscard]] const char *as_string() const {
            assert(is_string());
            return get_raw().c_str();
        }

        [[nodiscard]] Type get_type() const { return m_type; }

    private:
        Type m_type;

        std::string m_raw;

        union {
            int as_int;
            bool as_bool;
            float as_float;
        } m_literal_data{};
    };
};
