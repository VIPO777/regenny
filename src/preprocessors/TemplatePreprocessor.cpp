#include "TemplatePreprocessor.hpp"

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <optional>
#include <random>
#include <sstream>
#include <string_view>
#include <system_error>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace preprocessor {

namespace template_detail {

enum class TemplateParameterKind {
    Type,
    NonType
};

struct TemplateParameter {
    std::string m_name{};
    TemplateParameterKind m_kind{TemplateParameterKind::Type};
};

struct TemplateDefinition {
    struct Specialization {
        std::vector<std::string> m_arguments{};
        std::string m_sanitized_name{};
        std::string m_between{};
        std::string m_body{};
        std::string m_closing{};
    };

    std::string m_keyword{};
    std::string m_name{};
    std::vector<TemplateParameter> m_parameters{};
    std::string m_between{};
    std::string m_body{};
    std::string m_closing{};
    std::string m_indentation{};
    std::string m_scope_path{};
    size_t m_start{};
    size_t m_end{};
    bool m_placeholder_generated{false};
    std::vector<Specialization> m_specializations{};
    std::unordered_map<std::string, size_t> m_specialization_index{};

    std::string full_name() const {
        if (m_scope_path.empty()) {
            return m_name;
        }

        return m_scope_path + "." + m_name;
    }
};

struct ScopeFrame {
    std::string m_name{};
    std::string m_path{};
    size_t m_depth{};
    std::unordered_set<std::string> m_emitted_specializations{};
};

struct PendingScope {
    bool m_expect_name{};
    bool m_expect_brace{};
    std::string m_keyword{};
    std::string m_name{};
};

struct FileProcessResult {
    std::string m_processed_content{};
    bool m_had_templates{};
    std::vector<std::filesystem::path> m_imports{};
};

bool is_identifier_start(char c) {
    return std::isalpha(static_cast<unsigned char>(c)) || c == '_';
}

bool is_identifier_char(char c) {
    return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
}

bool is_type_char(char c) {
    return std::isalnum(static_cast<unsigned char>(c)) || c == '_' || c == '.' || c == ':';
}

void skip_whitespace_and_comments(const std::string& text, size_t& pos) {
    const auto size = text.size();

    while (pos < size) {
        const auto c = text[pos];

        if (std::isspace(static_cast<unsigned char>(c))) {
            ++pos;
            continue;
        }

        if (c == '/' && pos + 1 < size) {
            if (text[pos + 1] == '/') {
                pos += 2;

                while (pos < size && text[pos] != '\n') {
                    ++pos;
                }

                continue;
            }

            if (text[pos + 1] == '*') {
                pos += 2;

                while (pos + 1 < size && !(text[pos] == '*' && text[pos + 1] == '/')) {
                    ++pos;
                }

                pos = std::min(pos + 2, size);
                continue;
            }
        }

        break;
    }
}

void skip_string_literal(const std::string& text, size_t& pos, char delimiter) {
    ++pos;

    while (pos < text.size()) {
        const auto c = text[pos];

        if (c == '\\') {
            pos += 2;
            continue;
        }

        if (c == delimiter) {
            ++pos;
            break;
        }

        ++pos;
    }
}

bool match_keyword(const std::string& text, size_t pos, std::string_view keyword) {
    if (pos + keyword.size() > text.size()) {
        return false;
    }

    if ((pos > 0 && is_identifier_char(text[pos - 1])) ||
        (pos + keyword.size() < text.size() && is_identifier_char(text[pos + keyword.size()]))) {
        return false;
    }

    return std::string_view{text}.substr(pos, keyword.size()) == keyword;
}

std::string trim(std::string_view sv) {
    size_t begin = 0;
    size_t end = sv.size();

    while (begin < end && std::isspace(static_cast<unsigned char>(sv[begin]))) {
        ++begin;
    }

    while (end > begin && std::isspace(static_cast<unsigned char>(sv[end - 1]))) {
        --end;
    }

    return std::string{sv.substr(begin, end - begin)};
}

std::vector<TemplateParameter> split_template_parameters(std::string_view params) {
    std::vector<TemplateParameter> result{};
    int depth = 0;
    size_t token_start = 0;

    for (size_t i = 0; i < params.size(); ++i) {
        const auto c = params[i];

        if (c == '<') {
            ++depth;
        } else if (c == '>') {

            if (depth > 0) {
                --depth;
            }

        } else if (c == ',' && depth == 0) {

            auto token = trim(params.substr(token_start, i - token_start));

            if (!token.empty()) {
                auto cleaned = token;
                int assign_depth = 0;
                size_t assign_pos = std::string::npos;

                for (size_t j = 0; j < cleaned.size(); ++j) {
                    auto ch = cleaned[j];

                    if (ch == '<') {
                        ++assign_depth;
                    } else if (ch == '>') {
                        if (assign_depth > 0) {
                            --assign_depth;
                        }
                    } else if (ch == '=' && assign_depth == 0) {
                        assign_pos = j;
                        break;
                    }
                }

                if (assign_pos != std::string::npos) {
                    cleaned = trim(std::string_view{cleaned}.substr(0, assign_pos));
                }

                if (!cleaned.empty()) {
                    if (cleaned.size() >= 3 && cleaned.compare(cleaned.size() - 3, 3, "...") == 0) {
                        cleaned = trim(cleaned.substr(0, cleaned.size() - 3));
                    }

                    size_t end = cleaned.size();

                    while (end > 0 && std::isspace(static_cast<unsigned char>(cleaned[end - 1]))) {
                        --end;
                    }

                    size_t begin = end;

                    while (begin > 0) {
                        auto ch = cleaned[begin - 1];

                        if (std::isalnum(static_cast<unsigned char>(ch)) || ch == '_') {
                            --begin;
                        } else {
                            break;
                        }
                    }

                    if (begin < end) {
                        TemplateParameter param{};
                        param.m_name = cleaned.substr(begin, end - begin);
                        auto prefix = trim(std::string_view{cleaned}.substr(0, begin));
                        auto prefix_lower = prefix;

                        std::transform(prefix_lower.begin(), prefix_lower.end(), prefix_lower.begin(),
                            [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });

                        if (prefix_lower.find("typename") != std::string::npos ||
                            prefix_lower.find("class") != std::string::npos ||
                            prefix_lower.find("struct") != std::string::npos ||
                            prefix_lower.find("template") != std::string::npos) {
                            param.m_kind = TemplateParameterKind::Type;
                        } else {
                            param.m_kind = TemplateParameterKind::NonType;
                        }

                        result.emplace_back(std::move(param));
                    }
                }
            }

            token_start = i + 1;
        }
    }

    auto tail = trim(params.substr(token_start));

    if (!tail.empty()) {
        auto cleaned = tail;
        int assign_depth = 0;
        size_t assign_pos = std::string::npos;

        for (size_t j = 0; j < cleaned.size(); ++j) {
            auto ch = cleaned[j];

            if (ch == '<') {
                ++assign_depth;
            } else if (ch == '>') {
                if (assign_depth > 0) {
                    --assign_depth;
                }
            } else if (ch == '=' && assign_depth == 0) {
                assign_pos = j;
                break;
            }
        }

        if (assign_pos != std::string::npos) {
            cleaned = trim(std::string_view{cleaned}.substr(0, assign_pos));
        }

        if (!cleaned.empty()) {
            if (cleaned.size() >= 3 && cleaned.compare(cleaned.size() - 3, 3, "...") == 0) {
                cleaned = trim(cleaned.substr(0, cleaned.size() - 3));
            }

            size_t end = cleaned.size();

            while (end > 0 && std::isspace(static_cast<unsigned char>(cleaned[end - 1]))) {
                --end;
            }

            size_t begin = end;

            while (begin > 0) {
                auto ch = cleaned[begin - 1];

                if (std::isalnum(static_cast<unsigned char>(ch)) || ch == '_') {
                    --begin;
                } else {
                    break;
                }
            }

            if (begin < end) {
                TemplateParameter param{};
                param.m_name = cleaned.substr(begin, end - begin);
                auto prefix = trim(std::string_view{cleaned}.substr(0, begin));
                auto prefix_lower = prefix;

                std::transform(prefix_lower.begin(), prefix_lower.end(), prefix_lower.begin(),
                    [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });

                if (prefix_lower.find("typename") != std::string::npos ||
                    prefix_lower.find("class") != std::string::npos || prefix_lower.find("struct") != std::string::npos ||
                    prefix_lower.find("template") != std::string::npos) {
                    param.m_kind = TemplateParameterKind::Type;
                } else {
                    param.m_kind = TemplateParameterKind::NonType;
                }

                result.emplace_back(std::move(param));
            }
        }
    }

    return result;
}

bool parse_template_definition(const std::string& text, size_t pos, TemplateDefinition& out, size_t& consumed) {
    consumed = pos;

    skip_whitespace_and_comments(text, consumed);

    auto size = text.size();

    if (!match_keyword(text, consumed, "struct") && !match_keyword(text, consumed, "class")) {
        return false;
    }

    out.m_keyword = text.substr(consumed, text[consumed] == 's' ? 6 : 5); // "struct" or "class"
    consumed += out.m_keyword.size();

    skip_whitespace_and_comments(text, consumed);

    if (consumed >= size || !is_identifier_start(text[consumed])) {
        return false;
    }

    auto name_start = consumed;

    while (consumed < size && is_identifier_char(text[consumed])) {
        ++consumed;
    }

    out.m_name = text.substr(name_start, consumed - name_start);

    skip_whitespace_and_comments(text, consumed);

    if (consumed >= size || text[consumed] != '<') {
        return false;
    }

    auto params_start = ++consumed;
    int angle_depth = 1;

    while (consumed < size && angle_depth > 0) {
        auto c = text[consumed];

        if (c == '"') {
            skip_string_literal(text, consumed, '"');
            continue;
        }

        if (c == '\'') {
            skip_string_literal(text, consumed, '\'');
            continue;
        }

        if (c == '/' && consumed + 1 < size) {
            if (text[consumed + 1] == '/') {
                while (consumed < size && text[consumed] != '\n') {
                    ++consumed;
                }
                continue;
            }

            if (text[consumed + 1] == '*') {
                consumed += 2;
                while (consumed + 1 < size && !(text[consumed] == '*' && text[consumed + 1] == '/')) {
                    ++consumed;
                }
                consumed = std::min(consumed + 2, size);
                continue;
            }
        }

        if (c == '<') {
            ++angle_depth;
        } else if (c == '>') {
            --angle_depth;
            if (angle_depth == 0) {
                break;
            }
        }

        ++consumed;
    }

    if (angle_depth != 0 || consumed >= size || text[consumed] != '>') {
        return false;
    }

    auto params_view = std::string_view{text}.substr(params_start, consumed - params_start);
    out.m_parameters = split_template_parameters(params_view);

    if (out.m_parameters.empty()) {
        return false;
    }

    ++consumed;
    auto between_start = consumed;

    while (consumed < size) {
        auto c = text[consumed];

        if (c == '"') {
            skip_string_literal(text, consumed, '"');
            continue;
        }

        if (c == '\'') {
            skip_string_literal(text, consumed, '\'');
            continue;
        }

        if (c == '/' && consumed + 1 < size) {
            if (text[consumed + 1] == '/') {
                while (consumed < size && text[consumed] != '\n') {
                    ++consumed;
                }
                continue;
            }

            if (text[consumed + 1] == '*') {
                consumed += 2;
                while (consumed + 1 < size && !(text[consumed] == '*' && text[consumed + 1] == '/')) {
                    ++consumed;
                }
                consumed = std::min(consumed + 2, size);
                continue;
            }
        }

        if (c == '{') {
            break;
        }

        ++consumed;
    }

    if (consumed >= size || text[consumed] != '{') {
        return false;
    }

    out.m_between = text.substr(between_start, consumed - between_start);

    auto body_start = consumed + 1;
    int brace_depth = 1;
    ++consumed;

    while (consumed < size && brace_depth > 0) {
        auto c = text[consumed];

        if (c == '"') {
            skip_string_literal(text, consumed, '"');
            continue;
        }

        if (c == '\'') {
            skip_string_literal(text, consumed, '\'');
            continue;
        }

        if (c == '/' && consumed + 1 < size) {
            if (text[consumed + 1] == '/') {
                while (consumed < size && text[consumed] != '\n') {
                    ++consumed;
                }
                continue;
            }

            if (text[consumed + 1] == '*') {
                consumed += 2;
                while (consumed + 1 < size && !(text[consumed] == '*' && text[consumed + 1] == '/')) {
                    ++consumed;
                }
                consumed = std::min(consumed + 2, size);
                continue;
            }
        }

        if (c == '{') {
            ++brace_depth;
        } else if (c == '}') {
            --brace_depth;
            if (brace_depth == 0) {
                break;
            }
        }

        ++consumed;
    }

    if (brace_depth != 0 || consumed >= size) {
        return false;
    }

    auto body_end = consumed;
    auto closing_start = consumed;

    // include closing brace
    if (closing_start < size && text[closing_start] == '}') {
        ++closing_start;
    }

    while (closing_start < size && std::isspace(static_cast<unsigned char>(text[closing_start]))) {
        if (text[closing_start] == '\n') {
            ++closing_start;
            break;
        }
        ++closing_start;
    }

    if (closing_start < size && text[closing_start] == ';') {
        ++closing_start;
        while (closing_start < size && std::isspace(static_cast<unsigned char>(text[closing_start]))) {
            if (text[closing_start] == '\n') {
                ++closing_start;
                break;
            }
            ++closing_start;
        }
    }

    auto indentation_start = text.rfind('\n', pos);
    size_t indentation_pos = indentation_start == std::string::npos ? 0 : indentation_start + 1;

    out.m_indentation = text.substr(indentation_pos, pos - indentation_pos);
    out.m_body = text.substr(body_start, body_end - body_start);
    out.m_closing = text.substr(body_end, closing_start - body_end);
    out.m_start = pos;
    out.m_end = closing_start;

    consumed = closing_start;
    return true;
}

std::vector<std::string> parse_template_arguments(const std::string& text, size_t lt_pos, size_t& out_pos) {
    std::vector<std::string> result{};
    auto size = text.size();

    if (lt_pos >= size || text[lt_pos] != '<') {
        out_pos = lt_pos;
        return result;
    }

    size_t pos = lt_pos + 1;
    int depth = 1;
    size_t token_start = pos;

    while (pos < size && depth > 0) {
        auto c = text[pos];

        if (c == '"') {
            skip_string_literal(text, pos, '"');
            continue;
        }

        if (c == '\'') {
            skip_string_literal(text, pos, '\'');
            continue;
        }

        if (c == '/' && pos + 1 < size) {
            if (text[pos + 1] == '/') {
                while (pos < size && text[pos] != '\n') {
                    ++pos;
                }
                continue;
            }

            if (text[pos + 1] == '*') {
                pos += 2;
                while (pos + 1 < size && !(text[pos] == '*' && text[pos + 1] == '/')) {
                    ++pos;
                }
                pos = std::min(pos + 2, size);
                continue;
            }
        }

        if (c == '<') {
            ++depth;
        } else if (c == '>') {

            --depth;

            if (depth == 0) {
                auto token = trim(std::string_view{text}.substr(token_start, pos - token_start));

                if (!token.empty()) {
                    result.emplace_back(std::move(token));
                }

                ++pos;
                break;
            }

        } else if (c == ',' && depth == 1) {

            auto token = trim(std::string_view{text}.substr(token_start, pos - token_start));

            if (!token.empty()) {
                result.emplace_back(std::move(token));
            }

            token_start = pos + 1;
        }

        ++pos;
    }

    out_pos = pos;
    return result;
}

std::string sanitize_token(const std::string& token) {
    std::string result{};
    result.reserve(token.size() + 8);

    auto push_sep = [&]() {
        if (!result.empty() && result.back() != '_') {
            result.push_back('_');
        }
    };

    for (auto c : token) {

        if (std::isalnum(static_cast<unsigned char>(c)) || c == '_') {
            result.push_back(c);
        } else if (c == '*') {
            push_sep();
            result += "ptr";
            push_sep();
        } else if (c == '&') {
            push_sep();
            result += "ref";
            push_sep();
        } else if (c == '[' || c == ']') {
            push_sep();
            result += "arr";
            push_sep();
        } else if (c == ':') {
            push_sep();
        } else if (c == '<') {
            push_sep();
            result += "lt";
            push_sep();
        } else if (c == '>') {
            push_sep();
            result += "gt";
            push_sep();
        } else if (c == ',') {
            push_sep();
        } else if (c == '.') {
            push_sep();
            result += '.';
            push_sep();
        } else if (std::isspace(static_cast<unsigned char>(c))) {
            push_sep();
        } else {
            push_sep();
        }
    }

    if (!result.empty() && result.front() == '_') {
        result.erase(result.begin());
    }

    if (!result.empty() && std::isdigit(static_cast<unsigned char>(result.front()))) {
        result.insert(result.begin(), '_');
    }

    if (result.empty()) {
        result = "T";
    }

    std::replace(result.begin(), result.end(), '.', '_');
    return result;
}

std::string sanitize_scope_name(const std::string& path) {
    if (path.empty()) {
        return {};
    }

    std::string sanitized{};
    sanitized.reserve(path.size());

    for (auto c : path) {
        if (std::isalnum(static_cast<unsigned char>(c)) || c == '_') {
            sanitized.push_back(c);
        } else if (c == '.') {
            sanitized.push_back('_');
        } else {
            sanitized.push_back('_');
        }
    }

    return sanitized;
}

size_t find_identifier(const std::string& text, const std::string& token, size_t position) {
    while (true) {
        auto found = text.find(token, position);

        if (found == std::string::npos) {
            return std::string::npos;
        }

        auto before = found == 0 ? '\0' : text[found - 1];
        auto after = found + token.size() >= text.size() ? '\0' : text[found + token.size()];

        if (!is_identifier_char(before) && !is_identifier_char(after)) {
            return found;
        }

        position = found + token.size();
    }
}

std::string make_signature(const std::vector<std::string>& args) {
    std::ostringstream oss{};

    for (auto it = args.begin(); it != args.end(); ++it) {
        if (it != args.begin()) {
            oss << '\x1f';
        }

        oss << *it;
    }

    return oss.str();
}

std::string replace_parameters(std::string text, const std::vector<TemplateParameter>& params, const std::vector<std::string>& args) {
    if (params.size() != args.size()) {
        return text;
    }

    for (size_t i = 0; i < params.size(); ++i) {
        const auto& param = params[i].m_name;
        const auto& replacement = args[i];
        size_t pos = 0;

        while ((pos = find_identifier(text, param, pos)) != std::string::npos) {
            text.replace(pos, param.size(), replacement);
            pos += replacement.size();
        }
    }

    return text;
}

struct ConstantExpressionParser {

    std::string_view m_expr{};
    size_t m_pos{};
    bool m_valid{true};

    explicit ConstantExpressionParser(std::string_view expression)
        : m_expr{expression} {}

    void skip_ws() {
        while (m_pos < m_expr.size() && std::isspace(static_cast<unsigned char>(m_expr[m_pos]))) {
            ++m_pos;
        }
    }

    bool match_char(char ch) {
        skip_ws();

        if (m_pos < m_expr.size() && m_expr[m_pos] == ch) {
            ++m_pos;
            return true;
        }

        return false;
    }

    bool match_token(std::string_view token) {
        skip_ws();

        if (m_expr.substr(m_pos, token.size()) == token) {
            m_pos += token.size();
            return true;
        }

        return false;
    }

    bool starts_with(std::string_view token) {
        skip_ws();
        return m_expr.substr(m_pos, token.size()) == token;
    }

    std::optional<int64_t> parse() {
        auto value = parse_bitwise_or();
        skip_ws();

        if (!m_valid || m_pos != m_expr.size()) {
            return std::nullopt;
        }

        return value;
    }

    int64_t parse_bitwise_or() {
        auto value = parse_bitwise_xor();

        while (m_valid) {
            skip_ws();

            if (starts_with("||")) {
                m_valid = false;
                break;
            }

            if (match_char('|')) {
                if (m_pos < m_expr.size() && m_expr[m_pos] == '|') {
                    m_valid = false;
                    break;
                }

                value |= parse_bitwise_xor();
            } else {
                break;
            }
        }

        return value;
    }

    int64_t parse_bitwise_xor() {
        auto value = parse_bitwise_and();

        while (m_valid) {
            skip_ws();

            if (match_char('^')) {
                value ^= parse_bitwise_and();
            } else {
                break;
            }
        }

        return value;
    }

    int64_t parse_bitwise_and() {
        auto value = parse_shift();

        while (m_valid) {
            skip_ws();

            if (starts_with("&&")) {
                m_valid = false;
                break;
            }

            if (match_char('&')) {
                if (m_pos < m_expr.size() && m_expr[m_pos] == '&') {
                    m_valid = false;
                    break;
                }

                value &= parse_shift();
            } else {
                break;
            }
        }

        return value;
    }

    int64_t parse_shift() {
        auto value = parse_additive();

        while (m_valid) {
            skip_ws();

            if (match_token("<<")) {
                value <<= parse_additive();
            } else if (match_token(">>")) {
                value >>= parse_additive();
            } else {
                break;
            }
        }

        return value;
    }

    int64_t parse_additive() {
        auto value = parse_multiplicative();

        while (m_valid) {
            skip_ws();

            if (match_char('+')) {
                value += parse_multiplicative();
            } else if (match_char('-')) {
                value -= parse_multiplicative();
            } else {
                break;
            }
        }

        return value;
    }

    int64_t parse_multiplicative() {
        auto value = parse_unary();

        while (m_valid) {
            skip_ws();

            if (match_char('*')) {
                value *= parse_unary();
            } else if (match_char('/')) {
                auto rhs = parse_unary();

                if (rhs == 0) {
                    m_valid = false;
                    return 0;
                }

                value /= rhs;
            } else if (match_char('%')) {
                auto rhs = parse_unary();

                if (rhs == 0) {
                    m_valid = false;
                    return 0;
                }

                value %= rhs;
            } else {
                break;
            }
        }

        return value;
    }

    int64_t parse_unary() {
        skip_ws();

        if (match_char('+')) {
            return parse_unary();
        }

        if (match_char('-')) {
            return -parse_unary();
        }

        if (match_char('~')) {
            return ~parse_unary();
        }

        return parse_primary();
    }

    int64_t parse_primary() {
        skip_ws();

        if (match_char('(')) {
            auto value = parse_bitwise_or();

            if (!match_char(')')) {
                m_valid = false;
            }

            return value;
        }

        return parse_number();
    }

    int64_t parse_number() {
        skip_ws();

        if (m_pos >= m_expr.size()) {
            m_valid = false;
            return 0;
        }

        auto start = m_pos;

        if (m_expr[m_pos] == '0' && m_pos + 1 < m_expr.size() && (m_expr[m_pos + 1] == 'x' || m_expr[m_pos + 1] == 'X')) {
            m_pos += 2;
            auto digits_start = m_pos;

            while (m_pos < m_expr.size() && std::isxdigit(static_cast<unsigned char>(m_expr[m_pos]))) {
                ++m_pos;
            }

            if (digits_start == m_pos) {
                m_valid = false;
                return 0;
            }

            auto digits = std::string{m_expr.substr(digits_start, m_pos - digits_start)};

            try {
                auto value = std::stoll(digits, nullptr, 16);
                consume_numeric_suffix();
                return value;
            } catch (...) {
                m_valid = false;
                return 0;
            }
        }

        while (m_pos < m_expr.size() && std::isdigit(static_cast<unsigned char>(m_expr[m_pos]))) {
            ++m_pos;
        }

        if (start == m_pos) {
            m_valid = false;
            return 0;
        }

        auto digits = std::string{m_expr.substr(start, m_pos - start)};

        try {
            auto value = std::stoll(digits, nullptr, 10);
            consume_numeric_suffix();
            return value;
        } catch (...) {
            m_valid = false;
            return 0;
        }
    }

    void consume_numeric_suffix() {
        auto local_pos = m_pos;

        while (local_pos < m_expr.size()) {
            auto ch = m_expr[local_pos];

            if (ch == 'u' || ch == 'U' || ch == 'l' || ch == 'L') {
                ++local_pos;
            } else {
                break;
            }
        }

        m_pos = local_pos;
    }
};

std::optional<int64_t> evaluate_constant_expression(std::string_view expression) {
    ConstantExpressionParser parser{expression};
    return parser.parse();
}

void evaluate_bracket_expressions(std::string& text) {
    size_t search_pos = 0;

    while (search_pos < text.size()) {
        auto open = text.find('[', search_pos);

        if (open == std::string::npos) {
            break;
        }

        if ((open > 0 && text[open - 1] == '[') || (open + 1 < text.size() && text[open + 1] == '[')) {
            search_pos = open + 1;
            continue;
        }

        size_t depth = 1;
        size_t pos = open + 1;

        while (pos < text.size() && depth > 0) {
            auto ch = text[pos];

            if (ch == '[') {
                ++depth;
            } else if (ch == ']') {
                --depth;

                if (depth == 0) {
                    break;
                }
            }

            ++pos;
        }

        if (depth != 0 || pos >= text.size() || text[pos] != ']') {
            search_pos = open + 1;
            continue;
        }

        auto expression = std::string_view{text}.substr(open + 1, pos - open - 1);

        if (!expression.empty()) {
            auto maybe_value = evaluate_constant_expression(expression);

            if (maybe_value.has_value()) {
                auto replacement = std::to_string(*maybe_value);
                text.replace(open + 1, expression.size(), replacement);
                search_pos = open + 1 + replacement.size();
                continue;
            }
        }

        search_pos = pos + 1;
    }
}

std::string convert_template_body_placeholder(const TemplateDefinition& def) {
    auto result = def.m_body;

    for (const auto& param : def.m_parameters) {
        size_t search_pos{};

        while (search_pos < result.size()) {
            auto match_pos = find_identifier(result, param.m_name, search_pos);

            if (match_pos == std::string::npos) {
                break;
            }

            if (param.m_kind == TemplateParameterKind::Type) {
                auto replace_start = match_pos;
                auto lookahead = match_pos + param.m_name.size();

                auto skip_spaces = [&](size_t& idx) {
                    while (idx < result.size() && std::isspace(static_cast<unsigned char>(result[idx]))) {
                        ++idx;
                    }
                };

                skip_spaces(lookahead);

                auto skip_qualifiers = [&](size_t& idx) {
                    bool advanced = true;

                    while (advanced && idx < result.size()) {
                        advanced = false;

                        if (result.compare(idx, 5, "const") == 0 &&
                            (idx + 5 >= result.size() || !is_identifier_char(result[idx + 5]))) {
                            idx += 5;
                            skip_spaces(idx);
                            advanced = true;
                        }

                        if (result.compare(idx, 8, "volatile") == 0 &&
                            (idx + 8 >= result.size() || !is_identifier_char(result[idx + 8]))) {
                            idx += 8;
                            skip_spaces(idx);
                            advanced = true;
                        }
                    }
                };

                skip_qualifiers(lookahead);

                auto pointer_check = lookahead;
                skip_spaces(pointer_check);
                auto pointer_after = pointer_check < result.size() && result[pointer_check] == '*';

                std::string replacement = pointer_after ? "void" : "void*";
                result.replace(replace_start, param.m_name.size(), replacement);
                search_pos = replace_start + replacement.size();
            } else {
                result.replace(match_pos, param.m_name.size(), "1");
                search_pos = match_pos + 1;
            }
        }
    }

    evaluate_bracket_expressions(result);
    return result;
}

std::string generate_placeholder_definition(const TemplateDefinition& def) {
    auto converted_body = convert_template_body_placeholder(def);
    std::ostringstream oss{};

    oss << def.m_indentation << def.m_keyword << ' ' << def.m_name << def.m_between << '{';
    oss << converted_body;
    oss << def.m_closing;

    if (!converted_body.empty() && converted_body.back() != '\n' &&
        (def.m_closing.empty() || def.m_closing.front() != '\n')) {
        oss << '\n';
    }

    return oss.str();
}

struct DefinitionLookup {
    std::unordered_map<std::string, TemplateDefinition*> m_by_full{};
    std::unordered_map<std::string, std::vector<TemplateDefinition*>> m_by_name{};

    void register_definition(TemplateDefinition& def) {
        auto full = def.full_name();
        m_by_full[full] = &def;
        m_by_name[def.m_name].push_back(&def);
    }

    TemplateDefinition* resolve(const std::string& token, const std::string& current_scope) const {
        auto dot = token.rfind('.');
        std::string prefix = dot == std::string::npos ? std::string{} : token.substr(0, dot);
        std::string base = dot == std::string::npos ? token : token.substr(dot + 1);

        if (dot != std::string::npos) {
            if (auto it = m_by_full.find(token); it != m_by_full.end()) {
                return it->second;
            }
        }

        auto iter = m_by_name.find(base);

        if (iter == m_by_name.end()) {
            return nullptr;
        }

        TemplateDefinition* best = nullptr;
        size_t best_score = 0;

        auto score_candidate = [&](TemplateDefinition* def) -> size_t {
            if (!prefix.empty()) {
                if (def->m_scope_path == prefix) {
                    return 1000 + def->m_scope_path.size();
                }
                if (def->m_scope_path.size() >= prefix.size()) {
                    auto pos = def->m_scope_path.rfind(prefix);
                    if (pos != std::string::npos) {
                        auto at_end = pos + prefix.size() == def->m_scope_path.size();
                        auto boundary_ok = pos == 0 || def->m_scope_path[pos - 1] == '.';
                        if (at_end && boundary_ok) {
                            return 700 + prefix.size();
                        }
                    }
                }
            }

            if (def->m_scope_path == current_scope) {
                return 800 + def->m_scope_path.size();
            }

            if (!def->m_scope_path.empty() && !current_scope.empty()) {
                if (current_scope.rfind(def->m_scope_path, 0) == 0) {
                    if (current_scope.size() == def->m_scope_path.size() ||
                        current_scope[def->m_scope_path.size()] == '.') {
                        return 400 + def->m_scope_path.size();
                    }
                }
            }

            if (def->m_scope_path.empty()) {
                return 100;
            }

            return 0;
        };

        for (auto* def : iter->second) {
            auto score = score_candidate(def);

            if (score > best_score) {
                best_score = score;
                best = def;
            }

            if (score >= 1000) {
                break;
            }
        }

        return best;
    }
};

const TemplateDefinition::Specialization& register_specialization(
    TemplateDefinition& def, const std::vector<std::string>& args, const std::string& prefix,
    const std::string& current_scope) {
    std::string scope_hint{};

    if (!prefix.empty()) {
        scope_hint = sanitize_scope_name(prefix);
    } else if (!def.m_scope_path.empty()) {
        scope_hint = sanitize_scope_name(def.m_scope_path);
    } else {
        scope_hint = sanitize_scope_name(current_scope);
    }

    auto scope_token = scope_hint;
    auto signature = scope_token + "|" + make_signature(args);

    if (auto it = def.m_specialization_index.find(signature); it != def.m_specialization_index.end()) {
        return def.m_specializations[it->second];
    }

    std::string sanitized_name = def.m_name;

    if (!scope_token.empty()) {
        sanitized_name += "_";
        sanitized_name += scope_token;
    }

    for (auto&& arg : args) {
        sanitized_name += "_";
        sanitized_name += sanitize_token(arg);
    }

    auto spec_index = def.m_specializations.size();
    def.m_specialization_index.emplace(signature, spec_index);

    TemplateDefinition::Specialization spec{};
    spec.m_arguments = args;
    spec.m_sanitized_name = std::move(sanitized_name);
    spec.m_between = replace_parameters(def.m_between, def.m_parameters, args);
    spec.m_body = replace_parameters(def.m_body, def.m_parameters, args);
    evaluate_bracket_expressions(spec.m_body);
    spec.m_closing = replace_parameters(def.m_closing, def.m_parameters, args);

    def.m_specializations.emplace_back(std::move(spec));
    return def.m_specializations.back();
}

std::vector<std::filesystem::path> extract_imports(const std::string& text, const std::filesystem::path& file_path) {
    std::vector<std::filesystem::path> imports{};
    auto size = text.size();
    size_t pos = 0;

    while (pos < size) {
        auto c = text[pos];

        if (c == '"') {
            skip_string_literal(text, pos, '"');
            continue;
        }

        if (c == '\'') {
            skip_string_literal(text, pos, '\'');
            continue;
        }

        if (c == '/' && pos + 1 < size) {
            if (text[pos + 1] == '/') {
                pos += 2;
                while (pos < size && text[pos] != '\n') {
                    ++pos;
                }
                continue;
            }

            if (text[pos + 1] == '*') {
                pos += 2;
                while (pos + 1 < size && !(text[pos] == '*' && text[pos + 1] == '/')) { ++pos; }
                pos = std::min(pos + 2, size);
                continue;
            }
        }

        if (!is_identifier_start(c)) {
            ++pos;
            continue;
        }

        auto id_start = pos;

        while (pos < size && is_identifier_char(text[pos])) {
            ++pos;
        }

        auto identifier = std::string_view{text}.substr(id_start, pos - id_start);

        if (identifier != "import") {
            continue;
        }

        skip_whitespace_and_comments(text, pos);

        if (pos >= size || text[pos] != '"') {
            continue;
        }

        ++pos;
        auto path_start = pos;

        while (pos < size && text[pos] != '"') {
            if (text[pos] == '\\' && pos + 1 < size) {
                pos += 2;
            } else {
                ++pos;
            }
        }

        if (pos > path_start) {
            auto imported = text.substr(path_start, pos - path_start);
            auto absolute = std::filesystem::absolute(file_path.parent_path() / imported);

            try {
                imports.emplace_back(std::filesystem::weakly_canonical(absolute));
            } catch (...) {
                imports.emplace_back(std::move(absolute));
            }
        }

        if (pos < size && text[pos] == '"') {
            ++pos;
        }
    }

    return imports;
}

std::string random_suffix() {
    static std::mt19937_64 rng{
        static_cast<uint64_t>(std::chrono::high_resolution_clock::now().time_since_epoch().count())};

    std::uniform_int_distribution<uint64_t> dist{};
    std::ostringstream oss{};
    oss << std::hex << dist(rng);
    return oss.str();
}

std::filesystem::path canonicalize_path(const std::filesystem::path& path) {
    std::error_code ec{};
    auto absolute = std::filesystem::absolute(path, ec);

    if (ec) {
        absolute = path;
    }

    try {
        return std::filesystem::weakly_canonical(absolute);
    } catch (...) {
        return absolute;
    }
}

std::string current_indent(const std::string& text) {
    auto newline = text.find_last_of('\n');

    if (newline == std::string::npos) {
        return {};
    }

    std::string indent{};
    size_t idx = newline + 1;

    while (idx < text.size() && (text[idx] == ' ' || text[idx] == '\t')) {
        indent.push_back(text[idx]);
        ++idx;
    }

    return indent;
}

void remove_temp_directory(const std::filesystem::path& temp_directory) {
    if (temp_directory.empty()) {
        return;
    }

    std::error_code ec{};
    std::filesystem::remove_all(temp_directory, ec);
}

FileProcessResult process_file_content(const std::filesystem::path& file_path, const std::string& text) {
    FileProcessResult result{};
    DefinitionLookup lookup{};
    std::vector<TemplateDefinition> definitions{};
    definitions.reserve(16);

    std::string output{};
    output.reserve(text.size() + 512);

    size_t pos = 0;
    size_t brace_depth = 0;
    std::vector<ScopeFrame> scope_stack{{"", "", 0, {}}};
    PendingScope pending_scope{};

    while (pos < text.size()) {
        auto c = text[pos];

        if (c == '"') {
            auto start = pos;
            skip_string_literal(text, pos, '"');
            output.append(text, start, pos - start);
            continue;
        }

        if (c == '\'') {
            auto start = pos;
            skip_string_literal(text, pos, '\'');
            output.append(text, start, pos - start);
            continue;
        }

        if (c == '/' && pos + 1 < text.size()) {
            if (text[pos + 1] == '/') {
                auto start = pos;
                pos += 2;

                while (pos < text.size() && text[pos] != '\n') {
                    ++pos;
                }

                output.append(text, start, pos - start);
                continue;
            }

            if (text[pos + 1] == '*') {
                auto start = pos;
                pos += 2;

                while (pos + 1 < text.size() && !(text[pos] == '*' && text[pos + 1] == '/')) {
                    ++pos;
                }

                pos = std::min(pos + 2, text.size());
                output.append(text, start, pos - start);
                continue;
            }
        }

        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            TemplateDefinition def{};
            size_t consumed = 0;

            if (parse_template_definition(text, pos, def, consumed)) {
                def.m_scope_path = scope_stack.back().m_path;
                definitions.emplace_back(std::move(def));
                TemplateDefinition& stored = definitions.back();
                lookup.register_definition(stored);
                result.m_had_templates = true;

                if (!stored.m_placeholder_generated) {
                    auto placeholder = generate_placeholder_definition(stored);
                    output += placeholder;
                    if (!placeholder.empty() && output.back() != '\n') {
                        output.push_back('\n');
                    }
                    stored.m_placeholder_generated = true;
                }

                pos = stored.m_end;
                pending_scope = {};
                continue;
            }
        }

        if (c == '{') {
            output.push_back('{');
            ++pos;
            ++brace_depth;

            if (pending_scope.m_expect_brace) {
                auto new_path = scope_stack.back().m_path;

                if (!pending_scope.m_name.empty()) {
                    if (!new_path.empty()) {
                        new_path += '.';
                    }
                    new_path += pending_scope.m_name;
                }

                scope_stack.push_back(ScopeFrame{pending_scope.m_name, new_path, brace_depth, {}});
                pending_scope = {};
            }

            continue;
        }

        if (c == '}') {
            output.push_back('}');
            ++pos;

            if (brace_depth > 0) {
                --brace_depth;
            }

            while (scope_stack.size() > 1 && scope_stack.back().m_depth > brace_depth) {
                scope_stack.pop_back();
            }

            pending_scope = {};
            continue;
        }

        if (c == ';') {
            output.push_back(';');
            ++pos;
            pending_scope = {};
            continue;
        }

        if (std::isspace(static_cast<unsigned char>(c))) {
            output.push_back(c);
            ++pos;
            continue;
        }

        if (is_type_char(c)) {
            auto token_start = pos;

            while (pos < text.size() && is_type_char(text[pos])) {
                ++pos;
            }

            auto token = text.substr(token_start, pos - token_start);

            if (pending_scope.m_expect_name) {
                pending_scope.m_name = token;
                pending_scope.m_expect_name = false;
                pending_scope.m_expect_brace = true;
            }

            if (token == "namespace" || token == "struct" || token == "class") {
                pending_scope = {};
                pending_scope.m_keyword = token;
                pending_scope.m_expect_name = true;
                output.append(token);
                continue;
            }

            auto lookahead = pos;
            skip_whitespace_and_comments(text, lookahead);

            if (lookahead < text.size() && text[lookahead] == '<') {
                size_t args_end{};
                auto args = parse_template_arguments(text, lookahead, args_end);

                if (!args.empty()) {
                    auto* def = lookup.resolve(token, scope_stack.back().m_path);

                    if (def != nullptr) {
                    auto dot_pos = token.rfind('.');
                    std::string token_prefix = dot_pos == std::string::npos ? std::string{} : token.substr(0, dot_pos);
                        const auto& specialization = register_specialization(*def, args, token_prefix, scope_stack.back().m_path);
                        auto& scope = scope_stack.back();

                        if (scope.m_emitted_specializations.insert(specialization.m_sanitized_name).second) {
                            auto indent = current_indent(output);
                            if (!output.empty() && output.back() != '\n') {
                                output.push_back('\n');
                            }
                            output += indent;
                            output += def->m_keyword;
                            output.push_back(' ');
                            output += specialization.m_sanitized_name;
                            output += specialization.m_between;
                            output.push_back('{');
                            output += specialization.m_body;

                            auto first_non_space = std::find_if_not(specialization.m_closing.begin(),
                                specialization.m_closing.end(), [](unsigned char ch) { return std::isspace(ch); });

                            if (first_non_space == specialization.m_closing.end() || *first_non_space != '}') {
                                output.push_back('}');
                            }

                            output += specialization.m_closing;

                            if (output.empty() || output.back() != '\n') {
                                output.push_back('\n');
                            }

                            output += indent;
                        }

                        output += specialization.m_sanitized_name;
                        pos = args_end;
                        result.m_had_templates = true;
                        pending_scope = {};
                        continue;
                    }
                }

                // No specialization emitted, emit original text.
                output.append(text, token_start, args_end - token_start);
                pos = args_end;
                continue;
            }

            output += token;
            continue;
        }

        output.push_back(c);
        ++pos;
    }

    result.m_processed_content = std::move(output);
    result.m_imports = extract_imports(result.m_processed_content, file_path);
    return result;
}

} // namespace template_detail

std::optional<PreprocessResult> TemplatePreprocessor::process_tree(const std::filesystem::path& root_path) {

    if (root_path.empty()) {
        return std::nullopt;
    }

    PreprocessResult result{};
    auto canonical_root = template_detail::canonicalize_path(root_path);
    result.m_original_root = canonical_root;

    auto temp_dir = std::filesystem::temp_directory_path() / ("regenny_tmpl_" + template_detail::random_suffix());
    std::error_code ec{};
    std::filesystem::create_directories(temp_dir, ec);

    if (ec) {
        return std::nullopt;
    }

    result.m_temp_directory = temp_dir;

    std::unordered_set<std::filesystem::path, 
        std::hash<std::filesystem::path>, std::equal_to<>> visited{};

    std::vector<std::filesystem::path> queue{canonical_root};
    auto base_dir = canonical_root.parent_path();

    while (!queue.empty()) {
        auto current = queue.back();
        queue.pop_back();

        auto canonical_current = template_detail::canonicalize_path(current);

        if (!visited.emplace(canonical_current).second) {
            continue;
        }

        std::ifstream file{canonical_current};

        if (!file.is_open()) {
            continue;
        }

        std::stringstream buffer{};
        buffer << file.rdbuf();
        auto content = buffer.str();
        auto processed = template_detail::process_file_content(canonical_current, content);

        if (processed.m_had_templates) {
            result.m_had_templates = true;
        }

        ec.clear();
        auto relative = std::filesystem::relative(canonical_current, base_dir, ec);

        if (ec) {
            relative = canonical_current.filename();
        }

        auto processed_path = (temp_dir / relative).lexically_normal();
        std::filesystem::create_directories(processed_path.parent_path(), ec);

        if (ec) {
            continue;
        }

        std::ofstream out_file{processed_path, std::ios::binary};

        if (!out_file.is_open()) {
            continue;
        }

        out_file << processed.m_processed_content;

        result.m_original_to_processed.emplace(canonical_current, processed_path);
        result.m_processed_to_original.emplace(processed_path, canonical_current);

        for (auto& import : processed.m_imports) {
            queue.emplace_back(template_detail::canonicalize_path(import));
        }
    }

    if (!result.m_had_templates) {
        template_detail::remove_temp_directory(temp_dir);
        return std::nullopt;
    }

    if (auto it = result.m_original_to_processed.find(canonical_root); it != result.m_original_to_processed.end())
        result.m_processed_root = it->second;
    else
        result.m_processed_root = canonical_root;

    return result;
}

void TemplatePreprocessor::cleanup(const PreprocessResult& result) {
    template_detail::remove_temp_directory(result.m_temp_directory);
}

} // namespace preprocessor







