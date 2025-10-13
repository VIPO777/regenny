#pragma once

#include <filesystem>
#include <map>
#include <optional>
#include <string>

namespace preprocessor {

struct PreprocessResult {
    std::filesystem::path m_original_root{};
    std::filesystem::path m_processed_root{};
    std::filesystem::path m_temp_directory{};
    std::map<std::filesystem::path, std::filesystem::path, std::less<>> m_original_to_processed{};
    std::map<std::filesystem::path, std::filesystem::path, std::less<>> m_processed_to_original{};
    bool m_had_templates{false};
};

class IPreprocessor {
public:
    virtual ~IPreprocessor() = default;

    [[nodiscard]] virtual std::optional<PreprocessResult> process_tree(const std::filesystem::path& root_path) = 0;
    virtual void cleanup(const PreprocessResult& result) = 0;
};

} // namespace preprocessor