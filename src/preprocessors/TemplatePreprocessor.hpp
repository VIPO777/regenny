#pragma once

#include "IPreprocessor.hpp"

namespace preprocessor {

class TemplatePreprocessor final : public IPreprocessor {
public:
    TemplatePreprocessor() = default;
    ~TemplatePreprocessor() override = default;

    [[nodiscard]] std::optional<PreprocessResult> process_tree(const std::filesystem::path& root_path) override;
    void cleanup(const PreprocessResult& result) override;
};

} // namespace preprocessor
