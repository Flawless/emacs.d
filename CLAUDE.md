# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

This is a comprehensive Emacs configuration built using the `use-package` and `straight.el` package managers. The configuration is modularized into three main files:

- **init.el**: Entry point that bootstraps `straight.el` and loads configuration modules
- **config/core.el**: Core Emacs settings, Evil mode, general keybindings, and fundamental packages
- **config/editing.el**: Editor behavior, terminal integration, project management, LSP, and themes
- **config/langs.el**: Language-specific configurations and development tools
- **config/langs/flawless-clojure.el**: Specialized Clojure development setup with CIDER and advanced features

## Package Management

The configuration uses **straight.el** as the primary package manager with `use-package` for declarative configuration. Packages are automatically installed and managed through straight.el's Git-based approach.

Key package sources:
- Most packages from MELPA/GNU ELPA via straight.el
- Some packages directly from GitHub repositories
- Custom local packages in `config/langs/` directory

## Key Development Features

### Language Support
- **Clojure**: Full CIDER integration with tree-sitter, advanced REPL features, polylith project support
- **Python**: LSP with pyright, multiple formatters (black, isort, ruff), virtual environment support
- **Go**: Tree-sitter mode with LSP, DAP debugging, automatic formatting and imports
- **JavaScript/TypeScript**: Web-mode, TypeScript support, tide integration
- **Rust**: Rustic mode with LSP and cargo integration
- **Other**: Java, YAML, Terraform, GraphQL, Lua, Vue, and more

### Development Tools
- **LSP Mode**: Language server protocol support for most languages
- **DAP Mode**: Debug adapter protocol for debugging
- **Magit**: Advanced Git integration with GPT-powered commit messages
- **Projectile**: Project management and navigation
- **Flycheck**: Syntax checking and linting
- **Company**: Code completion
- **AI Integration**: OpenAI/Anthropic integration via lsp-ai, gptel, aider, and aidermacs

### Key Bindings
Uses Evil mode with Space as the leader key. Common patterns:
- `SPC f` - File operations
- `SPC b` - Buffer operations  
- `SPC p` - Project operations
- `SPC m` - Mode-specific operations
- `SPC g` - Git operations
- `SPC l` - AI/LLM operations

## Environment Setup

The configuration expects certain environment variables:
- `OPENAI_TOKEN` or `OPENAI_API_KEY` - For AI integrations
- `ANTHROPIC_API_KEY` - For Claude integration via lsp-ai

Environment variables are loaded via direnv (`.envrc` file) and exec-path-from-shell on macOS.

## Development Workflow

### For Clojure Development
1. Open Clojure project
2. Use `SPC mcj` to jack-in with CIDER
3. For polylith projects, aliases are automatically detected and prompted
4. Use `SPC mee` for eval-last-sexp, `SPC med` for eval-defun
5. Access REPL with `SPC mrb`

### For Python Development
1. Virtual environments are automatically detected via pet-mode
2. LSP starts automatically with pyright
3. Formatting happens on save (black, isort, ruff)
4. Use `SPC pc` for project compilation/testing

### For Go Development
1. LSP starts automatically with gopls
2. Formatting and imports organized on save
3. DAP debugging available
4. Tree-sitter provides enhanced syntax highlighting

## AI-Powered Development

The configuration includes multiple AI integrations:
- **lsp-ai**: Inline code completion using Claude 3.5 Sonnet
- **gptel**: Chat interface with GPT models
- **aider/aidermacs**: Direct code modification assistance
- **magit-gptcommit**: AI-generated Git commit messages

Access AI features via `SPC l` prefix or specific mode bindings.

## Performance Considerations

- Uses deferred loading for most packages
- Tree-sitter for syntax highlighting where available
- Remote file handling optimizations for TRAMP
- Flycheck disabled for remote files to improve performance

## Customization

The configuration is highly modular. To add new languages or features:
1. Add `use-package` declarations to appropriate config files
2. Follow existing patterns for keybindings and hooks
3. Language-specific configurations go in `config/langs.el` or separate files in `config/langs/`