# Tree-sitter grammar ABI 13 fixes for tsx and go
final: prev: {
  tree-sitter-grammars = prev.tree-sitter-grammars // {
    tree-sitter-tsx = prev.tree-sitter-grammars.tree-sitter-tsx.overrideAttrs
      (_: {
        nativeBuildInputs = [ final.tree-sitter ];
        configurePhase = ''
          tree-sitter generate --abi 13 src/grammar.json
        '';
      });
    tree-sitter-go = prev.tree-sitter-grammars.tree-sitter-go.overrideAttrs
      (_: {
        nativeBuildInputs = [ final.tree-sitter ];
        configurePhase = ''
          tree-sitter generate --abi 13 src/grammar.json
        '';
      });
  };
}
