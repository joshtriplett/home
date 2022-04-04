" recognize triple-quoted regions as code
syntax region markdownFencedCodeBlock matchgroup=markdownCodeDelimiter keepend start="```" end="```"
" replace markdownCodeBlock with markdownFencedCodeBlock to avoid highlighting
" 4-space indented regions as code
syn cluster markdownBlock contains=markdownH1,markdownH2,markdownH3,markdownH4,markdownH5,markdownH6,markdownBlockquote,markdownListMarker,markdownOrderedListMarker,markdownFencedCodeBlock,markdownRule
