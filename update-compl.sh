#!/bin/bash

echo "Reinstalling ZSH completions for DotF..."
rm ~/.config/zsh/functions/_dotf
dotf --zsh-completion-script `which dotf` >> ~/.config/zsh/functions/_dotf
