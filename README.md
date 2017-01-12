# Assembla

Manage assembla tickets from within Emacs. 

Project is still under development...
[![Build Status](https://travis-ci.org/jezifm/assembla.svg?branch=master)](https://travis-ci.org/jezifm/assembla)

Inspired by `magit` and `prodigy`

## Installation

Add `assembla` to you load path

## Usage

Need to set `credentials` before running. Add the following in your `init.el`
```elisp
(require 'assembla)
(setq assembla-api-key "MY_API_KEY")
(setq assembla-api-secret "MY_API_SECRET")
```

Start Assembla with `M-x assembla`
