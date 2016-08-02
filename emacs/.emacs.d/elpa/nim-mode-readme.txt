Large parts of this code is shamelessly stolen from python.el and
adapted to Nim

Todo:

-- Make things non-case-sensitive and ignore underscores
-- Identifier following "proc" gets font-lock-function-name-face
-- Treat parameter lists separately
-- Treat pragmas inside "{." and ".}" separately
-- Make double-# comments get font-lock-doc-face
-- Highlight tabs as syntax error
