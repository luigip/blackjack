blackjack engine
================
A work in progress - although it's nearly there!

Objective is to create a flexible, accurate, and fast simulator, to facilitate statistical and strategic analysis given sets of rules (which vary widely between casinos and online).

Design is functional, with no mutable state. Player hands are modelled as a binary trees, with each decision as a node.

Strategies and rules are loaded from XML resource files, which can be easily configured and exported from the supplied spreadsheets.

The extensive test suite uses Scalatest.
