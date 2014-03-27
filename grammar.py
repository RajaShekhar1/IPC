"""Generate random sentences from a grammar.  The grammar
consists of entries that can be written as S = 'NP VP | S and S',
which gets translated to {'S': [['NP', 'VP'], ['S', 'and', 'S']]}, and
means that one of the top-level lists will be chosen at random, and
then each element of the second-level list will be rewritten; if a symbol is
not in the grammar it rewrites as itself.   The functions generate and
generate_tree generate a string and tree representation, respectively, of
a random sentence."""

import random

def Grammar(**grammar):
  "Create a dictionary mapping symbols to alternatives."
  for (cat, rhs) in grammar.items():
    grammar[cat] = [alt.split() for alt in rhs.split('|')]
  return grammar
  
grammar = Grammar(
  S  = 'NP VP',
  NP = 'Art N',
  VP = 'V NP',
  Art= 'the | a',
  N  = 'man | ball | woman | table',
  V  = 'hit | took | saw | liked'
  )

def generate(symbol='S'):
  "Replace symbol with a random entry in grammar (recursively); join into a string."
  if symbol not in grammar:
    return symbol
  else:
    return ' '.join(map(generate, random.choice(grammar[symbol])))

def generate_tree(symbol='S'):
  "Replace symbol with a random entry in grammar (recursively); return a tree."
  if symbol not in grammar:
    return symbol
  else:
    return {symbol: map(generate_tree, random.choice(grammar[symbol]))}
