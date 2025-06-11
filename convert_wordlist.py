import re

input_file = r'C:\Users\franc\Documents\GitHub\wordle-prolog-haskell\hurdle\wordlist.pl'

output_file = r'C:\Users\franc\Documents\GitHub\wordle-prolog-haskell\meowdle\wordlist.hs'

with open(input_file, 'r') as f:
    lines = f.readlines()

words = []
for line in lines:
    match = re.match(r"word\(\[\s*('?[a-z]'?(,\s*'?[a-z]'?)*)\s*\]\)\.", line.strip())
    if match:
        letters = re.findall(r"'([a-z])'", match.group(1))
        word = ''.join(letters)
        if word:  
            words.append(word)

with open(output_file, 'w') as f:
    f.write("-- Auto-generated from wordlist.pl\n")
    f.write("module WordList where\n\n")
    f.write("wordList :: [String]\n")
    f.write("wordList = [\n")
    for i, word in enumerate(words):
        sep = ',' if i != len(words) - 1 else ''
        f.write(f'  "{word}"{sep}\n')
    f.write("]\n")

print(f"Converted {len(words)} words to Haskell list.")
