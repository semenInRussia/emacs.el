def alphabet_position(text):
    char_alphabet_position = lambda ch: str(ord(ch.lower()) - 96)
    return " ".join(map(char_alphabet_position,
                        filter(str.isalpha, text)))

