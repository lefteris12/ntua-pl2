#!/usr/bin/env python3

import sys
import requests
import re

morse_alphabet = {
   "·–" : "a",
   "–···" : "b",
   "–·–·" : "c",
   "–··" : "d",  
   "·" :   "e",  
   "··–·" : "f",  
   "––·" : "g",  
   "····" : "h",  
   "··" :  "i",  
   "·–––" : "j",  
   "–·–" : "k",  
   "·–··" : "l",  
   "––" : "m",  
   "–·" : "n",  
   "–––" : "o",  
   "·––·" : "p",  
   "––·–" : "q",  
   "·–·" : "r",  
   "···" : "s",  
   "–" :   "t",  
   "··–" : "u",  
   "···–" : "v",  
   "·––" : "w",  
   "–··–" : "x",  
   "–·––" : "y",  
   "––··" : "z"  
}

def decode(morse_str):
    words = morse_str.split('  ')
    decoded = ''
    for morse_word in words:
        if decoded != '':
            decoded += ' '
        for morse_letter in morse_word.split(' '):
            decoded += morse_alphabet[morse_letter]
    return decoded

def get_html(url, session):
    r = session.get(url)
    # Set the correct encoding
    r.encoding = r.apparent_encoding
    return r.text

def print_answer(html, url, session):
    # Find the answer
    code_tag = re.search('<code>(.*)</code>', html)
    encoded = code_tag.group(1)
    answer = decode(encoded)

    # Print the answer
    resp = session.post(url, data={'answer': answer})
    print(answer)

    # Check if finished
    secs = re.search('It took you .* seconds.', resp.text)
    if secs:
        print(secs.group(0))
        return True

    # Continue to the next question
    resp = session.post(url, data={'continue': 'continue'})
    return False

def play():
    session = requests.session()
    url = sys.argv[1]
    finished = False
    while not finished:
        html = get_html(url, session)
        finished = print_answer(html, url, session)

play()
