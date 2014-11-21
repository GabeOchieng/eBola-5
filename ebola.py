from erlport.erlterms import Atom
from erlport.erlang import call, cast, set_message_handler

def handler(message):

    f = open('messages', 'a')
    f.write(str(message))

def run(ServerPID):
    set_message_handler(handler)
    Names = ["Harry", "FuckFace", "ShitEater", "DumbFuckingFuck"]
    Health = [Atom("dormant"), Atom("clean"), Atom("clean"), Atom("clean")]
    Coords = [(0, 0), (1, 0), (0, 1), (1, 1)]
    DiseaseParams = (1, .5)
    cast(ServerPID, (Atom("initial_settings"), Names, Health, Coords, DiseaseParams))