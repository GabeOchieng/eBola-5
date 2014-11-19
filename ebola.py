from erlport.erlterms import Atom
from erlport.erlang import call, cast, set_message_handler

def run(ServerPID):
    Names = ["Harry", "FuckFace", "ShitEater", "DumbFuckingFuck"]
    Health = [Atom("clean"), Atom("dormant"), Atom("clean"), Atom("clean")]
    Coords = [(0, 0), (1, 0), (0, 1), (1, 1)]
    DiseaseParams = (5, .5)
    cast(ServerPID, (Atom("initial_settings"), Names, Coords, DiseaseParams))