import sys, getopt
import random
import pygame
import eztext
from erlport.erlterms import Atom
from erlport.erlang import call, cast, set_message_handler

BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
RED = (255, 0, 0)
BLUE = (0, 0, 255)
GREEN = (0, 255, 0)
YELLOW = (255, 255, 0)

flag = False

class Patient():

    def __init__(self, x, y, i, j, width, height, name):
        self.coords = (i, j)
        self.outline = pygame.Rect(x, y, width, height)
        self.state = 0 
        self.name = name
        # 0 is clean, 1 is dormant, 2 is sick, 3 is terminal, 4 is dead.

    def draw(self, screen):
        color = WHITE
        if (self.state == 1):
            color = BLACK
        elif (self.state == 2):
            color = RED
        elif (self.state == 3):
            color = BLUE
        elif (self.state == 4):
            color = GREEN
        elif (self.state == 5):
            color = YELLOW

        pygame.draw.rect(screen, color, self.outline, 0)

    def clicked(self):
        if (self.outline.collidepoint(pygame.mouse.get_pos())): 
            self.state = (self.state + 1) % 6

    def getInfo(self):
        if self.state != 0:
            return (self.getName(), self.getCoords(), self.getHealth())
        else:
            return None

    def getName(self):
        return self.name

    def getCoords(self):
        return self.coords

    def getHealth(self):
        if (self.state == 1):
            return Atom("clean")
        elif (self.state == 2):
            return Atom("dormant")
        elif (self.state == 3):
            return Atom("sick")
        elif (self.state == 4):
            return Atom("terminal")
        elif (self.state == 5):
            return Atom("dead")

class Map():
    # Will contain the array of "patients".
    # patient cout limit is currently set to 25.

    def __init__(self, x, y, width, height):
        self.outline = pygame.Rect(x, y, width, height)
        self.offset = 5
        self.patients = []

        patient_width = ((width - self.offset) / 5) - self.offset
        patient_height = ((height - self.offset) / 5) - self.offset

        cur_patient_x = self.offset
        cur_patient_y = self.offset

        Letter = 'a'

        for i in range(0, 5):
            for j in range (0, 5):
                self.patients.append(Patient(x + cur_patient_x, y + cur_patient_y, i, j, patient_width, patient_height, Letter))
                Letter = chr(ord(Letter) + 1)
                cur_patient_x = cur_patient_x + (self.offset + patient_width)
            cur_patient_x = self.offset
            cur_patient_y = cur_patient_y + (self.offset + patient_height)              

    def draw(self, screen):
        pygame.draw.rect(screen, BLACK, self.outline, 1)
        for patient in self.patients:
            patient.draw(screen)

    def clicked(self):
        for patient in self.patients:
            patient.clicked()

    def getInfo(self):
        Names = []
        Coords = []
        Health = []
        for patient in self.patients:
            Info = patient.getInfo()
            if Info != None:
                (Name, Coord, PHealth) = Info
                Names.append(Name)
                Coords.append(Coord)
                Health.append(PHealth)

        return (Names, Coords, Health)

#Global bc handler for ErlPort exists at the module level.
map = Map(50, 50, 400, 400)

def handler(message):

    f = open('messages', 'a')
    f.write(str(message))

set_message_handler(handler)

class TextBox():
    def __init__(self, x, y, prompt):
        self.focus = False
        self.x = x
        self.y = y
        self.textbox = eztext.Input(maxlength=5, color=RED, prompt=prompt)
        self.textbox.set_pos(x, y)
        self.outline = pygame.Rect(x, y - 10, 200, 40)
        self.width = 1

    def draw(self, screen):
        pygame.draw.rect(screen, BLACK, self.outline, self.width)
        self.textbox.draw(screen)

    def update(self, events):
        if self.focus:
            self.textbox.update(events)

    def change_focus(self):
        if (self.outline.collidepoint(pygame.mouse.get_pos())):
            self.focus = True
            self.width = 2
        else:
            self.focus = False
            self.width = 1

    # need to know how to get value out of the real textboxes
    def getVal(self):
        return .5

class Button():
    def __init__(self, x, y, width, height, text, map, ticktime, strength, ServerPID):
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        self.outline = pygame.Rect(x, y, width, height)
        self.font = pygame.font.SysFont("monospace", 15)
        self.text = text
        self.strength = strength
        self.ticktime = ticktime
        self.ServerPID = ServerPID

    def draw(self, screen):
        pygame.draw.rect(screen, BLACK, self.outline, 0)
        label = self.font.render(self.text, 1, (255, 255, 0))
        screen.blit(label, (self.x + self.width/5, self.y + self.height/3))

    def clicked(self):
        if (self.outline.collidepoint(pygame.mouse.get_pos())):
            (Names, Coords, Health) = map.getInfo()
            DiseaseParams = (5, self.strength.getVal())
            cast(self.ServerPID, (Atom("initial_settings"), Names, Health, Coords, DiseaseParams))
            self.outline = pygame.Rect(0,0,0,0)
            self.text = ""
            flag = True
            #Supposedly this would talk to the backend to start the simulation

def main(ServerPID):
    Names = ["Harry", "FuckFace", "ShitEater", "DumbFuckingFuck"]
    Health = [Atom("dormant"), Atom("clean"), Atom("clean"), Atom("clean")]
    Coords = [(0, 0), (1, 0), (0, 1), (1, 1)]
    DiseaseParams = (1, .5)
    cast(ServerPID, (Atom("initial_settings"), Names, Health, Coords, DiseaseParams))

 #   cast(ServerPID, (Atom("initial_settings"), ["Rob", "Paul"], [Atom("dormant"), Atom("clean")], [(0, 1), (1,1)], (.5,.5)))
    #while True:
    #    True
    """pygame.init()
    screen = pygame.display.set_mode([700, 800])

    clock = pygame.time.Clock()
    done = False

    ticktime = TextBox(485, 150, 'Tick time: ')
    strength = TextBox(485, 200, 'Strength: ')
    button = Button(150, 500, 200, 40, "Run Simulation", map, ticktime, strength, ServerPID)

    while not done:
        clock.tick(30)

        events = pygame.event.get()

        for event in events:
            if event.type == pygame.QUIT:
                done = True
            if event.type == pygame.MOUSEBUTTONDOWN:
                ticktime.change_focus()
                strength.change_focus()
                button.clicked()
                map.clicked()

        screen.fill(WHITE)

        map.draw(screen)

        ticktime.update(events)
        ticktime.draw(screen)

        strength.update(events)
        strength.draw(screen)

        button.draw(screen)

        pygame.display.flip()
        
        if flag:
            done = True

    pygame.quit()"""

if __name__ == '__main__':
    main()