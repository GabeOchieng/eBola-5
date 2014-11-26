import sys, getopt
import random
import pygame
import eztext
import multiprocessing
from erlport.erlterms import Atom
from erlport.erlang import call, cast, set_message_handler

global_q = multiprocessing.Queue()

BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
RED = (255, 0, 0)
NO_PERSON_COLOR = (250, 250, 250)
CLEAN_COLOR = (0, 255, 0)
DORMANT_COLOR = (255, 190, 190)
SICK_COLOR = (255, 140, 140)
TERMINAL_COLOR = (255, 100, 100)
DEAD_COLOR = (0, 0, 0)

class Patient():

    def __init__(self, x, y, i, j, width, height, name):
        self.coords = (i, j)
        self.outline = pygame.Rect(x, y, width, height)
        self.state = 0 
        self.name = name
        # 0 is clean, 1 is dormant, 2 is sick, 3 is terminal, 4 is dead.

    def draw(self, screen):
        color = NO_PERSON_COLOR
        if (self.state == 1):
            color = CLEAN_COLOR
        elif (self.state == 2):
            color = DORMANT_COLOR
        elif (self.state == 3):
            color = SICK_COLOR
        elif (self.state == 4):
            color = TERMINAL_COLOR
        elif (self.state == 5):
            color = DEAD_COLOR

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

    def setHealth(self, Health):
        self.state = Health

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

    def update(self, Name, Health):
        for patient in self.patients:
            if patient.getName() == Name:
                patient.setHealth(Health)

#Global bc handler for ErlPort exists at the module level.
map = Map(50, 50, 400, 400)

def handler(message):
    (MessageAtom, Name, Health) = message
    global_q.put((Name, Health))

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
        return self.textbox.get_text()

class Button():
    def __init__(self, x, y, width, height, text, map, ticktime, strength):
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        self.outline = pygame.Rect(x, y, width, height)
        self.font = pygame.font.SysFont("monospace", 15)
        self.text = text
        self.strength = strength
        self.ticktime = ticktime

    def draw(self, screen):
        pygame.draw.rect(screen, BLACK, self.outline, 0)
        label = self.font.render(self.text, 1, (255, 255, 0))
        screen.blit(label, (self.x + self.width/5, self.y + self.height/3))

    def clicked(self):
        if (self.outline.collidepoint(pygame.mouse.get_pos())):
            (Names, Coords, Health) = map.getInfo()
            global_q.put((Names, Health, Coords, (int(self.ticktime.getVal()), float(self.strength.getVal()))))
            self.outline = pygame.Rect(0,0,0,0)
            self.text = ""
            return True
            #Supposedly this would talk to the backend to start the simulation

def run_frontend():

    done = False

    pygame.init()
    screen = pygame.display.set_mode([700, 600])

    clock = pygame.time.Clock()

    ticktime = TextBox(485, 150, 'Tick time: ')
    strength = TextBox(485, 200, 'Strength: ')
    button = Button(150, 500, 200, 40, "Run Simulation", map, ticktime, strength)

    break_flag = False
    while not done:
        if break_flag:
            break
        clock.tick(30)

        events = pygame.event.get()

        for event in events:
            if event.type == pygame.QUIT:
                done = True
            if event.type == pygame.MOUSEBUTTONDOWN:
                ticktime.change_focus()
                strength.change_focus()
                if button.clicked():
                    break_flag = True
                map.clicked()

        screen.fill(WHITE)

        map.draw(screen)

        ticktime.update(events)
        ticktime.draw(screen)

        strength.update(events)
        strength.draw(screen)

        button.draw(screen)

        pygame.display.flip()
        
    while not done:
        clock.tick(30)

        events = pygame.event.get()

        for event in events:
            if event.type == pygame.QUIT:
                done = True

        screen.fill(WHITE)

        (Name, Health) = global_q.get()
        map.update(Name, Health)

        map.draw(screen)

        ticktime.update(events)
        ticktime.draw(screen)

        strength.update(events)
        strength.draw(screen)

        button.draw(screen)
        pygame.display.flip()

    pygame.quit()

def main(ServerPID):
    p = multiprocessing.Process(target=run_frontend, args=())
    p.start()
    (Names, Health, Coords, DiseaseParams) = global_q.get()
    cast(ServerPID, (Atom("initial_settings"), Names, Health, Coords, DiseaseParams))

if __name__ == '__main__':
    main()