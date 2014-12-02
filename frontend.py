# frontend.py
# eBola
# 
# COMP 50 - Mark Sheldon
# Hyung-Seo Park, Robert Ruenes, and Paul Chang
# 
# This is the front end of our ebola simulation. The front end uses pygame for
# the graphical user interface and uses Erlport to communicate to a back end
# written in erlang. 
# The main process is booted by an erlang server. This main process kicks off
# another process which is where the gui written with pygame exists in. The 
# first process acts as an intermediate of communication between the front end
# in python and backend server in erlang.
#

import sys, getopt
import random
import pygame
import eztext
import multiprocessing
from erlport.erlterms import Atom
from erlport.erlang import call, cast, set_message_handler

# Queue used for message passing between python processes
global_q = multiprocessing.Queue()

# Color constants
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
RED = (255, 0, 0)
NO_PERSON_COLOR = (250, 250, 250)
CLEAN_COLOR = (0, 255, 0)
DORMANT_COLOR = (255, 190, 190)
SICK_COLOR = (255, 140, 140)
TERMINAL_COLOR = (255, 100, 100)
DEAD_COLOR = (0, 0, 0)

# Screen size constant
SCREEN_SIZE = 700

# Patient class represents a single patient
class Patient():

    #(x,y) is the pixel position
    #(i,j) is the patient position
    #width, height are of the patient's square
    #name is the unique identifier
    def __init__(self, x, y, i, j, width, height, name):
        self.coords = (i, j)
        self.outline = pygame.Rect(x, y, width, height)
        # 0 is no person, 1 is clean, 2 is dormant, 3 is sick, 
        # 4 is terminal, 5 is dead.. start at state 0
        self.state = 0 
        self.name = name

    # Draw patient square with color representing the state
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

    # If a patient is clicked, then it will increment its state
    def clicked(self):
        if (self.outline.collidepoint(pygame.mouse.get_pos())): 
            self.state = (self.state + 1) % 6

    # Returns a tuple of (Name, Coordinates, Health)
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

    # Sets the health of a patient. Used to update the patient
    def setHealth(self, Health):
        self.state = Health
        
    # Used to enlarge the patients to fullscreen
    def new_pos_and_size(self, x, y, width, height):
        self.outline = pygame.Rect(x, y, width, height)

# Map class represents the simulation and includes 25 patient objects
class Map():

    # (x,y) pixel position of the patient map
    # width height are the map's width and height
    def __init__(self, x, y, width, height):
        self.x = x
        self.y = y
        self.outline = pygame.Rect(x, y, width, height)
        self.offset = 5
        self.patients = []

        patient_width = ((width - self.offset) / 5) - self.offset
        patient_height = ((height - self.offset) / 5) - self.offset

        cur_patient_x = self.offset
        cur_patient_y = self.offset

        Letter = 'a'

        #initialize 25 patients
        for i in range(0, 5):
            for j in range (0, 5):
                self.patients.append(Patient(x + cur_patient_x, 
                                             y + cur_patient_y, i, j, 
                                             patient_width, patient_height, 
                                             Letter))
                Letter = chr(ord(Letter) + 1)
                cur_patient_x = cur_patient_x + (self.offset + patient_width)
            cur_patient_x = self.offset
            cur_patient_y = cur_patient_y + (self.offset + patient_height)              

    # Draw calls all of patient's draw routines.
    def draw(self, screen):
        pygame.draw.rect(screen, BLACK, self.outline, 1)
        for patient in self.patients:
            patient.draw(screen)

    # Clicked calls each of the patient's clicked routines
    def clicked(self):
        for patient in self.patients:
            patient.clicked()

    # Calls getInfo on each patient and returns a compiled list
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

    # Update function when erlang produces a state change
    def update(self, Name, Health):
        for patient in self.patients:
            if patient.getName() == Name:
                patient.setHealth(Health)

    # Enlarge the map to full screen when the simulation is started.
    def enlarge(self):
        self.outline = pygame.Rect(self.x, self.y, SCREEN_SIZE - (2 * self.x),
                                                   SCREEN_SIZE - (2 * self.x))
        self.width = self.height = SCREEN_SIZE - (2 * self.x)

        patient_width = ((self.width - self.offset) / 5) - self.offset
        patient_height = ((self.height - self.offset) / 5) - self.offset

        cur_patient_x = self.offset
        cur_patient_y = self.offset

        count = 0
        
        # update each patient's position and size
        for i in range(0, 5):
            for j in range (0, 5):
                self.patients[count].new_pos_and_size(self.x + cur_patient_x, 
                                                      self.y + cur_patient_y, 
                                                      patient_width, 
                                                      patient_height)
                count += 1
                cur_patient_x = cur_patient_x + (self.offset + patient_width)
            cur_patient_x = self.offset
            cur_patient_y = cur_patient_y + (self.offset + patient_height)   

#Global bc handler for ErlPort exists at the module level.
map = Map(50, 50, 400, 400)

# Handler method for erlang server to communicate with. 
def handler(message):
    (MessageAtom, Name, Health) = message
    global_q.put((Name, Health))

# Set the message handler for erlport
set_message_handler(handler)

# Custom Textbox class for inputs
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
    
    # Returns the value that the user typed in
    def getVal(self):
        return self.textbox.get_text()

    # Hides the text box when the simulation is started
    def disappear(self):
        self.outline = pygame.Rect(0,0,0,0)
        self.textbox = eztext.Input(maxlength=5, color=RED, prompt="")

# Custom Button class used to start the simulation
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

    # Simulation starts.
    def clicked(self):
        if (self.outline.collidepoint(pygame.mouse.get_pos())):
            # Grab the initial states of each person in the simulation
            (Names, Coords, Health) = map.getInfo()
            # Put it into the Q so that the intermediate python process can 
            # pass it to the erlang backend server.
            global_q.put((Names, Health, Coords, (int(self.ticktime.getVal()),
                                              float(self.strength.getVal()))))
            # Hide the button
            self.outline = pygame.Rect(0,0,0,0)
            self.text = ""

            # Hide the textboxs
            self.ticktime.disappear()
            self.strength.disappear()
            
            # Make the simulation full screen
            map.enlarge()
            return True

# Front end process function 
def run_frontend():

    done = False
    #initialize pygame
    pygame.init()
    screen = pygame.display.set_mode([SCREEN_SIZE, SCREEN_SIZE])

    clock = pygame.time.Clock()
    #create forms and buttons
    ticktime = TextBox(485, 150, 'Tick time: ')
    strength = TextBox(485, 200, 'Strength: ')
    button = Button(150, 500, 200, 40, "Run Simulation", map, ticktime, strength)

    break_flag = False
    while not done:
        if break_flag:
            break
        clock.tick(30)

        events = pygame.event.get()
        #check whether there is a click
        for event in events:
            if event.type == pygame.QUIT:
                done = True
            if event.type == pygame.MOUSEBUTTONDOWN:
                ticktime.change_focus()
                strength.change_focus()
                if button.clicked(): 
                    break_flag = True
                button.clicked()
                map.clicked()
        #draw routine
        screen.fill(WHITE)

        map.draw(screen)

        ticktime.update(events)
        ticktime.draw(screen)

        strength.update(events)
        strength.draw(screen)

        button.draw(screen)

        pygame.display.flip()
    #Once the button is cicked, go into a while loop that checks for updates
    while not done:
        clock.tick(30)
        
        events = pygame.event.get()

        for event in events:
            if event.type == pygame.QUIT:
                done = True

        screen.fill(WHITE)
        #Block until we get an update from the backend
        (Name, Health) = global_q.get()
        #update the state of the patient
        map.update(Name, Health)
        #draw routine
        map.draw(screen)

        ticktime.update(events)
        ticktime.draw(screen)

        strength.update(events)
        strength.draw(screen)

        button.draw(screen)
        pygame.display.flip()

    pygame.quit()

#Intermediate python process given the erlang backend id to send back initial
#parameters from the gui.
def main(ServerPID):
    #kick off the front end process
    p = multiprocessing.Process(target=run_frontend, args=())
    p.start()
    #wait for the user to press start simulation and get the initial states
    (Names, Health, Coords, DiseaseParams) = global_q.get()
    #send data back to erlang backend to start the simulation
    cast(ServerPID, 
        (Atom("initial_settings"), Names, Health, Coords, DiseaseParams))

if __name__ == '__main__':
    main()