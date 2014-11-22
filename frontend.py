
import sys, getopt
import random
import pygame
import eztext

BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
RED = (255, 0, 0)
BLUE = (0, 0, 255)
GREEN = (0, 255, 0)
YELLOW = (255, 255, 0)

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

        for i in range(0, 5):
            for j in range (0, 5):
                self.patients.append(Patient(x + cur_patient_x, y + cur_patient_y, patient_width, patient_height))
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

class Patient():

    def __init__(self, x, y, width, height):
        self.outline = pygame.Rect(x, y, width, height)
        self.state = 0 
        # 0 is clean, 1 is dormant, 2 is sick, 3 is terminal, 4 is dead.

    def draw(self, screen):
        color = BLACK
        if (self.state == 1):
            color = RED
        elif (self.state == 2):
            color = BLUE
        elif (self.state == 3):
            color = GREEN
        elif (self.state == 4):
            color = YELLOW

        pygame.draw.rect(screen, color, self.outline, 0)

    def clicked(self):
        if (self.outline.collidepoint(pygame.mouse.get_pos())): 
            self.state = (self.state + 1) % 5

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

class Button():
    def __init__(self, x, y, width, height, text, func):
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        self.outline = pygame.Rect(x, y, width, height)
        self.font = pygame.font.SysFont("monospace", 15)
        self.text = text
        self.func = func

    def draw(self, screen):
        pygame.draw.rect(screen, BLACK, self.outline, 0)
        label = self.font.render(self.text, 1, (255, 255, 0))
        screen.blit(label, (self.x + self.width/5, self.y + self.height/3))

    def clicked(self):
        if (self.outline.collidepoint(pygame.mouse.get_pos())):
            self.func() #Supposedly this would talk to the backend to start the simulation

def main():
    pygame.init()
    screen = pygame.display.set_mode([700, 800])

    clock = pygame.time.Clock()
    done = False

    map = Map(50, 50, 400, 400)

    ticktime = TextBox(485, 150, 'Tick time: ')
    strength = TextBox(485, 200, 'Strength: ')
    button = Button(150, 500, 200, 40, "Run Simulation", test)

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
        

    pygame.quit()

def test():
    return 5

if __name__ == '__main__':
    main()