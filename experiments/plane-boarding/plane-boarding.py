import random
import time
import pygame
import sys

ROWS = 32
SEATS = "ABCDEF"
TICKS_REQUIRED_TO_PUT_AWAY_BAG = 5

RECTANGLE_SIZE = 20
GAP_BETWEEN_RECTS = 6
SPACE_FOR_BUTTON = 40
BUTTON_SIZE = 0.8*SPACE_FOR_BUTTON
SCREEN_SIZE = (SCREEN_WIDTH, SCREEN_HEIGHT) = (
    ROWS * (RECTANGLE_SIZE + GAP_BETWEEN_RECTS) + GAP_BETWEEN_RECTS,
    (len(SEATS) + 1) * (RECTANGLE_SIZE + GAP_BETWEEN_RECTS) + GAP_BETWEEN_RECTS + SPACE_FOR_BUTTON)
BACKGROUND_COLOUR = (255, 255, 255)
PASSENGER_COLOUR = (255, 0, 0)
SEAT_COLOUR = (0, 0, 255)
FLOOR_COLOUR = (172, 172, 172)

class Simulation:
    def __init__(self, entrances):
        self.entrance_to_waiting_passengers = {}
        for entrance in entrances:
            self.entrance_to_waiting_passengers[entrance] = []
        for row in range(ROWS):
            nearest_entrance = min(entrances, key=lambda e: abs(e - row))
            for seat in SEATS:
                self.entrance_to_waiting_passengers[nearest_entrance].append(Passenger(row, seat))
        for waiting_passengers in self.entrance_to_waiting_passengers.values():
            random.shuffle(waiting_passengers)
        self.active_passengers = []
        self.aisle = [None for _ in range(ROWS)]
        self.occupied_seats = set()
        self.tick_counter = 0

    def tick(self):
        self.move_passengers()
        self.add_waiting_passengers()
        self.tick_counter += 1

    def move_passengers(self):
        new_active_passengers = []
        for passenger in self.active_passengers:
            passenger.waiting_time += 1
            if passenger.position == passenger.row:
                passenger.ticks_spent_putting_away_bag += 1
                if passenger.ticks_spent_putting_away_bag >= TICKS_REQUIRED_TO_PUT_AWAY_BAG:
                    self.occupied_seats.add((passenger.row, passenger.seat))
                    self.aisle[passenger.position] = None
                else:
                    new_active_passengers.append(passenger)
            else:
                new_position = (passenger.position + 1
                    if passenger.position < passenger.row
                    else passenger.position - 1)
                if self.aisle[new_position] is None:
                    self.aisle[passenger.position] = None
                    self.aisle[new_position] = passenger
                    passenger.position = new_position
                new_active_passengers.append(passenger)
        self.active_passengers = new_active_passengers

    def add_waiting_passengers(self):
        for entrance, waiting_passengers in self.entrance_to_waiting_passengers.items():
            if waiting_passengers and self.aisle[entrance] is None:
                new_passenger = waiting_passengers.pop()
                new_passenger.position = entrance
                self.aisle[entrance] = new_passenger
                self.active_passengers.append(new_passenger)

    def finished(self):
        return not (any(waiting_passengers
                for waiting_passengers in self.entrance_to_waiting_passengers.values())
            or self.active_passengers)

    def render(self, screen):
        screen.fill(BACKGROUND_COLOUR)
        x, y = GAP_BETWEEN_RECTS, GAP_BETWEEN_RECTS
        drawer = RectDrawer(screen, GAP_BETWEEN_RECTS, GAP_BETWEEN_RECTS)
        for seat in SEATS[:len(SEATS) // 2]:
            for row in range(ROWS):
                drawer.draw(PASSENGER_COLOUR if (row, seat) in self.occupied_seats else SEAT_COLOUR)
            drawer.advance_row()
        for aisle_position in self.aisle:
            drawer.draw(FLOOR_COLOUR if aisle_position is None else PASSENGER_COLOUR)
        drawer.advance_row()
        for seat in SEATS[len(SEATS) // 2:]:
            for row in range(ROWS):
                drawer.draw(PASSENGER_COLOUR if (row, seat) in self.occupied_seats else SEAT_COLOUR)
            drawer.advance_row()

class Passenger:
    def __init__(self, row, seat):
        self.waiting_time = 0
        self.row = row
        self.seat = seat
        self.position = None
        self.ticks_spent_putting_away_bag = 0

    def __str__(self):
        return str(self.__dict__)

    def __repr__(self):
        return self.__str__()

class RectDrawer:
    def __init__(self, screen, initial_x, initial_y):
        self.screen = screen
        self.initial_x = initial_x
        self.x = initial_x
        self.y = initial_y

    def advance_row(self):
        self.x = self.initial_x
        self.y += RECTANGLE_SIZE + GAP_BETWEEN_RECTS

    def draw(self, colour):
        pygame.draw.rect(
            self.screen,
            colour,
            (self.x, self.y, RECTANGLE_SIZE, RECTANGLE_SIZE),
            0)
        self.x += RECTANGLE_SIZE + GAP_BETWEEN_RECTS

def main():
    pygame.init()
    screen = pygame.display.set_mode(SCREEN_SIZE)
    # sim = Simulation([0, ROWS-1])
    sim = Simulation([ROWS//4, ROWS//4*3])
    
    paused = True
    while True:
        sim.render(screen)
        if paused:
            button = pygame.draw.polygon(
                screen,
                (0, 255, 0),
                [(SCREEN_WIDTH//2, SCREEN_HEIGHT-0.1*SPACE_FOR_BUTTON),
                 (SCREEN_WIDTH//2, SCREEN_HEIGHT-0.9*SPACE_FOR_BUTTON),
                 (SCREEN_WIDTH//2+BUTTON_SIZE,
                  SCREEN_HEIGHT-0.5*SPACE_FOR_BUTTON)],
                0)
        else:
            button = pygame.draw.rect(
                screen,
                (255, 0, 0),
                (SCREEN_WIDTH//2 - BUTTON_SIZE//2,
                 SCREEN_HEIGHT-0.9*SPACE_FOR_BUTTON,
                 BUTTON_SIZE,
                 BUTTON_SIZE),
                0)

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                print(str(sim.tick_counter) + " ticks elapsed.")
                sys.exit()
            if event.type == pygame.MOUSEBUTTONUP:
                if button.collidepoint(pygame.mouse.get_pos()):
                    paused = not paused
        if not paused and not sim.finished():
            time.sleep(0.15)
            sim.tick()
        pygame.display.flip()

if __name__ == "__main__":
    main()
