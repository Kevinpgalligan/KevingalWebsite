title: Simulating plane designs for faster boarding
date: 2019-09-07

In the process of boarding a plane, you find yourself standing behind a man so unwashed that you imagine seeing a cartoonish, green stink cloud emanating from his trenchcoat. In front of him, a crouched old woman struggles futilely to lift her humongous suitcases, each of which is large enough to fit her inside them, into the luggage rack. Behind you is a red-eyed, defeated mother, who clutches a baby that screeches like an untuned violin. You think to yourself: "there has to be a faster way to get through this hell". Then the baby vomits into your backpack.

### A faster way
Consider a typical plane in Europe, such as Ryanair's Boeing 737-800. Two entrances for passengers, one at each end of the plane. About 32 rows of seats give a total of approximately 200 seats, and they're split by a single aisle.

During boarding, passengers are divided into 2 groups\*: those whose seats are closer to the rear entrance, who board through the rear entrance; and another group whose seats are closer to the front entrance. This makes boarding more efficient because the groups don't have to climb over each other to get to their places.

(\*Discounting priority boarding for simplification).

Here's an idea: what if we moved the entrances to 1/4 and 3/4 of the way through the plane, respectively, rather than placing them at the ends. Pre-boarding, passengers would still be sorted into 2 groups based on how close their seats were to the entrances. During boarding, however, they would effectively be split into 4 groups: those who turned left at the 1st entrance, those who turned right at the 1st entrance, those who turned left at the 2nd entrance, and those who turned right at the 2nd entrance.

![diagram demonstrating passenger grouping]({{ url_for('static', filename='img/plane-design.png') }})

This would further reduce the interference between passengers, and, in theory, further reduce boarding times. We're going to run an extremely simple simulation in Python to test this theory.

### Simulating it
Here's an animated representation of the simulation, created using the pygame library. The simulation runs in a series of "time steps". It takes 1 time step for a passenger to move between rows, and 5 time steps for a passenger to put their luggage into the rack and take their seat. These numbers were chosen pretty much arbitrarily. Reality would be more chaotic and teeth-grindingly frustrating. First we run a simulation with entrances at the ends of the plane. Then we run a version with centered entrances.

<iframe width="560" height="315" src="https://www.youtube.com/embed/lPdf7RwOlSE?rel=0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

In this case the entrances-at-ends design takes 239 time steps before everyone is seated, while the centered-entrances design takes 209 time steps, which is 12% less.

### Further testing and results
Just to be scientific about it, we can run both versions of the simulation 10,000 times and see what the average number of time steps is for each. Here are the results in graph form:

![results graph]({{ url_for('static', filename='img/plane-simulation-results-graph.png') }})

The centered-entrances design is, on average, ~15% faster in this run of the simulation.

Of course, there may be engineering or safety issues that prevent plane manufacturers from playing around with their plane designs like this. Perhaps placing the entrances at the ends gives space for more seats. Perhaps the wings and emergency exits get in the way of centered entrances. Etcetera.

Whatever the case, it's a nice distraction to think about as the trenchcoat stink cloud wafts its way towards you and the baby in your vicinity makes threatening heaving motions.

### Appendix A: follow-up reading
Further thoughts on the efficiency of plane boarding can be enjoyed in this [fun, informative little video by CPG Gray](https://www.youtube.com/watch?v=oAHbLRjF0vo), which doesn't mention plane design but discusses a number of algorithms for faster boarding.

### Appendix B: the code
For the curious, here's the 148-line script used to run the simulation.

```
import random
import time
import pygame
import sys

ROWS = 32
SEATS = "ABCDEF"
TICKS_REQUIRED_TO_PUT_AWAY_BAG = 5

RECTANGLE_SIZE = 20
GAP_BETWEEN_RECTS = 6
SCREEN_SIZE = (width, height) = (
    ROWS * (RECTANGLE_SIZE + GAP_BETWEEN_RECTS) + GAP_BETWEEN_RECTS,
    (len(SEATS) + 1) * (RECTANGLE_SIZE + GAP_BETWEEN_RECTS) + GAP_BETWEEN_RECTS)
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
    sim = Simulation([0, ROWS - 1])

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                print(str(sim.tick_counter) + " ticks elapsed.")
                sys.exit()
        if not sim.finished():
            time.sleep(0.15)
            sim.tick()
        sim.render(screen)
        pygame.display.flip()

if __name__ == "__main__":
    main()
```
