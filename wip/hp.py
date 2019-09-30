import itertools

import cairo

WIDTH, HEIGHT = 1080, 2160
POTION_WIDTH = 5
INITIAL_X_POTION_OFFSET = 10
INITIAL_Y_POTION_OFFSET = 60
LAYOUT_SOLUTION_X_PADDING = 10
POTION_X_PADDING = 10
POTION_Y_PADDING = 10

def poisonous_to_left_of_harmless(potions):
    for index, potion in enumerate(potions):
        if (potion.contents == PotionContents.HARMLESS and
                (index == 0 or potions[index - 1].contents not in [PotionContents.POISON, PotionContents.UNKNOWN])):
            return False
    return True

def ends_are_different(potions):
    return (potions[0].contents == PotionContents.UNKNOWN
        or potions[-1].contents == PotionContents.UNKNOWN
        or potions[0].contents != potions[-1].contents)

def ends_do_not_allow_move_forward(potions):
    return not any(PotionContents.MOVE_FORWARD == potion.contents
        for potion in (potions[0], potions[-1]))

def seconds_from_ends_are_same(potions):
    return (potions[1].contents == PotionContents.UNKNOWN
        or potions[-2].contents == PotionContents.UNKNOWN
        or potions[1].contents == potions[-2].contents)

def biggest_and_smallest_not_poisonous(potions):
    for potion in potions:
        if potion.size in [PotionSize.SMALLEST, PotionSize.BIGGEST] and potion.contents == PotionContents.POISON:
            return False
    return True

def correct_quantities(potions):
    contents = [potion.contents for potion in potions]
    return (contents.count(PotionContents.MOVE_FORWARD) <= 1
        and contents.count(PotionContents.MOVE_BACKWARD) <= 1
        and contents.count(PotionContents.POISON) <= 3
        and contents.count(PotionContents.HARMLESS) <= 2)

CONDITIONS = [
    poisonous_to_left_of_harmless,
    ends_are_different,
    ends_do_not_allow_move_forward,
    seconds_from_ends_are_same,
    biggest_and_smallest_not_poisonous,
    correct_quantities
]

def passes_conditions(potions):
    return all(condition(potions) for condition in CONDITIONS)

def is_valid_assignment(potions, index, contents):
    potions[index].contents = contents
    is_valid = passes_conditions(potions)
    potions[index].contents = PotionContents.UNKNOWN
    return is_valid

def all_assigned(potions):
    return all(potion.contents != PotionContents.UNKNOWN for potion in potions)

class PotionContents:
    MOVE_FORWARD = "MO"
    MOVE_BACKWARD = "MB"
    POISON = "P"
    HARMLESS = "H"
    UNKNOWN = "U"

POSSIBLE_POTION_CONTENTS = (
    PotionContents.MOVE_FORWARD,
    PotionContents.MOVE_BACKWARD,
    PotionContents.POISON,
    PotionContents.HARMLESS
)

COLOURS = [
    (100,   0, 180), # purple
    (  0,  90, 180), # blue
    (255,   0,   0), # red
    (200, 255,   0)  # vomit
]
POTION_CONTENTS_TO_COLOUR = dict(zip(POSSIBLE_POTION_CONTENTS, COLOURS))

class PotionSize:
    NORMAL = "N"
    SMALLEST = "S"
    BIGGEST = "B"

POSSIBLE_POTION_SIZES = (PotionSize.NORMAL, PotionSize.SMALLEST, PotionSize.BIGGEST)
POTION_SIZE_TO_HEIGHT = dict(zip(POSSIBLE_POTION_SIZES, [6, 4, 8]))
MAX_POTION_HEIGHT = max(POTION_SIZE_TO_HEIGHT.values())

class Potion:
    def __init__(self, size, contents=PotionContents.UNKNOWN):
        self.size = size
        self.contents = contents

    def __eq__(self, other):
        return self.size == other.size and self.contents == other.contents

    def __hash__(self):
        return hash((self.size, self.contents))

    def __str__(self):
        return self.size + "+" + self.contents

    def __repr__(self):
        return self.__str__()

    def copy(self):
        return Potion(self.size, self.contents)

def copy_of_potions(potions):
    return [potion.copy() for potion in potions]

def solve(potions):
    possible_contents = [(index, POSSIBLE_POTION_CONTENTS) for index in range(len(potions))]
    for solution in solve_aux(potions, possible_contents):
        yield solution

def solve_aux(potions, possible_contents):
    if all_assigned(potions):
        yield copy_of_potions(potions)
        return
    new_possible_contents = []
    for index, potion in enumerate(potions):
        if potion.contents == PotionContents.UNKNOWN:
            potion_possible_contents = tuple(
                contents for contents in dict(possible_contents)[index]
                if is_valid_assignment(potions, index, contents)
            )
            if not potion_possible_contents:
                # Contradicts the riddle.
                return
            new_possible_contents.append((index, potion_possible_contents))
    if not new_possible_contents:
        # Still haven't solved it, and there are no new possible
        # assignments, so no way to continue.
        return
    # Try assignments to potion with fewest possible contents, optimizes
    # the search.
    new_possible_contents.sort(key=lambda x: len(x[1]))
    index, assignments_to_try = new_possible_contents[0]
    for assignment in assignments_to_try:
        potions[index].contents = assignment
        for solution in solve_aux(potions, new_possible_contents):
            yield solution
        potions[index].contents = PotionContents.UNKNOWN
        
def find_solutions_by_layout():
    potions = [
        Potion(PotionSize.SMALLEST),
        Potion(PotionSize.BIGGEST)
    ]
    potions += [Potion(PotionSize.NORMAL) for _ in range(5)]
    assert len(potions) == 7
    layouts = list(set(itertools.permutations(potions, len(potions))))
    result = []
    for layout in layouts:
        solutions = [s for s in solve(layout)]
        result.append((layout, solutions))
    return result

def draw_potions(cr, x, y, ps):
    cr.set_source_rgb(0, 0, 0)
    cr.rectangle(x, y, 7 * (POTION_WIDTH + POTION_X_PADDING), MAX_POTION_HEIGHT)
    cr.fill()

def main():
    solutions_by_layout = find_solutions_by_layout()

    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
    cr = cairo.Context(surface)

    # Paint background white.
    cr.set_source_rgb(255, 255, 255)
    cr.rectangle(0, 0, WIDTH, HEIGHT)
    cr.fill()

    cr.set_source_rgb(0, 0, 0)
    cr.select_font_face("Purisa", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
    cr.set_font_size(40)
    cr.move_to(60, 50)
    cr.show_text("Layouts")
    cr.move_to(WIDTH // 2 + 60, 50)
    cr.show_text("Solutions")

    x, y = INITIAL_X_POTION_OFFSET, INITIAL_Y_POTION_OFFSET
    for (l, ss) in solutions_by_layout:
        draw_potions(cr, x, y, l)
        x += 7 * (POTION_WIDTH + POTION_X_PADDING) + LAYOUT_SOLUTION_X_PADDING
        if ss:
            for s in ss:
                draw_potions(cr, x, y, s)
                y += MAX_POTION_HEIGHT + POTION_Y_PADDING
        else:
            cr.set_source_rgb(0, 0, 0)
            cr.select_font_face("Purisa", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
            cr.set_font_size(20)
            cr.move_to(x, y)
            cr.show_text("None!")
        x = INITIAL_X_POTION_OFFSET
        y += POTION_Y_PADDING

    surface.write_to_png("example.png")

if __name__ == "__main__":
    main()
