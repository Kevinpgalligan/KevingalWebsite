import itertools

from PIL import Image
import cairo

POTION_WIDTH = 25
MAX_POTION_HEIGHT = 40
INITIAL_X_POTION_OFFSET = 10
INITIAL_Y_POTION_OFFSET = 10
LAYOUT_SOLUTION_X_PADDING = 30
POTION_X_PADDING = 12
POTION_Y_PADDING = 10
SEVEN_POTIONS_WIDTH = 7 * (POTION_WIDTH + POTION_X_PADDING)

WIDTH = 2 * INITIAL_X_POTION_OFFSET + 2 * SEVEN_POTIONS_WIDTH + LAYOUT_SOLUTION_X_PADDING
HEIGHT = 5230

SMALL_POTION = Image.open("small-potion.png")
NORMAL_POTION = Image.open("normal-potion.png")
BIG_POTION = Image.open("big-potion.png")

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

class PotionSize:
    SMALLEST = "S"
    NORMAL = "N"
    BIGGEST = "B"

POSSIBLE_POTION_SIZES = (PotionSize.SMALLEST, PotionSize.NORMAL, PotionSize.BIGGEST)
POTION_SIZE_TO_IMAGE = dict(zip(POSSIBLE_POTION_SIZES, (SMALL_POTION, NORMAL_POTION, BIG_POTION)))

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
    for i, p in enumerate(ps):
        draw_potion(cr, x + i * (POTION_WIDTH + POTION_X_PADDING), y, p)

def draw_potion(cr, x, y, p):
    img = POTION_SIZE_TO_IMAGE[p.size]
    y += MAX_POTION_HEIGHT - img.size[1]
    for row_idx in range(img.size[1]):
        for col_idx in range(img.size[0]):
            r, g, b, _ = img.getpixel((col_idx, row_idx))
            cr.set_source_rgb(*map_to_correct_colour(r, g, b, p.contents))
            cr.rectangle(x + col_idx, y + row_idx, 1, 1)
            cr.fill()

LIGHT_COLOUR = (191, 47, 244)
DARK_COLOUR = (144, 43, 181)

LIGHT_COLOUR_SUBSTITUTE = {
    PotionContents.MOVE_FORWARD: (158, 89, 255),
    PotionContents.MOVE_BACKWARD: (89, 172, 255), 
    PotionContents.POISON: (62, 222, 65),
    PotionContents.HARMLESS: (250, 156, 62),
    PotionContents.UNKNOWN: (255, 255, 255)
}

DARK_COLOUR_SUBSTITUTE = {
    PotionContents.MOVE_FORWARD: (106, 0, 255),
    PotionContents.MOVE_BACKWARD: (31, 143, 255), 
    PotionContents.POISON: (2, 186, 5),
    PotionContents.HARMLESS: (207, 118, 29),
    PotionContents.UNKNOWN: (255, 255, 255)
}

def map_to_correct_colour(r, g, b, potion_contents):
    rgb = (r, g, b)
    if rgb == LIGHT_COLOUR:
        rgb = LIGHT_COLOUR_SUBSTITUTE[potion_contents]
    elif rgb == DARK_COLOUR:
        rgb = DARK_COLOUR_SUBSTITUTE[potion_contents]
    return tuple(n / 255. for n in rgb)

def main():
    solutions_by_layout = find_solutions_by_layout()

    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
    cr = cairo.Context(surface)

    # Paint background white.
    cr.set_source_rgb(255, 255, 255)
    cr.rectangle(0, 0, WIDTH, HEIGHT)
    cr.fill()

    x, y = INITIAL_X_POTION_OFFSET, INITIAL_Y_POTION_OFFSET
    for (l, ss) in solutions_by_layout:
        draw_potions(cr, x, y, l)
        x += 7 * (POTION_WIDTH + POTION_X_PADDING) + LAYOUT_SOLUTION_X_PADDING
        if ss:
            draw_potions(cr, x, y, ss[0])
            for s in ss[1:]:
                y += MAX_POTION_HEIGHT + POTION_Y_PADDING
                draw_potions(cr, x, y, s)
        else:
            cr.set_source_rgb(0, 0, 0)
            cr.select_font_face("Liberation Mono", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
            cr.set_font_size(MAX_POTION_HEIGHT - 10)
            cr.move_to(x + WIDTH // 8, y + MAX_POTION_HEIGHT - 5)
            cr.show_text("None.")
        x = INITIAL_X_POTION_OFFSET
        y += MAX_POTION_HEIGHT + POTION_Y_PADDING

    surface.write_to_png("potions-puzzle-solutions.png")

if __name__ == "__main__":
    main()
