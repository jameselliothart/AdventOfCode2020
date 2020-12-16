DATA = [13,16,0,12,15,1]
SAMPLE = [0,3,6]

class Turn():
    def __init__(self, starting_numbers):
        self.current_round = len(starting_numbers) + 1
        self.last_spoken = starting_numbers[-1]
        self.previous_turns = {n:(i+1,0) for i,n in enumerate(starting_numbers)}

    def __repr__(self) -> str:
        return f'Turn(current_round={self.current_round}, last_spoken={self.last_spoken}, previous_turns={self.previous_turns}'

    def _next_turn(self):
        self.current_round += 1

    def _note_last_spoken(self, number):
        spoken_before, _ = self.previous_turns.get(number, (0,0))
        self.last_spoken = number
        self.previous_turns[number] = (self.current_round, spoken_before)

    def speak(self):
        spoken_before, before_that = self.previous_turns.get(self.last_spoken, (0,0))
        if before_that == 0:
            self._note_last_spoken(0)
        else:
            self._note_last_spoken(spoken_before - before_that)
        self._next_turn()


def solve(final_turn, starting_numbers):
    turn = Turn(starting_numbers)
    for _ in range(len(starting_numbers), final_turn):
        turn.speak()
    return turn

if __name__ == "__main__":
    t = solve(30000000, DATA)
    print(f'{t.current_round} - {t.last_spoken}')