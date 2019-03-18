import os
import sys
import pprint

import run_rl as rl
import run_random as random
import run_simple as simple


def run():
    results = {}

    with open(os.devnull, 'w') as null:
        sys.stdout = null

        for prob in range(8):
            print('Running problem [' + str(prob) + ']', file=sys.stderr)
            results[prob] = {
                'random': random.run(prob),
                'simple': simple.run(prob),
                'rl': rl.run(prob)
            }





    sys.stdout = sys.__stdout__
    pprint.pprint(results)


if __name__ == '__main__':
    run()
