import os
import sys
import matplotlib.pyplot as plt
import numpy as np

import run_rl as rl
import run_random as random
import run_simple as simple


def run():
    results = []

    with open(os.devnull, 'w') as null:
        sys.stdout = null

        for prob in range(8):
            print('Running problem [' + str(prob) + ']', file=sys.stderr)
            results.append({
                'random': random.run(prob),
                'simple': simple.run(prob),
                'rl': rl.run(prob)
            })

    sys.stdout = sys.__stdout__
    print(results)

    i = 0
    for res in results:
        rl_p = plt.plot(list(map(lambda x: x['iters'], res['rl'])), color='red')
        rand_p = plt.plot(list(map(lambda x: x['iters'], res['random'])), color='blue')
        simp_p = plt.plot(list(map(lambda x: x['iters'], res['simple'])), color='green')
        plt.legend((rl_p[0], rand_p[0], simp_p[0]), ('Reinforcement Learning', 'Random', 'Simple'))
        plt.ylabel('Iterations')
        plt.xlabel('Episode')
        plt.title('Number of iterations for agent to complete for problem ' + str(i))
        plt.show()

        succ = [list(map(lambda x: x['success'], res['rl'])).count(True)/10000.0, list(map(lambda x: x['success'], res['random'])).count(True)/10000.0, list(map(lambda x: x['success'], res['simple'])).count(True)/10000.0]
        print('{} & {} \\\\'.format(i, ' & '.join(map(str, succ))))

        i += 1


if __name__ == '__main__':
    run()
