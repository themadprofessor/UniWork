import numpy as np

from search import *

from helpers import env2statespace
from uofgsocsai import LochLomondEnv
from util import run_changed_stdout


def run(problem_id=0, max_episodes=10000, max_iters_per=2000, reward_hole=0.0):
    env = LochLomondEnv(problem_id=problem_id, is_stochastic=False, reward_hole=reward_hole)

    statespace_locs, statespace_actions, statespace_init, statespace_goal = env2statespace(env)
    maze_problem = GraphProblem(statespace_init, statespace_goal, UndirectedGraph(statespace_actions))

    np.random.seed(12)

    results = []

    for episode in range(max_episodes):
        print('-' * 50)
        env.reset()
        func = memoize(maze_problem.h, 'func')
        frontier = PriorityQueue('min', func)
        node = Node(maze_problem.initial)
        frontier.append(node)
        seen = set()
        for iter in range(max_iters_per):
            node = frontier.pop()
            print(",".join([str(episode), str(iter), node.state]))
            if maze_problem.goal_test(node.state):
                print('done')
                results.append({'iters': iter, 'success': True})
                break
            seen.add(node.state)
            for possible in node.expand(maze_problem):
                if possible.state not in seen and possible not in frontier:
                    frontier.append(possible)
                elif possible in frontier:
                    if func(possible) < frontier[possible]:
                        del frontier[possible]
                        frontier.append(possible)

    return results


if __name__ == '__main__':
    problem_id = int(sys.argv[1])
    run_changed_stdout(run, 'simple', sys.argv[1], problem_id=problem_id)
