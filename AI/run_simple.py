import sys

import numpy as np
import networkx as nx

from helpers import env2statespace
from uofgsocsai import LochLomondEnv
from utils import run_changed_stdout


def run(problem_id=0, max_episodes=200, max_iters_per=500, reward_hole=0.0):
    env = LochLomondEnv(problem_id=problem_id, is_stochastic=False, reward_hole=reward_hole)

    statespace_locs, statespace_actions, statespace_init, statespace_goal = env2statespace(env)
    graph = nx.Graph(directed=False)
    graph.add_nodes_from(statespace_locs.keys())
    for start, ends in statespace_actions.items():
        for end in ends:
            graph.add_edge(start, end)

    np.random.seed(12)

    # for episode in range(max_episodes):
    #     env.reset()
    #     for iter in range(max_iters_per):
    #         pass


if __name__ == '__main__':
    problem_id = int(sys.argv[1])
    run_changed_stdout(run, 'simple', sys.argv[1], problem_id=problem_id)
