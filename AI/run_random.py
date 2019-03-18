import numpy as np

from uofgsocsai import LochLomondEnv
from util import *


def run(problem_id=0, max_episodes=200, max_iters_per=500, reward_hole=0.0):
    env = LochLomondEnv(problem_id=problem_id, is_stochastic=True, reward_hole=reward_hole)

    np.random.seed(12)

    results = []

    for episode in range(max_episodes):
        env.reset()
        print('-' * 50)
        print_headers()

        for iteration in range(max_iters_per):
            action = env.action_space.sample()
            observation, reward, done, info = env.step(action)
            print(",".join([str(episode), str(iteration), str(reward), str(done), str(info), str(action)]))

            if done and reward == reward_hole:
                env.render()
                print("Hole Found in " + str(iteration) + " iterations")
                results.append({'iters': iteration, 'success': False})
                break

            if done and reward == 1.0:
                env.render()
                print("Frisbee acquired in " + str(iteration) + " iterations")
                results.append({'iters': iteration, 'success': True})
                break

    return results


if __name__ == '__main__':
    problem_id = int(sys.argv[1])
    run_changed_stdout(run, 'random', sys.argv[1], problem_id=problem_id)
