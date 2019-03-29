import numpy as np
import sys

from rl import *

from helpers import env2statespace
from uofgsocsai import LochLomondEnv
from util import run_changed_stdout, print_headers


def choose_action(state, epsilon, Q, env):
    if np.random.uniform(0, 1) < epsilon:
        action = env.action_space.sample()
    else:
        action = np.argmax(Q[state, :])

    return action


def learn(state, state2, reward, action, Q, gamma, lr_rate):
    predict = Q[state, action]
    target = reward + gamma * np.max(Q[state2, :])
    Q[state, action] = Q[state, action] + lr_rate * (target - predict)


def run(problem_id=0, max_episodes=10000, max_iters_per=2000, reward_hole=-1.0):
    env = LochLomondEnv(problem_id=problem_id, is_stochastic=True, reward_hole=reward_hole)

    epsilon = 0.9
    lr_rate = 0.81
    gamma = 0.96
    epsilon_reduce = 1 / max_episodes

    Q = np.zeros((env.observation_space.n, env.action_space.n))

    np.random.seed(12)

    results = []

    for episode in range(max_episodes):
        state = env.reset()
        print('-' * 50)
        print_headers()

        for iter in range(max_iters_per):
            action = choose_action(state, epsilon, Q, env)
            state2, reward, done, info = env.step(action)
            print(",".join([str(episode), str(iter), str(reward), str(done), str(info), str(action)]))

            learn(state, state2, reward, action, Q, gamma, lr_rate)
            state = state2
            if done and reward == reward_hole:
                print('Found a hole in ' + str(iter) + ' iterations')
                results.append({'iters': iter, 'success': False})
                break
            if done:
                print('Found frisbee in ' + str(iter) + ' iterations')
                results.append({'iters': iter, 'success': True})
                break

        epsilon -= epsilon_reduce

    return results


if __name__ == '__main__':
    problem_id = int(sys.argv[1])
    run_changed_stdout(run, 'rl', sys.argv[1], problem_id=problem_id)
