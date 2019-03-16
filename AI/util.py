import sys


def print_headers():
    print('episode,iteration,reward,done,info,action')


def run_changed_stdout(func, ai_name, problem, ext="txt", *args, **kwargs):
    with open('out_' + ai_name + "_" + problem + '.' + ext, 'w+') as out:
        sys.stdout = out
        func(*args, **kwargs)
