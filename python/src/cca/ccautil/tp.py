#!/usr/bin/env python3


'''
  A naive implementation of task pool

  Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
'''

import os
import fcntl
import random
import errno
import shutil
import tempfile
import math
import logging

logger = logging.getLogger()

#####

max_ntasks = 1024

task_dir_name = 'task'
result_dir_name = 'result'
lock_dir_name = 'lock'

# polling_interval = 3 # sec
lock_failure_count_threshold = 10

#####


class LockFailedException(Exception):
    pass


class base(object):

    def task(self, args):  # should be overridden
        return ('NOT', 'IMPLEMENTED')  # args:TUPLE -> result:TUPLE

    def gen_tasks(self):  # should be overridden
        return []  # list of tuples

    #####

    def prepare_dir(self, d, clear_cache=True):
        if os.path.isdir(d):
            if clear_cache:
                shutil.rmtree(d)
                os.makedirs(d)
        else:
            try:
                os.makedirs(d)
            except BaseException as e:
                logger.error(str(e))

    def __init__(self, working_dir='.', clear_cache=True):

        logger.info('working directory: "%s"' % working_dir)
        logger.info('maximum number of task sets: %d' % max_ntasks)

        self.working_dir = os.path.abspath(working_dir)
        self.__task_dir = os.path.join(self.working_dir, task_dir_name)
        self.__lock_dir = os.path.join(self.working_dir, lock_dir_name)
        self.__result_dir = os.path.join(self.working_dir, result_dir_name)

        self.max_ntasks = max_ntasks

        self.__consecutive_lock_failure_count = 0
        self.__task_tbl = {}
        self.ntasks = 0
        self.__nfinishedtasks = 0
        self.__result_tbl = {}
        self.clear_cache = clear_cache

        self.prepare_dir(self.__task_dir, clear_cache)
        self.prepare_dir(self.__result_dir, clear_cache)
        self.prepare_dir(self.__lock_dir, clear_cache)

        self.__LOCK_OK = False

    def tuple_to_string(self, t):
        s = ''
        if t:
            u = [str(x) for x in t]
            s = ','.join(u)
        return s

    def string_to_tuple(self, s):
        t = s.rstrip().split(',')
        return t

    def get_result(self):
        return self.__result_tbl

    def fill(self):
        logger.info('generating task sets...')
        tasks = self.gen_tasks()

        if not tasks:
            logger.info('no task sets are generated')
            return

        ntasks = len(tasks)

        logger.info('number of tasks: %d' % ntasks)

        sz = 1
        if ntasks > self.max_ntasks:
            sz = int(math.ceil(float(ntasks) / float(self.max_ntasks)))

        logger.info('size of each task set: %d' % sz)

        count = 0
        tidc = 0
        task_gr_tbl = {}

        for task in tasks:
            if count % sz == 0:
                if count > 0:
                    tidc += 1
                tid = str(tidc)
                task_gr_tbl[tid] = [task]
            else:
                tid = str(tidc)
                task_gr_tbl[tid].append(task)

            count += 1

        for i in task_gr_tbl.keys():

            task_f = os.path.join(self.__task_dir, i)

            try:
                f = open(task_f, 'w')
                for t in task_gr_tbl[i]:
                    f.write(self.tuple_to_string(t))
                    f.write('\n')
                f.close()

            except Exception as e:
                logger.warning(str(e))

            lock_f = os.path.join(self.__lock_dir, i)
            try:
                f = open(lock_f, 'w')
                f.close()

            except Exception as e:
                logger.warning(str(e))

        self.ntasks = len(task_gr_tbl)

        logger.info('%d task sets are generated' % self.ntasks)

    def lock_task(self, taskid):
        logger.debug('locking "%s"...' % taskid)

        lock_path = os.path.join(self.__lock_dir, taskid)
        try:
            f = open(lock_path, 'w')
            try:
                fcntl.lockf(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
                self.__task_tbl[taskid] = f
                logger.debug('task set "%s" is locked' % taskid)
            except IOError as e:
                if e.errno == errno.EACCES or e.errno == errno.EAGAIN:
                    f.close()
                    raise LockFailedException
                else:
                    f.close()
                    logger.warning('IO error: %s' % (str(e)))
                    raise LockFailedException

        except Exception:
            logger.warning('open failed: "%s"' % lock_path)
            raise LockFailedException

    def unlock_task(self, taskid):
        logger.debug('unlocking "%s"...' % taskid)

        try:
            f = self.__task_tbl[taskid]
            try:
                fcntl.lockf(f, fcntl.LOCK_UN)
                logger.debug('task set "%s" is unlocked' % taskid)
                del self.__task_tbl[taskid]
                f.close()
            except IOError as e:
                f.close()
                logger.warning(str(e))

        except KeyError:
            logger.warning('task set "%s" is not locked' % taskid)
        except Exception as e:
            logger.warning(str(e))

    def do_task(self, taskid):
        try:
            self.lock_task(taskid)

        except LockFailedException:
            logger.warning('failed to lock "%s"' % taskid)
            if not self.__LOCK_OK:
                self.__consecutive_lock_failure_count += 1
            else:
                self.__LOCK_OK = False
            return

        self.__LOCK_OK = True
        self.__consecutive_lock_failure_count = 0

        task_path = os.path.join(self.__task_dir, taskid)

        if not os.path.exists(task_path):
            return

        logger.info('tasksetid: "%s" task path: "%s"' % (taskid, task_path))

        results = []

        try:
            f = open(task_path, 'r')

            for args_s in f:
                if args_s:
                    args = self.string_to_tuple(args_s)

                    r = self.task(args)

                    results.append(r)

            f.close()

            result_path = os.path.join(self.__result_dir, taskid)

            try:
                os.remove(task_path)
                g = open(result_path, 'w')
                for r in results:
                    r_s = self.tuple_to_string(r)
                    g.write(r_s)
                    g.write('\n')
                g.close()

                self.__nfinishedtasks += 1
                self.unlock_task(taskid)

            except OSError as e:
                logger.warning(str(e))

        except EnvironmentError as e:
            logger.warning(str(e))
            self.unlock_task(taskid)
            return

    def pick_up_task(self, wid=''):
        li = os.listdir(self.__task_dir)
        if len(li) > 0:
            t = random.choice(li)
            w = ''
            if wid:
                w = '<wid:%s> ' % wid

            logger.info('%spicking up "%s"' % (w, t))
            self.do_task(t)

    def pick_up_results(self):
        for tid in os.listdir(self.__result_dir):
            tid_f = os.path.join(self.__result_dir, tid)

            results = []

            try:
                f = open(tid_f, 'r')
                for s in f:
                    results.append(self.string_to_tuple(s))
                f.close()

                self.__result_tbl[tid] = results

                if self.clear_cache:
                    os.remove(tid_f)
                else:
                    os.rename(tid_f, os.path.join(self.__result_dir_bak, tid))

            except EnvironmentError:
                logger.warning('cannot pick up "%s"' % tid)

    def watch_tasks(self, wid=''):
        w = ''
        if wid:
            w = '<wid:%s> ' % wid
        try:
            while(True):

                li = os.listdir(self.__task_dir)

                ntasks = len(li)

                if ntasks == 0:
                    break

                if self.__consecutive_lock_failure_count \
                   > lock_failure_count_threshold:
                    logger.info('%sABORTED!' % w)
                    break
                else:
                    logger.info('%s%d task sets remain' % (w, ntasks))
                    self.pick_up_task()
                    logger.info('{}{} task sets finished'
                                .format(w, self.__nfinishedtasks))

        except KeyboardInterrupt:
            pass

    def watch_results(self):
        if not self.clear_cache:
            self.__result_dir_bak = tempfile.mkdtemp('', 'result.',
                                                     self.working_dir)
            logger.info('created backup directory for result: "{}"'
                        .format(self.__result_dir_bak))

        logger.debug('reading "%s"' % self.__result_dir)
        li = os.listdir(self.__result_dir)
        nresults = len(li)
        logger.info('%d results found' % nresults)
        self.pick_up_results()
        logger.info('finished.')


#####

def test():
    import random

    def fib(n):
        if n == 0:
            return 0
        elif n == 1:
            return 1
        elif n > 1:
            return (fib(n - 1) + fib(n - 2))
        else:
            logger.error('out of range')

    class TaskPool(base):
        def task(self, args):
            a = int(args[0])
            return (fib(a),)

        def gen_tasks(self):
            tbl = {}
            for i in range(100):
                a = random.randint(6, 30)
                tbl[str(i)] = a
            return tbl

    t = TaskPool('.')
    t.fill()
    t.watch_tasks()


if __name__ == '__main__':
    test()
