"""We do work when jobs are placed in the queue."""
from functools import partial
from concurrent.futures import ThreadPoolExecutor
import json
import re
import os
import socket
import subprocess
import sys
import time

import pika
from pyiem.util import logger

LOG = logger()
FILENAME_RE = re.compile(
    "/i/(?P<scenario>[0-9]+)/env/(?P<huc8>[0-9]{8})/(?P<huc812>[0-9]{4})/"
    "(?P<huc12>[0-9]{12})_(?P<fpath>[0-9]+).env"
)


def get_rabbitmqconn():
    """Load the configuration."""
    # load rabbitmq.json in the directory local to this script
    with open("rabbitmq.json", "r", encoding="utf-8") as fh:
        config = json.load(fh)
    return pika.BlockingConnection(
        pika.ConnectionParameters(
            host=config["host"],
            port=config["port"],
            virtual_host=config["vhost"],
            credentials=pika.credentials.PlainCredentials(
                config["user"], config["password"]
            ),
        )
    )


def drain(channel, method, _props, _rundata):
    """NOOP to clear out the queue via a hackery"""
    channel.basic_ack(delivery_tag=method.delivery_tag)


def run(channel, method, _props, rundata):
    """Actually run wepp for this event"""
    with subprocess.Popen(
        ["timeout", "60", "wepp"],
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stdin=subprocess.PIPE,
    ) as proc:
        (stdoutdata, stderrdata) = proc.communicate(rundata)
    if stdoutdata[-13:-1] != b"SUCCESSFULLY":
        # So our job failed and we now have to figure out a filename to use
        # for the error file.  This is a quasi-hack here, but the env file
        # should always point to the right scenario being run.
        m = FILENAME_RE.search(rundata.decode("ascii"))
        if m:
            d = m.groupdict()
            errorfn = (
                f"/i/{d['scenario']}/error/{d['huc8']}/{d['huc812']}/"
                f"{d['huc12']}_{d['fpath']}.error"
            )
            os.makedirs(os.path.dirname(errorfn), exist_ok=True)
            with open(errorfn, "wb") as fp:
                hn = f"Hostname: {socket.gethostname()}\n"
                fp.write(hn.encode("ascii"))
                fp.write(stdoutdata)
                fp.write(stderrdata)
    channel.basic_ack(delivery_tag=method.delivery_tag)


def consumer_thread(jobfunc, thread_id):
    """Our main runloop."""
    LOG.info("Starting consumer_thread(%s)", thread_id)

    conn = get_rabbitmqconn()
    channel = conn.channel()
    channel.queue_declare("dep", durable=True)
    # otherwise rabbitmq will send everything
    channel.basic_qos(prefetch_count=100)
    # make us acknowledge the message
    channel.basic_consume("dep", jobfunc, auto_ack=False)
    # blocks
    channel.start_consuming()


def main(argv):
    """Go main Go."""
    if len(argv) not in [3, 4]:
        print("USAGE: python queue_worker.py <scenario> <threads> <drainme?>")
        return
    while True:
        try:
            start_threads = int(argv[2])
            f = partial(consumer_thread, run if len(argv) < 4 else drain)
            with ThreadPoolExecutor(max_workers=start_threads) as pool:
                LOG.info("Starting %s threads", start_threads)
                pool.map(f, range(start_threads))
            print("runloop exited cleanly, sleeping 30 seconds")
            time.sleep(30)
        except KeyboardInterrupt:
            print("Exiting due to keyboard interrupt")
            break
        except Exception as exp:
            print("Encountered Exception, sleeping 30 seconds")
            print(exp)
            time.sleep(30)


if __name__ == "__main__":
    main(sys.argv)
