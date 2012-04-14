# CloudRover

## Overview

CloudRover serves as configuration entry-point for instances running in the cloud. CloudRover provides an easy way to dynamically re-configure a running instance. Tools like Puppet and Chef serve a similar purpose, but those are based on a "pull" model, while CloudRover allows configuration to be "pushed" to an instance. That approach provides more transparency in regard to the state of a running instance.

![image](https://github.com/neuhausler/CloudRover/blob/master/CloudRover.jpg?raw=true)

CloudRover serves two distinct use-cases:

 * Provides an external HTTP-based API to execute shell-scripts locally. Example: "Deploy war file to a Tomcat service running on that box/instance"
 * Provides a key/value store that can serve as a lookup service for configuration information for locally installed apps. Example: "Get me the address for the system-wide message queue"


Some architectural decisions made:

 * CloudRover stores configuration in memory only. All configuration has to be re-set after every reboot of an instance.
 * For authentication and security purposes a CloudRover-ID has to be sent to the CloudRover for every write and execute operation. That ID is part of the URL. The ID has to be set every time either the CloudRover or the the instance gets restarted. The ID can only be set once.
 * Shell-scripts get pulled down from a Git repository. The URL for the Git repository can only be set once.
 * No ID is require to retrieve any of the values. Exceptions are ID and URL for the Git repository, those values can't be read/retrieved. 


CloudRover is written in Erlang based on [webmachine] (https://github.com/basho/webmachine).

## Quick Start

CloudRover was developed and tested using Erlang R14B03.

**Build:**

```
git clone git://github.com/neuhausler/CloudRover.git
cd CloudRover
make
```

**Run:**

Run in Erlang console mode

```
./start.sh
```

**Set CloudRover ID**

A CloudRover ID has to be set first via a PUT request. That ID will be used later as part of the URL. That ID can only be set once.

Example: Set ID of local CloudRover to "A123B345":

```
curl -X PUT http://localhost:8000/base/key \
     -H 'Content-Type: application/json' \
     -d '{"accesskey":"A123B345"}'
```

**Set Git repository**

The CloudRover can execute scripts that are available via a Git repository. The URL for the Git repository can only be set once. The CloudRover ID defined in the previous step (`A123B345`) has to be part of the URL used to set the Git repository.

If the Git repository requires username/password those can be passed in via the URL: `https://username:password@github.com/username/test`.

Example: Set the URL for the Git repository to the test repository `https://github.com/neuhausler/cloudrover_test_config.git`
 
```
curl -X PUT http://localhost:8000/base/A123B345/gitsh \
     -H 'Content-Type: application/json' \
     -d '{"gitsh":"https://github.com/neuhausler/cloudrover_test_config.git"}'
```

**Execute a shell command remotely**

Shell commands can be executed via CloudRover. The CloudRover ID has to be part of the URL. Only shell scripts in the previously defined Git repository can be executed. CloudRover does a `git clone/pull` before every execution of a command. Shell scripts are "packaged" in folders.

Example: Execute `ls_tmp.sh` defined in the `playground` folder in the test Git repository

```
curl http://localhost:8000/base/A123B345/sh/playground/ls_tmp.sh
```

The output of a command gets returned as a JSON object: `{"Output":"8 lrwxr-xr-x@ 1 root  wheel  11 Aug 20  2011 /tmp -> private/tmp"}`


Optional environment variables can be passed in via the URL as regular URL parameters.

Example: Execute `ls.sh` in the `playground` folder with `$cdpath` set to `/` (URL encoded: `%2F`)


```
curl http://localhost:8000/base/A123B345/sh/playground/ls.sh?cdpath=%2F
```

**Set a configuration parameter**

CloudRover serves as a generic Key/Value store. This can be used to store/exchange configuration information. The CloudRover ID defined a startup (`A123B345`) has to be used to set a value.

Example: Set the host for the message queue `mq_hostname` to `mq.test.com` and the port `mq_portnumber` of the queue to `5672`

```
curl -X PUT http://localhost:8000/base/A123B345/dict/mq_hostname \
     -H 'Content-Type: application/json' \
     -d '{"value":"mq.test.com"}'

curl -X PUT http://localhost:8000/base/A123B345/dict/mq_portnumber \
     -H 'Content-Type: application/json' \
     -d '{"value":"5672"}'
```

**Retrieve a configuration parameter**

Configuration information stored in the previous step can be retrieved by local applications. No CloudRover ID is needed to retrieve information.

```
curl http://localhost:8000/base/get/dict/mq_hostname

curl http://localhost:8000/base/get/dict/mq_portnumber
```

The value gets returned as a JSON object: `{"mq_hostname":"mq.test.com"}` and `{"mq_portnumber":"5672"}`

**CloudRover Configuration**

The configuration for the CloudRover itself is stored in `./priv/cloudrover.conf` and can be adjusted as needed.
```
{port,     8000}.

{log_dir,  "priv/log/"}.

{work_dir, "priv/var/work/"}.

{pid_file, "priv/var/cloudrover.pid"}.
```

