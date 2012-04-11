# CloudRover

## Overview

CloudRover serves as configuration entry-point for instances running in the cloud. CloudRover provides an easy way to dynamically re-configure an instance. Tools like Puppet/Chef serve a similar purpose, but those work on a "pull" model, while CloudRover allows the configuration to be "pushed" to an instance. That approach provides more transparency in regard to the state of a running instance.

CloudRover serves two distinct use-cases:

 * Provides an external HTTP-based API to execute shell-scripts locally. Example: "Stop service A running on that box/instance"
 * Provides a key/value store that can serve as a lookup service for configuration information for locally installed apps. Example: "Get me the address for the system-wide message queue"


Some architectural decisions made:

 * CloudRover stores configuration in memory only. All configuration has to be re-set after every reboot of an instance.
 * For authentication and security purposes a CloudRover-ID has to be sent to the CloudRover for every write and execute operation. That ID is part of the URL. The ID has to be set every time the CloudRover, the instance gets restarted. The ID can only be set once.
 * Shell-scripts get pulled down from a GitHub repository. The URL for the GitHub repository can only be set once.
 * No ID is require to retrieve any of the values. But ID and URL for the GitHub repository can't be read/retrieved. 


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

```
./start.sh
```

