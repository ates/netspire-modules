Additional modules for Netspire
================================

mod\_disconnect\_script
---------------------

    Disconnect client by executing external program or script

###Configuration###
This should be added to the **modules** section of the netspire.conf:

    {mod_disconnect_script, [{disconnect_script, "/full/path/to/script"}]}

mod\_disconnect\_pod
------------------

    Terminate client's session by sending Radius Disconnect-Message (rfc3576) to the NAS

###Configuration###
This should be added to the **modules** section of the netspire.conf:
    {mod_disconnect_pod, []}

mod\_geoip
----------

    Provides interface to fastest way to lookup country code by IP address
    Use GeoIP database from software77.net

###Configuration
This should be added to the **modules** section of the netspire.conf:
    {mod_geoip, [{file, "/path/to/geoip.database"}]}
