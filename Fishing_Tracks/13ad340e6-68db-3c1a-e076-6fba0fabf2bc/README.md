
# Global Fishing Watch Map Tracks

The downloaded file provides basic track information for a vessel of interest.

## Schema of the file

Lon: longitude in decimal degrees
Lat: latitude decimal degrees
Timestamp: timestamp for AIS position in UTC
Speed: speed (knots) of vessel transmitted by AIS
Course: course (direction) of vessel transmitted by AIS
Seg_id: Unique identifier for the segment of AIS


## Caveats

The track information is derived from the AIS sources AIS sources; Orbcom 2012 to June 2016, Orbomm and Spire June 2016 to December 2022, Spire Global January 2023. Segment ID is an internal unit used at Global Fishing Watch as part of the process for ensuring valid AIS positional data is linked together into a coherent track.

AIS data is limited by those vessels that transmit AIS data and do so by entering accurate vessel identity information in the transmitter. Track information and segment ID are all impacted by quality of AIS data. Vessels transmitting in low reception areas, with class B AIS,  transmitting intermittently, or vessels that turn their AIS off for long periods of time (when in port for example) are more likely to have numerous Vessel IDs associated with the same physical vessel, gaps in track information, or other possible inconsistencies.

While there is no definitive solution to this issue since it is inherent to the nature of AIS, GFW continues to develop methods to identify the true track for a single physical vessel over time.

Some points of the track are removed for license restriction with our data providers. If the full track is needed, please contact Global Fishing Watch Support <support@globalfishingwatch.org>.

## License

Non-Commercial Use Only. The Site and the Services are provided for Non-Commercial use only in accordance with the CC BY-NC 4.0 license. If you would like to use the Site and/or the Services for commercial purposes, please contact us.

Meta data:

Dataset Id used to generate the export: public-global-all-tracks:v3.0
Vessel Id: 7a52b967d-d4c7-3bcf-3efa-6a6063a6d414,13ad340e6-68db-3c1a-e076-6fba0fabf2bc,077fbcb4a-a192-1f94-4fe0-8535de771dd9,b0956d11c-ce8c-6341-77de-ba636eaa2e6f,9e3e3cb50-0ee0-4d52-9da5-2b31c3c064df,e771ccf7d-dc8e-734d-1b66-9b1f331e9dd7,6c651a03a-afd0-ac0d-3d80-0a37f0a12f5b,e0a11d514-4f9d-48bf-d191-a1f08e0481d0,6a8dbfb5d-ddbe-cf41-1818-16af4ab1ac8f
Time Range: 2013-06-21T08:25:05.000Z,2025-05-15T23:59:45.000Z

