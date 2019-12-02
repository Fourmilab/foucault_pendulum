/*

                       Foucault Pendulum
                   by John Walker (Fourmilab)
                    http://www.fourmilab.ch/

        This program is licensed under a Creative Commons
        Attribution-ShareAlike 4.0 International License.
            http://creativecommons.org/licenses/by-sa/4.0/
        Please see the License section in the "Fourmilab
        Foucault Pendulum User Guide" notecard included in the
        object for details.

    The pendulum swings in its principal axis according to the
    simple harmonic motion of a physical pendulum.  The plane in
    which it is fixed with respect to the distant stars and
    precesses as the Earth rotates with a speed determined by
    the latitude at which the pendulum is installed.  In
    northern latitudes (latitude > 0), the precession is
    clockwise, while in the southern hemisphere (latitude < 0),
    precession is counterclockwise.  The angular rate of
    precession in clockwise degrees per day is 360 *
    sin(latitude).  At the north or south poles, the rate of
    precession will be at its maximum of one rotation per
    sidereal day, where a sidereal day is 86164.0905 seconds or
    0.99726957 solar days. There is no precession if the
    pendulum is installed at the equator (latitude 0).

    The handling of time in this program is rather wonky thanks
    to Second Life's restriction of float variables to single
    precision.  We need to maintain an accurate fraction of a
    second clock to maintain a smooth animation of the pendulum
    bob (the animation step is set by UPDATE_ANIMATION below),
    but since the simulation may run for a long time, we also
    need to accurately keep track of the elapsed time since the
    simulation started and the absolute time it started in order
    to compute the apparent precession of the plane of swing due
    to rotation of the planet, and those numbers can get
    sufficiently large that we lose the least significant digits
    in a single precision float.  To cope with this, we use two
    separate timekeeping mechanisms for the animation of the bob
    and the precession of the plane of swing.  The bob animation
    is based upon the float variable tee, in units of seconds,
    and the value of tee is updated in updatePendulum() in units
    modulo the swing time given by period, which is computed
    from the string length and the gravtational acceleration.
    This keeps the value of tee, used to compute the angle of
    the bob and its angular velocity, from growing so large it
    begins to lose precision.

    The time used to compute precession is kept in terms of Unix
    time [UTC seconds as returned by llGetUnixTime()], and is
    subtracted from the start time of the simulation in
    startUtime to determine the amount of precession since the
    start.  Since this is a 32-bit integer and we only need
    precision to the second to compute precession, this won't
    overflow until the dreaded end of the world in 2038.  All of
    this extra complexity is hardly clean, but it's what we must
    do to cope with Second Life's gnarly single precision
    floats.

*/

    float latitude = 45;            // Latitude of installation

    integer commandChannel = 1851;  // Command channel in chat
    integer commandH;               // Handle for command channel
    key whoDat = NULL_KEY;          // Avatar who sent command

    integer ncBusy;                 // Are we reading a notecard ?
    list ncQueue;                   // Queue of pending notecards to read

    integer animating = FALSE;      // Are we animating ?

    integer sit = FALSE;            // Is somebody sitting on the bob ?
    key seated = NULL_KEY;          // UUID of seated avatar

    //  Link indices within the object

    integer lPivot = LINK_THIS;     // Pivot
    integer lBob;                   // Pendulum bob
    integer lString;                // String
    integer lPlaneI;                // Plane indicator
    integer lHourPast;              // Hour marker past
    integer lHourNext;              // Hour marker present
    integer lHourFuture;            // Hour marker future

    //  Useful constants

    integer SECONDS_PER_HOUR = 3600;
    integer SECONDS_PER_DAY = 86400;
    float SIDEREAL_EARTH = 86164.0905;          // Earth sidereal day in seconds
    float GEE_EARTH = 9.80665;                  // Earth standard gravity in metres/sec^2
    
    //  Special characters
    string U_deg;                       // U+00B0 Degree Sign

    /*  Find a linked prim from its name.  Avoids having to
        slavishly link prims in order in complex builds to
        reference them later by link number.  You should only
        call this once, in state_entry(), and then save the link
        numbers in global variables.  Returns the prim number or
        -1 if no such prim was found.  Caution: if there is more
        than one prim with the given name, the first will be
        returned without warning of the duplication.  */

    integer findLinkNumber(string pname) {
        integer i = llGetLinkNumber() != 0;
        integer n = llGetNumberOfPrims() + i;

        for (; i < n; i++) {
            if (llGetLinkName(i) == pname) {
                return i;
            }
        }
        llOwnerSay("Could not find link number for \"" + pname + "\".");
        return -1;
    }

    //  tawk  --  Send a message to the interacting user in chat

    tawk(string msg) {
        if (whoDat == NULL_KEY) {
            //  No known sender.  Say in nearby chat.
            llSay(PUBLIC_CHANNEL, msg);
        } else {
            llRegionSayTo(whoDat, PUBLIC_CHANNEL, msg);
        }
    }

    //  updatePendulum  --  Update pendulum for this animation step

    float period;                   // Period of swing in seconds
    float amplitude = 8;            // Amplitude of pendulum swing in degrees (theta_0)
                                    // This is excursion; full swing angle is twice this
    float UPDATE_ANIMATION = 0.1;   // How often do we update when animating ?
    float tee = 0;                  // Current simulation time
    integer nPeriods = 0;           // Number of full periods completed
    float ang = 0;                  // Current displacement angle
    float stringLength;             // Length of string
    vector bobSize;                 // Size of bob
    float stringDia = 0.01;         // Diameter of string
    float gravity = 9.80665;        // Earth standard gravity in metres/sec^2
    float siderealDay = 86164.0905; // Sidereal day in seconds
    float solarDay = 86400;         // Solar day in seconds
    integer precessionUpdate = 60;  // Update precession plane time in seconds
    integer lastPrecess;            // Last time precession computed
    float precessionRate;           // Precession rate in degrees per second
    float precessionAngle;          // Precession angle
    float planeILength;             // Length of plane indicator
    float planeIz;                  // Plane indicator local Z co-ordinate
    integer startUtime;             // Unix time animation started
    integer hourP;                  // Previous hour marker
    integer hourN;                  // Next hour marker
    integer hourF;                  // Future hour marker
    integer nextHourMarker;         // Unix time of next hour marker
    integer showTime;               // Show universal time and marker hours
    integer timeSource;             // Source of displayed time
    float timeZone;                 // Time zone in hours and fraction offset from UTC
    integer wallClockDays;          // Wall clock elapsed days bias
    integer lastWallClock;          // Last wall clock value retrieved
    integer restrictAccess;         // Access restriction: 0 none, 1 group, 2 owner

    string helpFileName = "Fourmilab Foucault Pendulum User Guide"; // Help notecard name

    integer traceSwing;             // Trace swing operation
    integer tracePrecession;        // Trace precession updates
    integer traceHours;             // Trace hour marker updates
    integer tracePeriod;            // Measure actual period of pendulum

    //  fixangle  --  Range reduce an angle in degrees

    float fixangle(float a) {
        return a - (360.0 * llFloor(a / 360.0));
    }

    /*  eddec  --  Edit a float to decimal with the specified
                   number of decimal places.  */

    string eddec(float f, integer decimals) {
        /*  The following code relies upon float to string
            conversion always supplying six decimal places.  */
        float fr = f + (5 * (llPow(0.1, decimals + 1)));   // Round number
        list l = llParseString2List((string) fr, [ " " ], [ "." ]);
        return llList2String(l, 0) + "." +
               llGetSubString(llList2String(l, 2), 0, decimals - 1);
    }

    //  zerofill  --  Fill an integer to a field of places with leading zeroes

    string zerofill(integer n, integer places) {
        string sn = (string) n;
        while (llStringLength(sn) < places) {
            sn = "0" + sn;
        }
        return sn;
    }

    /*  edutime  --  Edit a Unix time() value to HH:MM:SS  */

    string edutime(integer t) {
        integer time = t % SECONDS_PER_DAY;         // Seconds within day
        integer hour = time / SECONDS_PER_HOUR;     // Hours
        integer min = (time % 3600) / 60;           // Minutes
        integer sec = time % 60;                    // Seconds
        return zerofill(hour, 2) + ":" +
               zerofill(min, 2) + ":" +
               zerofill(sec, 2);
    }

    /*  effectiveTime  --  Get the time we're using as time of
                           day.  There are three options,
                           specified by timeSource.
                             0   UTC
                             1   Second Life Time (PST/PDT)
                             2   Local time zone from timeZone
                           UTC and local time zone times include
                           the full time from the Unix epoch. 
                           The llGetWallclock() function which
                           obtains Second Life time returns only
                           seconds since midnight of the current
                           day.  In order to make day-crossing
                           time computations work when that is
                           selected, we synthesise a day number
                           by adding the number of seconds in a
                           day to the value returned to
                           wallClockDays every time the day
                           changes (detected by the seconds
                           returned being less than on the last
                           query).  */

    integer effectiveTime() {
        if (timeSource == 0)  {
            return llGetUnixTime();
        } else if (timeSource == 1) {
            integer wall = (integer) llGetWallclock();
            if (wall < lastWallClock) {
                wallClockDays += SECONDS_PER_DAY;
            }
            lastWallClock = wall;
            return wall + wallClockDays;
        } else if (timeSource == 2) {
            return llGetUnixTime() + ((integer) (timeZone * (60 * 60)));
        } else {
            llOwnerSay("Invalid timeSource setting: " + (string) timeSource);
            return 0;
        }
    }

    /*  resetPendulum  --  Restore pendulum to starting
                           position. This does more than it
                           really has to, in order to facilitate
                           recovery from fat fingers while
                           testing that mess up the structure of
                           the model.  If the length of the
                           string (given by stringLength,
                           determined from the model in the
                           state_entry() event or set by the
                           length command) has changed, the
                           position of the bob and the plane
                           indicator are adjusted accordingly.  */

    resetPendulum() {
        animating = FALSE;
        llSetTimerEvent(0);
        tee = ang = 0;
        startUtime = hourP = hourN = hourF = nextHourMarker = nPeriods = 0;
        period = pendulumPeriod(stringLength, gravity, amplitude * DEG_TO_RAD);
        lastPrecess = -120;             // Force precession update when started
        precessionRate = (360 * llSin(latitude * DEG_TO_RAD)) / siderealDay;
        //  Reset rotation of pivot (and entire apparatus) in case it's been messed up
        llSetLinkPrimitiveParamsFast(lPivot, [ PRIM_ROTATION, llEuler2Rot(<PI, 0, 0>) ]);
        //  Move bob to end of string
        llSetLinkPrimitiveParamsFast(lBob, [ PRIM_POS_LOCAL, <0, 0, stringLength> ]);
        //  Reset string to run between pivot and bob
        llSetLinkPrimitiveParamsFast(lString, [ PRIM_POS_LOCAL, <0, 0, stringLength / 2> ]);
        //  Set string vertical
        llSetLinkPrimitiveParamsFast(lString, [ PRIM_ROT_LOCAL, llEuler2Rot(<0, 0, 0>) ]);
        //  Zero precession
        llSetLinkPrimitiveParamsFast(lPivot, [ PRIM_ROT_LOCAL, llEuler2Rot(<0, PI, 0>) ]);
        //  Place plane indicator beneath the bob
        bobSize = llList2Vector(llGetLinkPrimitiveParams(lBob, [ PRIM_SIZE ]), 0);
        planeIz = stringLength + ((bobSize.x * 5) / 8);
        llSetLinkPrimitiveParamsFast(lPlaneI, [ PRIM_POS_LOCAL, <0, 0, planeIz> ]);
        llSetLinkPrimitiveParamsFast(lPlaneI, [ PRIM_ROT_LOCAL, llEuler2Rot(<0, PI_BY_TWO, 0>) ]);
        vector v = llList2Vector(llGetLinkPrimitiveParams(lPlaneI, [ PRIM_SIZE ]), 0);
        planeILength = v.z;
        //  Place sit target on top of bob
        llSitTarget(<0, 0, stringLength - (3 * bobSize.z)>, llEuler2Rot(<0, PI, 3 * PI_BY_TWO>));
        //  Clear time display if shown
        showTime = TRUE;
        positionHourMarkers(0);
        updateTimeLegend(0);

        //  Clear trace options
        traceSwing = tracePrecession = traceHours = tracePeriod = FALSE;

        //  Process restart-time commands from notecard, if any
        processNotecardCommands("Reset commands");
    }

    /*  pendulumPeriod  --  Compute pendulum period in seconds
                            from string length and gravitational
                            acceleration.  String length is in
                            metres and gee is in metres/sec^2.

                            We use the power series expansion
                            of the elliptic integral for the
                            general solution for any angle
                            of swing, theta0 (radians).  Since
                            Second Life floats are single precision,
                            we only evaluate the first four terms
                            of the series, as subsequent terms
                            would be lost due to lack of precision.  */

    float pendulumPeriod(float stringL, float gee, float theta0) {
        //  Start with small angle approximation
        float pperiod =  TWO_PI * llSqrt(stringL / gee);
        if (theta0 > 0) {
            float t0p2 = theta0 * theta0;
            float t0p4 = t0p2 * t0p2;
            period *= (1 + (t0p2 / 16.0) + ((11.0 / 3072.0) * t0p4) +
                      ((173.0 / 737280.0) * t0p4 * t0p2));
        }
        return pperiod;
    }

    /*  positionHourMarker  --  Place a single hour marker to indicate the
                                precession angle at the given Unix time.  */

    positionHourMarker(integer marker, integer utime) {
        integer elapsed = utime - startUtime;       // Time elapsed since start (may be negative)
        /*  Angle of marker to zero plane,  Note that we have to subtract
            out the precession angle since the entire apparatus is rotated
            globally by that angle and that also rotates the markers.  */
        float mangle = fixangle(((elapsed * precessionRate) + 180) -
                                precessionAngle) * DEG_TO_RAD;
        float mlength = (planeILength / 2) + (planeILength / 20);
        vector mlpos = <mlength * llCos(mangle),
                        mlength * llSin(mangle),
                        planeIz>;
        llSetLinkPrimitiveParamsFast(marker, [ PRIM_POS_LOCAL, mlpos ]);

        if (showTime) {
            llSetLinkPrimitiveParamsFast(marker, [ PRIM_TEXT,
                llGetSubString(edutime(utime), 0, 1), <0, 1, 0>, 1 ]);
        } else {
            llSetLinkPrimitiveParamsFast(marker, [ PRIM_TEXT, "", <0, 0, 0>, 0 ]);
        }

        if (traceHours) {
            /*  Numbering of markers is a hack and may break if you
                unlink and re-link the markers in the linked object.  */
            tawk("Hour marker " + (string) (1 - (marker - lHourPast)) +
                 " placed at " + (string) ((integer) llRound(mangle * RAD_TO_DEG)) +
                 U_deg + " indicating " + llGetSubString(edutime(utime), 0, 1) + ":00");
        }
    }

    /*  positionHourMarkers  --  Place the three hour markers to indicate
                                 the precession angle at the previous top
                                 of the hour and the next two hours.  */

    positionHourMarkers(integer utime) {
        hourP = utime / SECONDS_PER_HOUR;       // Start of last hour
        if ((utime % SECONDS_PER_HOUR) == 0) {
            hourP--;                            // If exactly at top of hour, back up last hour
        }
        hourP *= SECONDS_PER_HOUR;              // Start of previous hour, Unix time
        hourN = hourP + SECONDS_PER_HOUR;       // Start of next hour, Unix time
        hourF = hourN + SECONDS_PER_HOUR;       // Start of hour after that, Unix time

        nextHourMarker = hourN;                 // Set time for next reposition

        //  Place the three markers at the computed hour times
        positionHourMarker(lHourPast, hourP);
        positionHourMarker(lHourNext, hourN);
        positionHourMarker(lHourFuture, hourF);
    }

    //  updateTimeLegend  --  Show or hide time on plane indicator

    updateTimeLegend(integer utime) {
        if (showTime) {
            llSetLinkPrimitiveParamsFast(lPlaneI, [ PRIM_TEXT,
                llGetSubString(edutime(utime), 0, 4), <0, 1, 0>, 1 ]);
        } else {
            llSetLinkPrimitiveParamsFast(lPlaneI, [ PRIM_TEXT, "", <0, 0, 0>, 0 ]);
        }
    }

    /*  updatePendulum  --  Update pendulum model for animation
                            step. This is where most of the
                            heavy lifting is done.  This is
                            called at every UPDATE_ANIMATION
                            interval and should be be optimised
                            to do as little computation as
                            possible.

                            One further optimisation might be to
                            precompute a table of bob positions
                            for every UPDATE_ANIMATION step in
                            period, then use these values when
                            adjusting the position of the bob
                            and string.  But my intuition is
                            that the API calls to move these
                            objects are far more costly than the
                            computation of their position, so
                            this wouldn't really make a
                            significant difference.  */

    float lang = 0;
    float lrevtime = 0;

    updatePendulum() {

        if (tracePeriod) {
            //  Measure actual period of simulation
            if ((lang < 0) && (ang >= 0)) {
                float revtime = (float) llGetSubString(llGetTimestamp(), -10, -2);
                if ((lrevtime > 0) && (revtime > lrevtime)) {
                    tawk("Measured period: " + (string) (revtime - lrevtime));
                }
                lrevtime = revtime;
            }
            lang = ang;
        }

        ang = amplitude * llSin((1.0 / (period / TWO_PI)) * tee);
        if (traceSwing) {
            tawk("Tee " + (string) tee + "  Ang " + (string) ang);
        }

        float angR = ang * DEG_TO_RAD;
        float bobX = stringLength * llSin(angR);
        float bobZ = stringLength * llCos(angR);
        llSetLinkPrimitiveParamsFast(lBob, [ PRIM_POS_LOCAL, <bobX, 0, bobZ> ]);
        llSetLinkPrimitiveParamsFast(lString, [ PRIM_ROT_LOCAL,
            llAxisAngle2Rot(<0, 1, 0>, angR) ]);
        //  Move string centre so ends still touch pivot and bob.
        llSetLinkPrimitiveParamsFast(lString, [ PRIM_POS_LOCAL,
            <bobX / 2, 0, bobZ / 2> ]);

        //  If an avatar is seated, move it with the bob.

        if (sit) {
            llSetLinkPrimitiveParamsFast(llGetNumberOfPrims(), [ PRIM_POS_LOCAL, <bobX, 0, bobZ - (bobSize.z * 2)> ]);
        }

        /*  If precessionUpdate seconds have elapsed since the last
            computation of precession, update the precession and
            apply the global rotation to the pivot.  */

        integer gut = effectiveTime();
        if ((gut - lastPrecess) > precessionUpdate) {
            lastPrecess = gut - (gut % 60);
            precessionAngle = fixangle(precessionRate * (gut - startUtime));
            if (tracePrecession) {
                tawk("Precession angle: " + (string) precessionAngle + "  Time: " + (string) (gut - startUtime));
            }
            llSetLinkPrimitiveParamsFast(lPivot,
                [ PRIM_ROT_LOCAL, llEuler2Rot(<0, PI, precessionAngle * DEG_TO_RAD>) ]);
            //  Update the plane indicator to align with the rotation plane
            llSetLinkPrimitiveParamsFast(lPlaneI, [ PRIM_ROT_LOCAL,
                llEuler2Rot(<0, PI_BY_TWO, precessionAngle * DEG_TO_RAD>) ]);
            //  Must reposition hour markers to take out global rotation
            positionHourMarkers(gut);
            //  If displayed, update time legend
            updateTimeLegend(gut);
        }

        tee += UPDATE_ANIMATION;

        /*  If tee is greater than one swing period, range
            reduce it modulo the period.  This is to
            avoid getting hit by loss of precision in single
            precision floats.  */
        if (tee >= period) {
            nPeriods++;
            tee -= period;
        }
    }

    //  checkAccess  --  Check if user has permission to send commands

    integer checkAccess(key id) {
        return (restrictAccess == 0) ||
               ((restrictAccess == 1) && llSameGroup(id)) ||
               (id == llGetOwner());
    }

    //  processCommand  --  Process a command

    processCommand(key id, string message) {

        if (!checkAccess(id)) {
            llRegionSayTo(id, PUBLIC_CHANNEL,
                "You do not have permission to control this object.");
            return;
        }

        string lmessage = llToLower(llStringTrim(message, STRING_TRIM));
        list args = llParseString2List(lmessage, [" "], []);    // Command and arguments
        string command = llList2String(args, 0);    // The command

        whoDat = id;                    // Direct chat output to sender of command

        //  Access who                  Restrict chat command access to public/group/owner

        if (command == "access") {
            string who = llList2String(args, 1);

            if (who == "public") {
                restrictAccess = 0;
            } else if (who == "group") {
                restrictAccess = 1;
            } else if (who == "owner") {
                restrictAccess = 2;
            } else {
                tawk("Unknown access restriction \"" + who +
                    "\".  Valid: public, group, owner.\n");
            }

        /*  Channel n                   Change command channel.  Note that
                                        the channel change persists across
                                        a reset but not across a script reset.  */

        } else if (command == "channel") {
            integer newch = (integer) llList2String(args, 1);
            if ((newch < 2)) {
                tawk("Invalid channel " + (string) newch + ".");
            } else {
                llListenRemove(commandH);
                commandChannel = newch;
                commandH = llListen(commandChannel, "", NULL_KEY, "");
                tawk("Listening on /" + (string) commandChannel);
            }

        //  Help                        Give help information

        } else if (command == "help") {
            llGiveInventory(id, helpFileName);      // Give requester the User Guide notecard

        //  Reset                       Reset to initial state

        } else if (command == "reset") {
            resetPendulum();

        //  Restart                     Perform a hard restart (reset script)

        } else if (command == "restart") {
            llResetScript();            // Note that all global variables are re-initialised

        //  Set                         Set simulation parameter

        } else if (command == "set") {
            string param = llList2String(args, 1);
            string svalue = llList2String(args, 2);
            float value = (float) svalue;
            integer change = FALSE;

            if (param == "amplitude") {             // amplitude: swing excursion, degrees
                amplitude = value;
                change = TRUE;
                
            } else if (param == "animation") {      // animation: animation step, seconds
                UPDATE_ANIMATION = value;
                change = TRUE;
                
            } else if (param == "gravity") {        // gravity: acceleration m/sec^2
                if (svalue == "earth") {
                    gravity = GEE_EARTH;
                } else {
                    gravity = value;
                }
                change = TRUE;
                
            } else if (param == "latitude") {       // latitude: installation latitude, degrees
                latitude = value;
                tawk("Latitude " + eddec(latitude, 2) + "  Precession " +
                    eddec(360 * llSin(latitude * DEG_TO_RAD), 2) + U_deg + "/sidereal day, " +
                    eddec((360 * llSin(latitude * DEG_TO_RAD) * (siderealDay / solarDay)) / 24, 2) +
                    U_deg + "/hour");
                change = TRUE;
                
            } else if (param == "length") {       // length: length of string
                stringLength = value;
                llSetLinkPrimitiveParamsFast(lBob, [ PRIM_POS_LOCAL, <0, 0, stringLength> ]);
                llSetLinkPrimitiveParamsFast(lString, [ PRIM_SIZE, <stringDia, stringDia, stringLength> ]);
                llSetLinkPrimitiveParamsFast(lString, [ PRIM_POS_LOCAL, <0, 0, stringLength / 2> ]);
                llSetLinkPrimitiveParamsFast(lString, [ PRIM_ROT_LOCAL, llEuler2Rot(<0, 0, 0>) ]);
                tawk("String length: " + eddec(stringLength, 2));
                tawk("Period: " + eddec(pendulumPeriod(stringLength, gravity, amplitude * DEG_TO_RAD), 2) + " seconds");
                change = TRUE;
                
            } else if (param == "precession") {     // precession: precession update, integer seconds
                precessionUpdate = (integer) value;
                change = TRUE;
                
            } else if (param == "shiny") {          // shiny: set shininess of bob and pivot
                integer shine = (integer) value;
                if ((shine >= PRIM_SHINY_NONE) && (shine <= PRIM_SHINY_HIGH)) {
                     llSetLinkPrimitiveParamsFast(lPivot, [ PRIM_BUMP_SHINY, ALL_SIDES, shine, PRIM_BUMP_NONE ]);
                     llSetLinkPrimitiveParamsFast(lBob, [ PRIM_BUMP_SHINY, ALL_SIDES, shine, PRIM_BUMP_NONE ]);
                } else {
                    tawk("Invalid shiny setting.  Must be " + (string) PRIM_SHINY_NONE +
                         " to " + (string) PRIM_SHINY_HIGH);
                }
                
            } else if (param == "sidereal") {       // sidereal: set length of sidereal day
                if (svalue == "earth") {
                    siderealDay = SIDEREAL_EARTH;
                } else {
                    siderealDay = value;
                    if (llGetSubString(svalue, -1, -1) == "d") {
                        siderealDay *= SECONDS_PER_DAY;
                    }
                }
                tawk("Sidereal day set to " + eddec(siderealDay, 2) + " seconds.");
                change = TRUE;
                
            } else if (param == "solar") {       // solar: set length of solar day
                if (svalue == "earth") {
                    solarDay = SECONDS_PER_DAY;
                } else {
                    solarDay = value;
                    if (llGetSubString(svalue, -1, -1) == "d") {
                        solarDay *= SECONDS_PER_DAY;
                    }
                }
                tawk("Solar day set to " + eddec(solarDay, 2) + " seconds.");
                change = TRUE;
                
            } else {
                tawk("Unknown variable \"" + param +
                    "\".  Valid: amplitude, animation, gravity, latitude, length, precession, shiny, sidereal, solar.");
            }

            if (change) {
                resetPendulum();
            }

        //  Start                       Start animation

        } else if (command == "start") {
            animating = TRUE;
            llSetTimerEvent(UPDATE_ANIMATION);
            if (startUtime == 0) {
                //  If this is the first start (and not a restart) mark start time
                startUtime = effectiveTime();
            }

        //  Stat                        Print current status

        } else if (llGetSubString(command, 0, 3) == "stat") {
            tawk("Latitude " + eddec(latitude, 2) + "  Precession " +
                eddec(360 * llSin(latitude * DEG_TO_RAD), 2) + U_deg + "/sidereal day, " +
                eddec(360 * llSin((latitude * DEG_TO_RAD) * (siderealDay / SECONDS_PER_DAY)) / 24, 2) +
                U_deg + "/hour");
            tawk("String length: " + eddec(stringLength, 2));
            tawk("Plane indicator length: " + eddec(planeILength, 2));
            tawk("Solar day: " + eddec(solarDay, 2) + " sec.  Sidereal day: " + eddec(siderealDay, 2) + " sec.");
            tawk("Gravity: " + eddec(gravity, 2) + " m/s^2");
            tawk("Period: " + eddec(pendulumPeriod(stringLength, gravity, amplitude * DEG_TO_RAD), 2) + " seconds");
            tawk("Tee: " + eddec(tee, 2) + "  Nperiods: " + (string) nPeriods);
            tawk("Precession angle: " + eddec(precessionAngle, 2) + "  Tee: " + eddec(tee, 2));
            tawk("Hour indicators:  past " + edutime(hourP) +
                 "  next " + edutime(hourN) +
                 "  future " + edutime(hourF) +
                 "  Now: " + edutime(effectiveTime()));
            integer mFree = llGetFreeMemory();
            integer mUsed = llGetUsedMemory();
            tawk("Script memory.  Free: " + (string) mFree +
                  "  Used: " + (string) mUsed + " (" +
                  (string) ((integer) llRound((mUsed * 100.0) / (mUsed + mFree))) + "%)\n");

        //  Stop                        Stop animation

        } else if (command == "stop") {
            animating = FALSE;
            llSetTimerEvent(0);

/*
        //  Test n                      Run built-in test n

        } else if (command == "test") {
            integer n = (integer) llList2String(args, 1);
            if (n == 1) {
                processNotecardCommands("Initialisation on restart");
            } else if (n == 2) {
                tawk("Time: " + edutime(effectiveTime()));
            } else if (n == 3) {
                tawk("Period: " + (string) pendulumPeriod(1, gravity, 10.0 * DEG_TO_RAD));
            } else {
                tawk("Invalid test number " + (string) n + ".");
            }
*/

        //  Time                        Show or hide times, set time zone

        } else if (command == "time") {
            string subcmd = llList2String(args, 1);
            integer timeChanged = FALSE;
            if ((subcmd == "on") || (subcmd == "off")) {        // on/off
                showTime = llList2String(args, 1) == "on";
                integer gut = effectiveTime();
                positionHourMarkers(gut);
                updateTimeLegend(gut);
            } else if (subcmd == "utc") {                       // UTC
                timeSource = 0;
                timeChanged = TRUE;
            } else if (subcmd == "wall") {                      // wall
                timeSource = 1;
                wallClockDays = lastWallClock = 0;  // Reset wall clock bias
                timeChanged = TRUE;
            } else if (subcmd == "zone") {                      // zone n
                timeSource = 2;
                timeZone = (float) llList2String(args, 2);
                timeChanged = TRUE;
            } else {
                tawk("Unknown time command \"" + subcmd +
                    "\".  Valid: on/off, UTC, wall, zone.");
            }
            if (timeChanged) {
                resetPendulum();
            }

           /*  Trace what on/off            Enable/disable trace
                  swing                     Swing of pendulum
                  period                    Empirically measure period
                  precession                Updates of swing plane precession
                  hours                     Adjustment of hour markers  */

        } else if (command == "trace") {
            string subcmd = llList2String(args, 1);
            integer onoff = llList2String(args, 2) == "on";

            if (subcmd == "swing") {
                traceSwing = onoff;
            } else if (subcmd == "precession") {
                tracePrecession = onoff;
            } else if (subcmd == "hours") {
                traceHours = onoff;
            } else if (subcmd == "period") {
                tracePeriod = onoff;
            } else {
                tawk("Unknown trace request \"" + subcmd +
                    "\".  Valid: swing, period, precession, hours.");
            }

        } else {
            tawk("Huh?  \"" + message + "\" undefined.  Chat /" +
                (string) commandChannel + " help for the User Guide.");
        }
    }

    //  processNotecardCommands  --  Read and execute commands from a notecard

    string ncSource;
    key ncQuery;
    integer ncLine;

    processNotecardCommands(string ncname) {
        ncSource = ncname;
        if (llGetInventoryKey(ncSource) == NULL_KEY) {
            llOwnerSay("No notecard named \"" + ncSource + "\".");
            return;
        }
        if (ncBusy) {
            ncQueue += ncname;
        } else {
            ncLine = 0;
            ncBusy = TRUE;                  // Mark busy reading notecard
            ncQuery = llGetNotecardLine(ncSource, ncLine);
        }
    }

    //  Event processor.  We have only one state, default.

    default {

        //  When the object is newly instantiated, give owner the user guide

        on_rez(integer start_param) {
            llGiveInventory(llGetOwner(), helpFileName);    // Give owner the User Guide notecard
            llResetScript();                // Reset script to default values
        }

        //  The state_entry message performs one-time initialisation

        state_entry() {
            //  Find and save link numbers for child prims
            lBob = findLinkNumber("Bob");
            lString = findLinkNumber("String");
            lPlaneI = findLinkNumber("Plane indicator");
            lHourPast = findLinkNumber("Hour marker past");
            lHourNext = findLinkNumber("Hour marker next");
            lHourFuture = findLinkNumber("Hour marker future");
            
            //  Define Unicode special characters
            U_deg = llUnescapeURL("%C2%B0");    // U+00B0 Degree Sign

            //  Save string length to auto-scale other components
            vector v = llList2Vector(llGetLinkPrimitiveParams(lString, [ PRIM_SIZE ]), 0);
            stringLength = v.z;

            timeSource = 1;                 // Set display time to Wall (Second Life time)
            timeZone = 0;                   // Local time zone set to UTC
            wallClockDays = lastWallClock = 0;  // Reset wall clock bias

            ncBusy = FALSE;                 // Mark no notecard being read
            ncQueue = [ ];                  // Queue of pending notecards

            restrictAccess = 0;             // Public access to chat commands

            commandH = llListen(commandChannel, "", NULL_KEY, ""); // Listen on command chat channel
            llOwnerSay("Listening on chat /" + (string) commandChannel +
                ".  Chat /" + (string) commandChannel + " help for instructions.");

            //  Process restart-time commands from notecard, if any
            processNotecardCommands("Initialisation on restart");

            resetPendulum();
        }

        //  The listen message receives commands entered in chat.

        listen(integer channel, string name, key id, string message) {
            processCommand(id, message);
        }

        //  Touching any part of the apparatus toggles the animation on and off.

        touch_start(integer total_number) {
            key id = llDetectedKey(0);      // Only respond to touches from those with access
            if (checkAccess(id)) {
                whoDat = id;                // Direct chat output to whoever touched
                animating = !animating;
                if (animating) {
                    llSetTimerEvent(UPDATE_ANIMATION);
                    if (startUtime == 0) {
                        //  If this is the first start (and not a restart) mark start time
                        startUtime = effectiveTime();
                    }
                } else {
                    llSetTimerEvent(0);
                }
            }
        }

        /*  The changed event handler detects when an avatar
            sits on the bob or stands up and departs.  We need
            to know if somebody is sitting so that updatePendulum()
            can move the seated avatar with the bob when we're
            running the animation.  */

        changed(integer change) {
            seated = llAvatarOnSitTarget();
            if (change & CHANGED_LINK) {
                if ((seated == NULL_KEY) && sit) {
                    //  Avatar has stood up, departing
                    sit = FALSE;
                } else if (!sit) {
                    //  Avatar has sat on the bob
                    seated = llAvatarOnSitTarget();
                    sit = TRUE;
                }
            }
        }

        //  The timer event is used to update the animation

        timer() {
            if (animating) {
                updatePendulum();
                llSetTimerEvent(UPDATE_ANIMATION);          // Wind the cat
            }
        }

        //  The dataserver event receives lines from the notecard we're reading

        dataserver(key query_id, string data) {
            if (query_id == ncQuery) {
                if (data == EOF) {
                    if (llGetListLength(ncQueue) > 0) {
                        ncSource = llList2String(ncQueue, 0);
                        ncQueue = llDeleteSubList(ncQueue, 0, 0);
                        ncLine = 0;
                        ncQuery = llGetNotecardLine(ncSource, ncLine);
                    } else {
                        ncBusy = FALSE;         // Mark notecard input idle
                    }
                } else {
                    string s = llStringTrim(data, STRING_TRIM);
                    if ((llStringLength(s) > 0) && (llGetSubString(s, 0, 0) != "#")) {
                        processCommand(llGetOwner(), data);
                    }
                    ncLine++;
                    ncQuery = llGetNotecardLine(ncSource, ncLine);
                }
            }
        }
    }
