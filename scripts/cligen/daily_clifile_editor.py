"""
  This is it, we shall create our gridded weather analysis and edit the
  climate files!

  development laptop has data for 9 Sep 2014

"""
import numpy as np
import datetime
import pytz
import pygrib
import sys
import os
import netCDF4
from pyiem import iemre
from scipy.interpolate import NearestNDInterpolator
from pyiem.datatypes import temperature
from multiprocessing import Pool
from pyiem.dep import SOUTH, NORTH, EAST, WEST
import unittest
import logging
from PIL import Image

logging.basicConfig(format='%(asctime)-15s %(message)s')
LOG = logging.getLogger()
LOG.setLevel(logging.INFO)

SCENARIO = sys.argv[1]
YS = int((NORTH - SOUTH) * 100.)
XS = int((EAST - WEST) * 100.)
high_temp = np.zeros((YS, XS), np.float16)
low_temp = np.zeros((YS, XS), np.float16)
dewpoint = np.zeros((YS, XS), np.float16)
wind = np.zeros((YS, XS), np.float16)
solar = np.zeros((YS, XS), np.float16)
precip = np.zeros((30*24, YS, XS), np.float16)
stage4 = np.zeros((YS, XS), np.float16)

# used for breakpoint logic
ZEROHOUR = datetime.datetime(2000, 1, 1, 0, 0)


def get_xy_from_lonlat(lon, lat):
    """Get the grid position"""
    x = int((lon - WEST) * 100.)
    y = int((lat - SOUTH) * 100.)
    return [x, y]


def iemre_bounds_check(name, val, lower, upper):
    """Make sure our data is within bounds, if not, exit!"""
    minval = np.nanmin(val)
    maxval = np.nanmax(val)
    if np.ma.is_masked(minval) or minval < lower or maxval > upper:
        print(("FATAL: iemre failure %s %.3f to %.3f [%.3f to %.3f]"
               ) % (name, minval, maxval, lower, upper))
        sys.exit()
    return val


def load_iemre(valid):
    """Use IEM Reanalysis for non-precip data

    24km product is smoothed down to the 0.01 degree grid
    """
    xaxis = np.arange(WEST, EAST, 0.01)
    yaxis = np.arange(SOUTH, NORTH, 0.01)
    xi, yi = np.meshgrid(xaxis, yaxis)

    fn = "/mesonet/data/iemre/%s_mw_daily.nc" % (valid.year,)
    if not os.path.isfile(fn):
        print("Missing %s for load_solar, aborting" % (fn,))
        sys.exit()
    nc = netCDF4.Dataset(fn, 'r')
    offset = iemre.daily_offset(valid)
    lats = nc.variables['lat'][:]
    lons = nc.variables['lon'][:]
    lons, lats = np.meshgrid(lons, lats)

    # Storage is W m-2, we want langleys per day
    data = nc.variables['rsds'][offset, :, :] * 86400. / 1000000. * 23.9
    # Default to a value of 300 when this data is missing, for some reason
    nn = NearestNDInterpolator((np.ravel(lons), np.ravel(lats)),
                               np.ravel(data))
    solar[:] = iemre_bounds_check('rsds', nn(xi, yi), 0, 1000)

    data = temperature(nc.variables['high_tmpk'][offset, :, :], 'K').value('C')
    nn = NearestNDInterpolator((np.ravel(lons), np.ravel(lats)),
                               np.ravel(data))
    high_temp[:] = iemre_bounds_check('high_tmpk', nn(xi, yi), -60, 60)

    data = temperature(nc.variables['low_tmpk'][offset, :, :], 'K').value('C')
    nn = NearestNDInterpolator((np.ravel(lons), np.ravel(lats)),
                               np.ravel(data))
    low_temp[:] = iemre_bounds_check('low_tmpk', nn(xi, yi), -60, 60)

    data = temperature(nc.variables['avg_dwpk'][offset, :, :], 'K').value('C')
    nn = NearestNDInterpolator((np.ravel(lons), np.ravel(lats)),
                               np.ravel(data))
    dewpoint[:] = iemre_bounds_check('avg_dwpk', nn(xi, yi), -60, 60)

    data = nc.variables['wind_speed'][offset, :, :]
    nn = NearestNDInterpolator((np.ravel(lons), np.ravel(lats)),
                               np.ravel(data))
    wind[:] = iemre_bounds_check('wind_speed', nn(xi, yi), 0, 30)

    nc.close()


def load_stage4(valid):
    """ It sucks, but we need to load the stage IV data to give us something
    to benchmark the MRMS data against, to account for two things:
    1) Wind Farms
    2) Over-estimates
    """
    midnight = datetime.datetime(valid.year, valid.month, valid.day, 12, 0)
    midnight = midnight.replace(tzinfo=pytz.timezone("UTC"))
    midnight = midnight.astimezone(pytz.timezone("America/Chicago"))
    midnight = midnight.replace(hour=1, minute=0, second=0)
    # clever hack for CST/CDT
    tomorrow = midnight + datetime.timedelta(hours=36)
    tomorrow = tomorrow.replace(hour=0)

    lats = None
    lons = None
    totals = None
    now = midnight
    while now <= tomorrow:
        utc = now.astimezone(pytz.timezone("UTC"))
        gribfn = utc.strftime(("/mesonet/ARCHIVE/data/%Y/%m/%d/stage4/"
                               "ST4.%Y%m%d%H.01h.grib"))
        if not os.path.isfile(gribfn):
            print("%s is missing" % (gribfn,))
            now += datetime.timedelta(hours=1)
            continue

        grbs = pygrib.open(gribfn)
        grb = grbs[1]
        if totals is None:
            lats, lons = grb.latlons()
            totals = np.zeros(np.shape(lats))
        # Don't take any values over 10 inches, this is bad data
        values = np.where(grb['values'] < 250, grb['values'], 0)
        # Cap any values over 4 inches to 4 inches
        values = np.where(values > 100, 100, values)
        totals += values
        now += datetime.timedelta(hours=1)

    if totals is None:
        print('No StageIV data found, aborting...')
        sys.exit()
    # set a small non-zero number to keep things non-zero
    totals = np.where(totals > 0.001, totals, 0.001)

    xaxis = np.arange(WEST, EAST, 0.01)
    yaxis = np.arange(SOUTH, NORTH, 0.01)
    xi, yi = np.meshgrid(xaxis, yaxis)
    nn = NearestNDInterpolator((lons.flatten(), lats.flatten()),
                               totals.flatten())
    stage4[:] = nn(xi, yi)
    # print np.max(stage4)
    # import matplotlib.pyplot as plt
    # (fig, ax) = plt.subplots(2, 1)
    # im = ax[0].imshow(stage4)
    # fig.colorbar(im)
    # im = ax[1].imshow(totals)
    # fig.colorbar(im)
    # fig.savefig('test.png')


def qc_precip():
    """ Do the quality control on the precip product """
    mrms_total = np.sum(precip, 0)
    # So what is our logic here.  We should care about aggregious differences
    # Lets make MRMS be within 33% of stage IV
    ratio = mrms_total / stage4
    print_threshold = 0
    # (myx, myy) = get_xy_from_lonlat(-91.44, 41.28)
    # print myx, myy
    for y in range(YS):
        for x in range(XS):
            # if x == myx and y == myy:
            #    print precip[:, y, x]
            if ratio[y, x] < 1.3:
                continue
            # Don't fuss over small differences, if mrms_total is less
            # than 10 mm
            if mrms_total[y, x] < 10:
                continue
            # Pull the functional form down to stage4 total
            precip[:, y, x] = precip[:, y, x] / ratio[y, x]
            # if x == myx and y == myy:
            #    print precip[:, y, x]

            # limit the amount of printout we do, not really useful anyway
            if mrms_total[y, x] > print_threshold:
                print(('QC y: %3i x: %3i stageIV: %5.1f MRMS: %5.1f New: %5.1f'
                       ) % (y, x, stage4[y, x], mrms_total[y, x],
                            np.sum(precip[:, y, x])))
                print_threshold = mrms_total[y, x]

    # basedir = "/mnt/idep2/data/dailyprecip/2015"
    # np.save(valid.strftime(basedir+"/%Y%m%d_ratio.npy"), ratio)
    # np.save(valid.strftime(basedir+"/%Y%m%d_mrms_total.npy"), mrms_total)


def load_precip_legacy(valid):
    """ Compute a Legacy Precip product for dates prior to 1 Jan 2014"""
    LOG.debug("load_precip_legacy() called...")
    ts = 12 * 24  # 5 minute

    midnight = datetime.datetime(valid.year, valid.month, valid.day, 12, 0)
    midnight = midnight.replace(tzinfo=pytz.timezone("UTC"))
    midnight = midnight.astimezone(pytz.timezone("America/Chicago"))
    midnight = midnight.replace(hour=0, minute=0, second=0)
    # clever hack for CST/CDT
    tomorrow = midnight + datetime.timedelta(hours=36)
    tomorrow = tomorrow.replace(hour=0)

    top = int((50. - NORTH) * 100.)
    bottom = int((50. - SOUTH) * 100.)

    right = int((EAST - -126.) * 100.)
    left = int((WEST - -126.) * 100.)

    now = midnight
    m5 = np.zeros((ts, YS, XS), np.float16)
    tidx = 0
    # Load up the n0r data, every 5 minutes
    while now < tomorrow:
        utc = now.astimezone(pytz.timezone("UTC"))
        fn = utc.strftime(("/mesonet/ARCHIVE/data/%Y/%m/%d/GIS/uscomp/"
                           "n0r_%Y%m%d%H%M.png"))
        if os.path.isfile(fn):
            if tidx >= ts:
                # Abort as we are in CST->CDT
                break
            img = Image.open(fn)
            imgdata = np.array(img)
            # Convert the image data to dbz
            dbz = (np.flipud(imgdata[top:bottom, left:right]) - 7.) * 5.
            m5[tidx, :, :] = np.where(dbz < 255,
                                      ((10. ** (dbz/10.)) / 200.) ** 0.625,
                                      0)
        else:
            print('daily_clifile_editor missing: %s' % (fn,))

        now += datetime.timedelta(minutes=5)
        tidx += 1
    LOG.debug("load_precip_legacy() finished loading N0R Composites")

    m5total = np.sum(m5, 0)

    minute2 = np.arange(0, 60 * 24, 2)
    minute5 = np.arange(0, 60 * 24, 5)

    def _compute(y, x):
        s4total = stage4[y, x]
        if s4total < 1:
            return
        five = m5total[y, x]
        # TODO unsure of this... Smear the precip out over the first hour
        if five < 10:
            precip[0:30, y, x] = s4total / 30.
            return
        # Interpolate weights to a 2 minute interval grid
        # we divide by 2.5 to downscale the 5 minute values to 2 minute
        weights = np.interp(minute2, minute5, m5[:, y, x] / five / 2.5)
        # Now apply the weights to the s4total
        precip[:, y, x] = weights * s4total

    [_compute(y, x) for y in range(YS) for x in range(XS)]

    LOG.debug("load_precip_legacy() finished precip calculation")


def load_precip(valid):
    """ Load the 5 minute precipitation data into our ginormus grid """
    ts = 30 * 24  # 2 minute

    midnight = datetime.datetime(valid.year, valid.month, valid.day, 12, 0)
    midnight = midnight.replace(tzinfo=pytz.timezone("UTC"))
    midnight = midnight.astimezone(pytz.timezone("America/Chicago"))
    midnight = midnight.replace(hour=0, minute=0, second=0)
    # clever hack for CST/CDT
    tomorrow = midnight + datetime.timedelta(hours=36)
    tomorrow = tomorrow.replace(hour=0)

    top = int((55. - NORTH) * 100.)
    bottom = int((55. - SOUTH) * 100.)

    right = int((EAST - -130.) * 100.)
    left = int((WEST - -130.) * 100.)
    # (myx, myy) = get_xy_from_lonlat(-93.6, 41.99)
    # samplex = int((-96.37 - -130.)*100.)
    # sampley = int((55. - 42.71)*100)

    # Oopsy we discovered a problem
    a2m_divisor = 10. if (valid < datetime.date(2015, 1, 1)) else 50.

    now = midnight
    while now < tomorrow:
        utc = now.astimezone(pytz.timezone("UTC"))
        fn = utc.strftime(("/mesonet/ARCHIVE/data/%Y/%m/%d/GIS/mrms/"
                           "a2m_%Y%m%d%H%M.png"))
        if os.path.isfile(fn):
            tidx = int((now - midnight).seconds / 120.)
            if tidx >= ts:
                # Abort as we are in CST->CDT
                return precip
            img = Image.open(fn)
            # --------------------------------------------------
            # OK, once and for all, 0,0 is the upper left!
            # units are 0.1mm
            imgdata = np.array(img)
            # sample out and then flip top to bottom!
            data = np.flipud(imgdata[top:bottom, left:right])
            # print now, data[myy, myx]
            # print np.shape(imgdata), bottom, top, left, right
            # print now, imgdata[sampley, samplex]
            # if imgdata[sampley, samplex] > 0:
            #    import matplotlib.pyplot as plt
            #    (fig, ax) = plt.subplots(2,1)
            #    ax[0].imshow(imgdata[0:3000, :])
            #    ax[1].imshow(data)
            #    fig.savefig('test.png')
            #    sys.exit()
            # Turn 255 (missing) into zeros
            precip[tidx, :, :] = np.where(data < 255, data / a2m_divisor, 0)

        else:
            print 'daily_clifile_editor missing: %s' % (fn,)

        now += datetime.timedelta(minutes=2)


def bpstr(ts, accum):
    """Make a string representation of this breakpoint and accumulation"""
    return "%02i.%02i  %6.2f" % (ts.hour, ts.minute / 60.0 * 100.,
                                 accum)


def compute_breakpoint(ar, accumThreshold=2., intensityThreshold=1.):
    """ Compute the breakpoint data based on this array of data!

    To prevent massive ASCII text files, we do some simplification to the
    precipitation dataset.  We want to retain the significant rates though.

    Args:
      ar (array-like): precipitation accumulations every 2 minutes...
      accumThreshold (float): ammount of accumulation before writing bp
      intensityThreshold (float): ammount of intensity before writing bp

    Returns:
        list(str) of breakpoint precipitation
    """
    total = np.sum(ar)
    # Any total less than (0.01in) is not of concern, might as well be zero
    if total < 0.254:
        return []
    bp = None
    # in mm
    accum = 0
    lastaccum = 0
    lasti = 0
    for i, intensity in enumerate(ar):
        if intensity == 0:
            continue
        # Need to initialize the breakpoint data
        if bp is None:
            ts = ZEROHOUR + datetime.timedelta(minutes=(i*2))
            bp = [bpstr(ts, 0), ]
        accum += intensity
        lasti = i
        if ((accum - lastaccum) > accumThreshold or
                intensity > intensityThreshold):
            lastaccum = accum
            if (i + 1) == len(ar):
                ts = ZEROHOUR.replace(hour=23, minute=59)
            else:
                ts = ZEROHOUR + datetime.timedelta(minutes=((i+1)*2))
            bp.append(bpstr(ts, accum))
    if accum != lastaccum:
        # print("accum: %.5f lastaccum: %.5f lasti: %s" % (accum, lastaccum,
        #                                                 lasti))
        if (lasti + 1) == len(ar):
            ts = ZEROHOUR.replace(hour=23, minute=59)
        else:
            ts = ZEROHOUR + datetime.timedelta(minutes=((lasti + 1)*2))
        bp.append("%02i.%02i  %6.2f" % (ts.hour, ts.minute / 60.0 * 100.,
                                        accum))
    return bp


def myjob(row):
    """ Thread job, yo """
    [x, y] = row
    lon = WEST + x * 0.01
    lat = SOUTH + y * 0.01
    fn = "/i/%s/cli/%03.0fx%03.0f/%06.2fx%06.2f.cli" % (SCENARIO,
                                                        0 - lon,
                                                        lat,
                                                        0 - lon,
                                                        lat)
    if not os.path.isfile(fn):
        return False

    # Okay we have work to do
    data = open(fn, 'r').read()
    pos = data.find(valid.strftime("%-d\t%-m\t%Y"))
    if pos == -1:
        print 'Date find failure for %s' % (fn,)
        return False

    pos2 = data[pos:].find(
            (valid + datetime.timedelta(days=1)).strftime("%-d\t%-m\t%Y"))
    if pos2 == -1:
        print 'Date2 find failure for %s' % (fn,)
        return False

    bpdata = compute_breakpoint(precip[:, y, x])

    thisday = ("%s\t%s\t%s\t%s\t%3.1f\t%3.1f\t%4.0f\t%4.1f\t%s\t%4.1f\n%s%s"
               ) % (valid.day, valid.month, valid.year, len(bpdata),
                    high_temp[y, x], low_temp[y, x], solar[y, x],
                    wind[y, x], 0, dewpoint[y, x], "\n".join(bpdata),
                    "\n" if len(bpdata) > 0 else "")

    o = open(fn, 'w')
    o.write(data[:pos] + thisday + data[(pos+pos2):])
    o.close()
    return True


def save_daily_precip(valid):
    """Save off the daily precip totals for usage later in computing huc_12"""
    data = np.sum(precip, 0)
    basedir = "/mnt/idep2/data/dailyprecip/"+str(valid.year)
    if not os.path.isdir(basedir):
        os.makedirs(basedir)
    np.save(valid.strftime(basedir+"/%Y%m%d.npy"), data)
    # save Stage IV as well, for later hand wringing
    # np.save(valid.strftime(basedir+"/%Y%m%d_stageIV.npy"), stage4)


def precip_workflow(valid):
    """Drive the precipitation workflow

    Args:
      valid (date): The date that we care about
    """
    load_stage4(valid)
    if valid.year >= 2014:
        load_precip(valid)
    else:
        load_precip_legacy(valid)
    qc_precip()
    save_daily_precip(valid)


def workflow():
    """ The workflow to get the weather data variables we want! """

    # 1. Max Temp C
    # 2. Min Temp C
    # 3. Radiation l/d
    # 4. wind mps
    # 6. Mean dewpoint C
    load_iemre(valid)
    # 5. wind direction (always zero)
    # 7. breakpoint precip mm
    precip_workflow(valid)

    QUEUE = []
    for y in range(YS):
        for x in range(XS):
            QUEUE.append([x, y])

    pool = Pool()  # defaults to cpu-count
    sz = len(QUEUE)
    sts = datetime.datetime.now()
    success = 0
    lsuccess = 0
    for i, res in enumerate(pool.imap_unordered(myjob, QUEUE), 1):
        if res:
            success += 1
        if success > 0 and success % 20000 == 0 and lsuccess != success:
            delta = datetime.datetime.now() - sts
            secs = delta.microseconds / 1000000. + delta.seconds
            rate = success / secs
            remaining = ((sz - i) / rate) / 3600.
            print ('%5.2fh Processed %6s/%6s/%6s [%.2f /sec] '
                   'remaining: %5.2fh') % (secs / 3600., success, i, sz, rate,
                                           remaining)
            lsuccess = success
    print('daily_clifile_editor edited %s files...' % (success,))


if __name__ == '__main__':
    # This is important to keep valid in global scope
    valid = datetime.date.today() - datetime.timedelta(days=1)
    if len(sys.argv) == 5:
        valid = datetime.date(int(sys.argv[2]), int(sys.argv[3]),
                              int(sys.argv[4]))

    workflow()


class test(unittest.TestCase):

    def test_speed(self):
        """Test the speed of the processing"""
        global SCENARIO, valid
        SCENARIO = 0
        valid = datetime.date(2014, 10, 10)
        sts = datetime.datetime.now()
        myjob(get_xy_from_lonlat(-91.44, 41.28))
        ets = datetime.datetime.now()
        delta = (ets - sts).total_seconds()
        print(("Processed 1 file in %.5f secs, %.0f files per sec"
               ) % (delta, 1.0 / delta))
        self.assertTrue(1 == 0)

    def test_bp(self):
        """ issue #6 invalid time """
        data = np.zeros([30*24])
        data[0] = 3.2
        bp = compute_breakpoint(data)
        self.assertEqual(bp[0], "00.00    0.00")
        self.assertEqual(bp[1], "00.03    3.20")
        data[0] = 0
        data[24*30 - 1] = 9.99
        bp = compute_breakpoint(data)
        self.assertEqual(bp[0], "23.96    0.00")
        self.assertEqual(bp[1], "23.98    9.99")

        data[24*30 - 1] = 10.99
        bp = compute_breakpoint(data)
        self.assertEqual(bp[0], "23.96    0.00")
        self.assertEqual(bp[1], "23.98   10.99")

        # Do some random futzing
        for _ in range(1000):
            data = np.random.randint(20, size=(30*24,))
            bp = compute_breakpoint(data)
            lastts = -1
            lastaccum = -1
            for b in bp:
                tokens = b.split()
                if float(tokens[0]) <= lastts or float(tokens[1]) <= lastaccum:
                    print data
                    print bp
                    self.assertTrue(1 == 0)
                lastts = float(tokens[0])
                lastaccum = float(tokens[1])
                self.assertTrue(True)
