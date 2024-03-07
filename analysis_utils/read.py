import yaml
import pathlib
from functools import cache

import pandas as pd

from . import clean


@cache
def _userconf() -> dict:
    """
    User defined configuration

    """
    with open(
        pathlib.Path(__file__).resolve().parents[1] / "userconf.yaml", "r"
    ) as stream:
        return yaml.safe_load(stream)


@cache
def _conf() -> dict:
    """
    Hard coded stuff

    """
    with open(
        pathlib.Path(__file__).resolve().parents[1] / "config.yaml", "r"
    ) as stream:
        return yaml.safe_load(stream)


def raw_meal_info() -> pd.DataFrame:
    """
    Meal info exactly as it appears in the CSV

    """
    path = pathlib.Path(_userconf()["seaco_dir"]) / _conf()["meal_info"]
    return pd.read_csv(path)


def _datetime(meal_info: pd.DataFrame) -> pd.Series:
    """
    Get a series representing the timestamp

    """
    return pd.to_datetime(
        meal_info["date"].map(str) + meal_info["timestamp"], format=r"%d%b%Y%H:%M:%S"
    )


@cache
def all_meal_info(*, verbose=False) -> pd.DataFrame:
    """
    Get smartwatch meal info from the smartwatch data; sorted by entry timestamp

    :param verbose: extra print output
    :returns: dataframe where the date and timestamp are combined into a single column and set as the index

    """
    retval = raw_meal_info()

    # Find a series representing the timestamp
    retval["Datetime"] = _datetime(retval)

    # Set it as the index
    retval = retval.set_index("Datetime")

    # We likely care more about the time since the start of the study
    retval = add_timedelta(retval)

    # Remove the old date/time columns
    retval = retval.drop(["date", "timestamp"], axis=1)

    # Add catchup info
    retval = clean.flag_catchups(retval)
    retval = clean.flag_catchup_entries(retval)

    # Remove incorrect Ramadan flags
    retval = retval[[col for col in retval if "ramadanflag" not in col]]

    # Remove the start/end dates, since they're wrong
    retval = retval[[col for col in retval if col not in {"firstdate", "lastdate"}]]

    return retval
