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


@cache
def smartwatch_feasibility() -> pd.DataFrame:
    """
    Get a dataframe of smartwatch feasibility data

    """
    path = pathlib.Path(_userconf()["seaco_dir"]) / _conf()["feasibility_info"]
    return pd.read_stata(path)


def add_timedelta(meal_info: pd.DataFrame) -> pd.DataFrame:
    """
    Add a column showing the delta between watch distribution date and entry date
    to a dataframe

    Also adds Datetime, residents_id column

    """
    feasibility_info = smartwatch_feasibility()

    # We only care about ones who consented to the smartwatch study
    feasibility_info = feasibility_info[feasibility_info["smartwatchwilling"] == 1]
    feasibility_info = feasibility_info[["residents_id", "actualdateofdistribution1st"]]

    # Join dataframes
    meal_info = (
        meal_info.reset_index()
        .merge(feasibility_info, left_on="p_id", right_on="residents_id", how="left")
        .set_index(meal_info.index)
    )

    meal_info["delta"] = (
        meal_info.index.to_series() - meal_info["actualdateofdistribution1st"]
    )

    assert (meal_info["residents_id"] == meal_info["p_id"]).all()

    return meal_info.drop(
        columns=["Datetime", "residents_id", "actualdateofdistribution1st"]
    )


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


def survey_info() -> pd.DataFrame:
    """
    Get survey info

    """
    path = pathlib.Path(_userconf()["seaco_dir"]) / _conf()["questionnaire"]
    return pd.read_csv(path, low_memory=False)
