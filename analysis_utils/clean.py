"""
Data cleaning stuff

"""

import warnings
import traceback

import numpy as np
import pandas as pd

from . import util, read


def ramadan_2022() -> pd.Series:
    """
    Ramadan dates

    """
    return pd.to_datetime(["2022-04-02", "2022-05-02"])


def in_ramadan_2022(dates: pd.Series, verbose: bool = True) -> pd.Series:
    """
    Boolean mask indicating whether a series of datetimes are n ramadan

    :param dates: series of datetimes
    :param verbose: warnings
    :raises: warning if the dates are not all in 2022

    """
    # Check if the dates are all in 2022
    try:
        if set(dates.year.unique()) != {2022}:
            if verbose:
                warnings.warn(
                    f"Not all dates are in 2022: {set(dates.year.unique())=} "
                )
                traceback.print_stack(limit=5)

    except AttributeError:
        if verbose:
            warnings.warn(f"Dates are not a datetime series: {type(dates)=}")
            traceback.print_stack(limit=5)

    # Check whether they're in 2022 ramadan
    start, end = ramadan_2022()
    return (start <= dates) & (dates <= end)


def duplicates(meal_info: pd.DataFrame, delta_minutes: int = 5) -> pd.Series:
    """
    Boolean mask indicating which rows in the dataframe are duplicates

    Index in the dataframe must be sorted

    :param meal_info: smartwatch entry dataframe
    :param delta_minutes: maximum time difference between entries before they are considered duplicates

    :returns: boolean mask

    """
    assert meal_info.index.is_monotonic_increasing, "Index not sorted"

    # Bool mask for the whole dataframe
    mask = pd.Series(False, index=meal_info.index)

    # Iterate over each participant
    for p_id, group in meal_info.groupby("p_id"):
        group_mask = np.full(len(group), True)

        # Check if the previous entry matches in all these columns
        for column in ["meal_type", "portion_size", "utensil", "location"]:
            series = group[column]
            group_mask &= series.eq(series.shift(1))

        # Check whether the time is close to the previous entry
        minutes_diff = group.index.to_series().diff().dt.total_seconds().div(60)
        time_mask = minutes_diff < delta_minutes

        group_mask &= time_mask.values

        pid_mask = meal_info["p_id"] == p_id
        mask.loc[pid_mask] = group_mask

    mask.index = meal_info.index

    return mask


def flag_catchups(meal_info: pd.DataFrame) -> pd.DataFrame:
    """
    Find the catchup category for each "Catch-up start" entry in the dataframe.

    Adds a new column "catchup_category" and returns a new dataframe.
    Dataframe must have datetime as index

    Checks in this order:
        - Open-ended: no Catch-up end
        - Long: end longer than 60s after start
        - Early: started before 0800
        - Normal: end within 60s of start, started between 0800 and 0805
        - Late: started after 0805
    ValueError otherwise.

    :param meal_info: dataframe holding smartwatch entries

    :returns: a new dataframe with the catchup category column added
    :raises ValueError: if a catchup is encountered outside of these categories

    """
    copy = meal_info.copy()

    # Add a new column
    col_name = "catchup_category"
    copy["catchup_category"] = np.nan

    # Iterate rows
    # Slow, but it's fine
    in_catchup = False
    start_time = None
    for time, row in copy.iterrows():
        # We've encountered a new catchup
        if row["meal_type"] == "Catch-up start":
            # If there's no end time, then it's open-ended
            if in_catchup:
                copy.loc[start_time, col_name] = "Open-ended"

            start_time = time
            in_catchup = True
            continue

        elif row["meal_type"] == "Catch-up end":
            if not in_catchup:
                raise ValueError("Catch-up end without start")

            in_catchup = False
            catchup_length = (time - start_time).total_seconds()

            # If the end is more than 60s after the start, then it's long
            if catchup_length > 60:
                copy.loc[start_time, col_name] = "Long"
                continue

            # If the start time is before 0800, then it's early
            if start_time.hour < 8:
                copy.loc[start_time, col_name] = "Early"
                continue

            # If the start time is after 0805, then it's late
            if start_time.hour > 8 or (start_time.hour == 8 and start_time.minute > 5):
                copy.loc[start_time, col_name] = "Late"
                continue

            # Otherwise, it's normal
            copy.loc[start_time, col_name] = "Normal"

    # We've reached the end of the dataframe, but are still in a catchup. This means it's open-ended
    if in_catchup:
        warnings.warn(
            "Reached end of dataframe while in catchup: the last entry is open-ended"
        )
        copy.loc[start_time, col_name] = "Open-ended"

    return copy


def catchups_mask(meal_info: pd.DataFrame) -> pd.Series:
    """
    Find a boolean mask indicating which rows indicate the start and end of the catchup period

    :param meal_info: dataframe holding smartwatch entries

    :returns: a boolean mask indicating which rows are catchups

    """
    return meal_info["meal_type"].isin(
        ["No catch-up", "Catch-up start", "Catch-up end"]
    )


def flag_catchup_entries(meal_info: pd.DataFrame) -> pd.DataFrame:
    """
    Flag whether each entry was in the catchup period

    Adds a new column "catchup_flag" and returns a new dataframe.
    Dataframe must have datetime as index, and must have a "catchup_category" column

    :param meal_info: dataframe holding smartwatch entries

    :returns: a new dataframe with a "catchup_flag" column added
    :raises ValueError: if a catchup is encountered outside of these categories

    """
    copy = meal_info.copy()

    # Add a new column
    copy["catchup_flag"] = False

    # This is much less efficient, but I've decided to iterate over the dataframe multiple times marking each type of catchup
    # This is because it's easier to understand and debug

    # Mainline/early/late catchups
    in_catchup = False
    for time, row in copy.iterrows():
        if in_catchup:
            copy.loc[time, "catchup_flag"] = True

        if row["catchup_category"] in {"Early", "Late", "Normal"}:
            in_catchup = True

        elif row["meal_type"] == "Catch-up end":
            in_catchup = False

    # Long catchup
    iterator = copy.iterrows()
    for time, row in iterator:
        if row["catchup_category"] == "Long":
            # Check if the first entry is a no response
            next_time, next_row = next(iterator, None)
            assert (
                next_row["meal_type"] == "No response"
            ), "Long catchup not started with no response"

            # Warn the user (me) that we're assuming the next entry isn't a real catch-up
            next_time, next_row = next(iterator, None)
            warnings.warn(
                f"{util.bcolour.OKBLUE}Long catchup: not marking"
                f" {next_row['meal_type']} at {next_time} as catchup"
                f"{util.bcolour.ENDC}"
            )

            # Check that the next entry is the end of the catch-up period
            _, next_row = next(iterator, None)
            assert next_row["meal_type"] == "Catch-up end", "Long catchup not ended"

    # Open-ended catchups
    iterator = copy.iterrows()
    in_catchup = False
    n_open_ended = 0
    for time, row in iterator:
        # If we encounter an open catch-up period, go into a loop
        if row["catchup_category"] == "Open-ended":
            n_open_ended += 1
            # Look through rows
            while True:
                next_time, next_row = next(iterator, None)
                # If we encounter a No catch-up, then we're out of the catch-up period
                if next_row["meal_type"] == "No catch-up":
                    print(
                        f" Open ended catchup at {util.bcolour.WARNING}{time}{util.bcolour.ENDC} ended by No catch-up at {util.bcolour.WARNING}{next_time}{util.bcolour.ENDC}"
                    )
                    break

                # If we encounter a No Response, then we're out of the catch-up period
                if next_row["meal_type"] == "No response":
                    print(
                        f" Open ended catchup at {util.bcolour.OKGREEN}{time}{util.bcolour.ENDC} ended by No response at {util.bcolour.OKGREEN}{next_time}{util.bcolour.ENDC}"
                    )
                    break

                # If we encounter a time > 30 minutes after the start, then we're out of the catch-up period
                if (next_time - time) > pd.Timedelta(minutes=30):
                    print(
                        f" Open ended catchup at {util.bcolour.FAIL}{time}{util.bcolour.ENDC} ended by long wait at {util.bcolour.FAIL}{next_time}{util.bcolour.ENDC}"
                    )
                    break

                # If we encounter a non-open-ended catch-up start, then we are out of the catch-up period
                if (
                    next_row["meal_type"] == "Catch-up start"
                    and next_row["catchup_category"] != "Open-ended"
                ):
                    print(
                        f" Open ended catchup at {util.bcolour.FAIL}{time}{util.bcolour.ENDC} ended by Catch-up start at {util.bcolour.FAIL}{next_time}{util.bcolour.ENDC}"
                    )
                    break

                # If we encounter a meal, drink or snack within 5 minutes of the start, mark it as catch-up and keep iterating
                if next_row["meal_type"] in {"Meal", "Drink", "Snack"} and (
                    next_time - time
                ) < pd.Timedelta(minutes=5):
                    copy.loc[next_time, "catchup_flag"] = True
                    continue

                # Something I haven't considered
                else:
                    raise ValueError(f"{time}, {next_time}")

    # Just to check sanity
    print(f"{n_open_ended=}")
    return copy


def remove_catchups(meal_info: pd.DataFrame) -> pd.DataFrame:
    """
    From a dataframe of meal info, remove the catch-up entries and their flags

    Must have added these already

    """
    keep_mask = meal_info["catchup_flag"] == False
    keep_mask &= ~catchups_mask(meal_info)

    # Also remove no responses at 8am, since these are entries where the participant ignored the catch up prompt
    # This is where the time is within 15min of 8am
    index_datetime = pd.to_datetime(meal_info.index)
    time_diff = pd.Series(
        (index_datetime - index_datetime.normalize() - pd.to_timedelta("08:00:00")),
        index=meal_info.index,
    ).abs()
    discard = (time_diff < pd.Timedelta("15min")) & (
        meal_info["meal_type"] == "No response"
    )
    keep_mask &= ~discard

    return meal_info[keep_mask]


def _first_last_date(meal_info: pd.DataFrame) -> tuple[pd.Series, pd.Series]:
    """
    Return a series of the first date for each participant

    :param meal_info: dataframe of smartwatch entries

    :returns: series of the first and last date for each participant. Indexed by p_id

    """
    dates = meal_info.reset_index().groupby("p_id")["Datetime"]
    return dates.first(), dates.last()


def _ramadan_info(meal_info: pd.DataFrame, verbose: bool) -> pd.DataFrame:
    """
    Information about whether the participant's period intersected with Ramadan

    """
    # Whether the first and last entry for this participant was within Ramadan
    first, last = _first_last_date(meal_info)
    ramadan_df = (
        in_ramadan_2022(first, verbose=verbose)
        .to_frame()
        .rename(columns={"Datetime": "first_in_ramadan"})
    )  # Find whether the first entry was in ramadan
    ramadan_df = ramadan_df.merge(
        in_ramadan_2022(last, verbose=verbose)
        .to_frame()
        .rename(columns={"Datetime": "last_in_ramadan"}),
        on="p_id",
    )  # Find whether the last entry was in ramadan, and add it to the df

    ramadan_df["all_in_ramadan"] = (
        ramadan_df["first_in_ramadan"] & ramadan_df["last_in_ramadan"]
    )

    ramadan_df["any_in_ramadan"] = (
        ramadan_df["first_in_ramadan"] | ramadan_df["last_in_ramadan"]
    )

    return ramadan_df


def clean_meal_info(
    meal_df: pd.DataFrame,
    *,
    keep_catchups: bool,
    verbose: bool = False,
    keep_day0: bool = False,
) -> pd.DataFrame:
    """
    Clean the provided meal info dataframe.

    Returns a dataframe of meal time info that has:
        - had duplicates removed (as defined above)
        - had events before the participant watch distribution date removed
        - had events on the watch distribution date removed
        - had events more than 7 days after the distribution date removed

    :param meal_df: dataframe of meal info
    :param keep_catchups: whether to keep catchup markers and entries
    :param verbose: extra print information
    :param keep_day0: whether to keep day 0 entries (i.e. the first non-full day)

    :returns: a cleaned copy of the dataframe

    """
    retval = meal_df.copy()

    retval = retval.sort_index(inplace=False)

    # Remove entries from bad participants
    bad_pid = {
        10214,  # Afraid to take it to school
        15671,  # Respondent remove EMA app from smartwatch, already reinstall but no data detected and failed to extract the data.
    }
    retval = retval[~retval["p_id"].isin(bad_pid)]

    # Find early entries
    first_day = 0 if keep_day0 else 1
    retval = retval[retval["delta"].dt.days >= first_day]

    # Find late entries
    retval = retval[retval["delta"].dt.days <= 7]

    # Find duplicates
    retval = retval[~duplicates(retval)]

    # Optionally remove catchups
    if not keep_catchups:
        retval = remove_catchups(retval)

    # Add Ramadan info
    # Whether each entry was within Ramadan
    retval["entry_in_ramadan"] = in_ramadan_2022(retval.index, verbose=verbose)

    # Whether the participants period was within Ramadan
    ramadan_info = _ramadan_info(retval, verbose=verbose)

    # Whether the participant's last positive entry was on day 7
    last_entry = {
        p_id: retval[
            (retval["p_id"] == p_id)
            & (retval["meal_type"].isin({"Meal", "Drink", "Snack", "No food/drink"}))
        ]["delta"].dt.days.max()
        for p_id in retval["p_id"].unique()
    }
    retval["early_stop"] = False
    for pid in retval["p_id"].unique():
        if last_entry[pid] < 7:
            retval.loc[retval["p_id"] == pid, "early_stop"] = True

    # Need to do this to preserve the index
    retval["Datetime"] = retval.index
    return retval.merge(ramadan_info, on="p_id").set_index("Datetime").sort_index()


def cleaned_smartwatch(*, keep_catchups: bool, keep_day0: bool) -> pd.DataFrame:
    return clean_meal_info(
        read.all_meal_info(), keep_catchups=keep_catchups, keep_day0=keep_day0
    )


def cleaned_survey() -> pd.DataFrame:
    """
    Survey responses with:
     - only those who agreed to take part
     - column added for age group
     - bad participants (those who deleted the data, or those who didn't use the watch) removed

    """
    # Get survey dataframe
    survey_df = read.survey_info()

    # Remove those who didn't agree to take part in the SEACO study
    survey_df = survey_df[survey_df["respondent_status"] == 1]

    # Add age group column
    age_lims = 7, 9, 18
    survey_df["child"] = (survey_df["age_dob"] >= age_lims[0]) & (
        survey_df["age_dob"] <= age_lims[1]
    )
    survey_df["adolescent"] = (survey_df["age_dob"] > age_lims[1]) & (
        survey_df["age_dob"] <= age_lims[2]
    )

    # Check no overlap
    assert not (survey_df["child"] & survey_df["adolescent"]).any()

    # Check no unassigned
    assert (survey_df["child"] | survey_df["adolescent"]).all()

    # Remove participant who was afraid to take it to school
    survey_df = survey_df[survey_df["residents_id"] != 10214]

    # Remove participant who deleted the data
    # survey_df = survey_df[survey_df["residents_id"] != 15671]

    return survey_df
