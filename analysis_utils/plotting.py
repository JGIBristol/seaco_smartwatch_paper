"""
Utilities for plotting

"""

import matplotlib.pyplot as plt
from . import clean


def shade_ramadan(axis: plt.Axes, **kwargs) -> None:
    """
    Shade the Ramadan period on a plot

    """
    ymin, ymax = axis.get_ylim()

    ramadan_dates = clean.ramadan_2022()
    axis.fill_between(
        ramadan_dates,
        [ymin, ymin],
        [ymax, ymax],
        color="k" if "color" not in kwargs else kwargs["color"],
        alpha=0.25 if "alpha" not in kwargs else kwargs["alpha"],
    )

    axis.set_ylim(ymin, ymax)
