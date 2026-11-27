import numpy as np
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt

from statannotations.Annotator import Annotator
from scipy import stats
from statsmodels.stats.multitest import multipletests


mpl.rcParams.update({
    "font.family": "Avenir",
    "font.sans-serif": ["Avenir"],
    "axes.titlesize": 14,
    "axes.labelsize": 12,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "legend.fontsize": 10,
})

sns.set_theme(style="whitegrid", font="Avenir")

CONDITIONS = ["No explanation", "Push explanation", "Pull explanation"]

COLORS = {
    "Lumo": "#6929c4",
    "Coda": "#1192e8",
    "Nelo": "#005d5d",
    "Pull explanation": "#6929c4",
    "Push explanation": "#1192e8",
    "No explanation": "#005d5d",
}

def sig_stars(p):
    if p < 0.001:
        return "***"
    if p < 0.01:
        return "**"
    if p < 0.05:
        return "*"
    return "ns"


def paired_ttests(df, dv, subject_col="participant_id", alternative="two-sided"):
    wide = (
        df.pivot_table(
            index=subject_col,
            columns="condition",
            values=dv,
            aggfunc="mean"
        )
        .dropna(subset=CONDITIONS)
    )

    pairs = [
        ("Push explanation", "No explanation"),
        ("Pull explanation", "No explanation"),
        ("Pull explanation", "Push explanation"),
    ]

    t_values = []
    p_values = []

    for c1, c2 in pairs:
        t, p = stats.ttest_rel(wide[c1], wide[c2], alternative=alternative)
        t_values.append(t)
        p_values.append(p)

    _, p_adj, _, _ = multipletests(p_values, method="holm")

    results = {}

    for (c1, c2), t, p_raw, p_holm in zip(pairs, t_values, p_values, p_adj):
        results[(c1, c2)] = {
            "t": t,
            "p_raw": p_raw,
            "p_holm": p_holm,
            "sig": sig_stars(p_holm),
            "n": len(wide),
        }

    return results


def plot_qq_by_condition(df, dv, condition_col="condition", title=None):
    conditions = df[condition_col].unique()

    fig, axes = plt.subplots(1, len(conditions), figsize=(5 * len(conditions), 5))

    if len(conditions) == 1:
        axes = [axes]

    for ax, cond in zip(axes, conditions):
        subset = df[df[condition_col] == cond][dv].dropna()

        stats.probplot(subset, dist="norm", plot=ax)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.spines["left"].set_color("#ccc")
        ax.spines["bottom"].set_color("#ccc")
        ax.tick_params(axis="x", which="both", length=4, width=1, color="black")
        ax.tick_params(
            axis="y",
            which="both",
            length=4,
            width=1,
            color="black",
            direction="out",
            labelsize=10,
        )
        ax.set_facecolor("#FFFFFF")
        ax.get_lines()[0].set_markerfacecolor("#BFD7EA")
        ax.get_lines()[0].set_markeredgecolor("#087E8B")
        ax.grid(axis="y", color="#ddd", linewidth=0.5, linestyle="--")
        ax.grid(axis="x", color="#ddd", linewidth=0.5, linestyle="--")
        ax.get_lines()[1].set_color("#C81D25")

        ax.set_title(f"{title} – {cond}")

    plt.tight_layout()
    plt.show()


def stylized_boxplot(
    df,
    metric,
    annotations=None,
    show=False,
):
    settings = {
        "trust": {
            "ylabel": "Trust (STS-AD)",
            "print_stats": False,
        },
        "understanding": {
            "ylabel": "System Understanding Score",
            "print_stats": False,
        },
        "nasa": {
            "ylabel": "Mental Workload (NASA-TLX)",
            "print_stats": True,
        },
    }

    if metric not in settings:
        raise ValueError(
            f"metric must be one of {list(settings.keys())}"
        )

    cfg = settings[metric]

    required_columns = {
        "participant_id",
        "condition",
        metric,
    }

    missing_columns = required_columns.difference(df.columns)

    if missing_columns:
        raise ValueError(
            f"Missing required columns: {sorted(missing_columns)}"
        )

    if annotations is not None:
        available_conditions = set(df["condition"].dropna().unique())

        for pair, text in annotations:
            if len(pair) != 2:
                raise ValueError(
                    f"Each pair must contain exactly two conditions: {pair}"
                )

            missing_conditions = set(pair).difference(
                available_conditions
            )

            if missing_conditions:
                raise ValueError(
                    f"Conditions {sorted(missing_conditions)} are not "
                    f"present in df['condition']. "
                    f"Available conditions: {sorted(available_conditions)}"
                )

            if not isinstance(text, str):
                raise TypeError(
                    "Each annotation must be a string, "
                    f"but received {type(text).__name__}."
                )

    rng = np.random.default_rng(42)

    plt.rcParams.update({
        "font.family": "Avenir",
        "font.sans-serif": ["Avenir"],
        "font.size": 12,
    })

    fig, ax = plt.subplots(
        figsize=(5.5, 4.5),
        dpi=300,
    )

    sns.boxplot(
        data=df,
        x="condition",
        y=metric,
        hue="condition",
        order=CONDITIONS,
        hue_order=CONDITIONS,
        palette=COLORS,
        width=0.6,
        fliersize=0,
        linewidth=1,
        boxprops={
            "alpha": 0.45,
        },
        medianprops={
            "color": "#111111",
            "linewidth": 1,
        },
        whiskerprops={
            "color": "#666666",
            "linewidth": 1,
        },
        capprops={
            "color": "#666666",
            "linewidth": 1,
        },
        ax=ax,
        legend=False,
    )

    for i, condition in enumerate(CONDITIONS):
        values = (
            df.loc[df["condition"] == condition, metric]
            .dropna()
            .to_numpy()
        )

        if len(values) == 0:
            continue

        jitter = rng.uniform(
            -0.12,
            0.12,
            size=len(values),
        )

        mean_value = values.mean()

        ax.scatter(
            np.full(len(values), i, dtype=float) + jitter,
            values,
            color=COLORS[condition],
            s=20,
            alpha=0.5,
            zorder=4,
            linewidths=0,
        )

        ax.hlines(
            mean_value,
            i - 0.3,
            i + 0.3,
            colors=COLORS[condition],
            linestyles="--",
            linewidth=1,
            zorder=5,
        )

    if cfg["print_stats"]:
        summary = (
            df.groupby("condition", observed=True)[metric]
            .agg(["mean", "std", "count"])
            .reindex(CONDITIONS)
        )

        print(f"\n{metric.upper()} descriptive statistics:")

        for condition, row in summary.iterrows():
            print(
                f"{condition}: "
                f"M = {row['mean']:.2f}, "
                f"SD = {row['std']:.2f}, "
                f"N = {int(row['count'])}"
            )

    ax.set_xticks(range(len(CONDITIONS)))
    ax.set_xticklabels(
        CONDITIONS,
        fontsize=12,
        color="black",
        fontfamily="Avenir",
    )

    ax.set_xlabel("")

    ax.set_ylabel(
        cfg["ylabel"],
        fontsize=14,
        fontfamily="Avenir",
    )

    ax.set_facecolor("#FFFFFF")
    ax.grid(
        axis="y",
        color="#dddddd",
        linewidth=0.5,
        linestyle="--",
    )

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_color("#cccccc")
    ax.spines["bottom"].set_color("#cccccc")

    for patch, condition in zip(ax.patches, CONDITIONS):
        patch.set_edgecolor(COLORS[condition])
        patch.set_linewidth(1.2)

    ax.xaxis.set_ticks_position("bottom")
    ax.yaxis.set_ticks_position("left")

    if annotations:
        pairs = [
            pair
            for pair, _ in annotations
        ]

        annotation_texts = [
            text
            for _, text in annotations
        ]

        annotator = Annotator(
            ax,
            pairs,
            data=df,
            x="condition",
            y=metric,
            order=CONDITIONS,
        )

        annotator.configure(
            loc="outside",
            text_format="star",
            verbose=0,
        )

        annotator.set_custom_annotations(
            annotation_texts
        )

        annotator.annotate()

    ax.tick_params(
        axis="x",
        which="both",
        length=4,
        width=1,
        color="black",
        labelsize=12,
    )

    ax.tick_params(
        axis="y",
        which="both",
        length=4,
        width=1,
        color="black",
        direction="out",
        labelsize=10,
    )

    for text in (
        list(ax.get_xticklabels())
        + list(ax.get_yticklabels())
        + [ax.xaxis.label, ax.yaxis.label]
        + list(ax.texts)
    ):
        text.set_fontfamily("Avenir")

    fig.tight_layout()

    fig.savefig(
        f"plots/boxplot_{metric}.pdf",
        dpi=300,
        bbox_inches="tight",
    )

    if show:
        plt.show()
    else:
        plt.close(fig)

    return fig, ax

