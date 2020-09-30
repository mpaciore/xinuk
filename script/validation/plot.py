import glob
import os
from statistics import median

from itertools import groupby, product
from matplotlib import pyplot as plt

# general settings
PREFIXES = ['formin', 'fortwist', 'torch']
VARIANTS = ['default', 'variant1', 'variant2', 'variant3']
WORKERS_ROOT_MAX = 4
SAMPLES = 30
ITERATIONS = {
    'formin': 1000,
    'fortwist': 1000,
    'torch': 200
}

# files settings
DATASET = 'proper6'
LOGS_BASE_DIR = os.path.join('C:\\Dane\\xinuk-log')  # stacjonarka, laptop
# LOGS_BASE_DIR = os.path.join('D:\\xinuk-log')  # praca
IMGS_BASE_DIR = 'out/img/'
LOGS_DIR = os.path.join(LOGS_BASE_DIR, DATASET)
IMGS_DIR = os.path.join(IMGS_BASE_DIR, DATASET)
IMG_EXT = 'pdf'

# plot settings
PLOT_LEGEND_LABELS = ['1x1 workers', '2x2 workers', '3x3 workers', '4x4 workers']
PLOT_LEGEND_MARKERS = ['.', '+', '^', 's']

BOXPLOT_POSITIONS = [1, 2, 3, 4, 7, 8, 9, 10, 13, 14, 15, 16, 19, 20, 21, 22]
BOXPLOT_LABEL_TICKS = [2.5, 8.5, 14.5, 20.5]
BOXPLOT_COLOR = 'k'

ERRORBAR_SKIP = 2
ERRORBAR_OFFSET_FRAC = 0.008
ERRORBAR_COLORS = ['#252525cc', '#636363cc', '#969696cc', '#cccccccc']  # greyscale with transparency
# ERRORBAR_COLORS = ['#252525', '#636363', '#969696', '#cccccc']  # greyscale
# ERRORBAR_COLORS = ['#000000', '#ff0000', '#00ff00', '#0000ff']  # colors
ERRORBAR_LINEWIDTH = 0.01
ERRORBAR_MARKERSIZE = 2


def parse_metrics(log_filename, iterations_n):
    with open(log_filename, 'r') as f:
        headers, *lines = [l.split(':')[-1] for l in f.read().strip().split('\n')[1:]]
    headers = headers.split(';')
    lines = sorted([[float(el) for el in line.split(';')] for line in lines], key=lambda x: x[0])

    # merge lines by iteration
    iter_lines = [[sum(x) for x in zip(*group)][1:] for key, group in groupby(lines, key=lambda x: x[0])]

    # aggregate to lists
    raw = {k: [] for k in headers}
    [[raw[k].append(float(v)) for k, v in zip(headers, line)] for line in iter_lines]

    # all iterations
    separate = [{k: v[i] for k, v in raw.items()} for i in range(iterations_n)]

    aggregated = {
        # aggregate to sums
        'sum': {k: sum(v) for k, v in raw.items()},
        # aggregate to averages
        'avg': {k: sum(v) / len(v) for k, v in raw.items()}
    }

    # meaningful iterations
    iterations = list(range(0, 9)) + list(range(9, 49, 5)) + list(range(49, iterations_n, 50))
    for i in iterations:
        aggregated[f'{i + 1:04d}'] = {k: v[i] for k, v in raw.items()}

    return separate, aggregated


def append_boxplot_metrics(metrics, to_add, variant, workers_root):
    for aggr_type, aggr in to_add.items():
        if aggr_type not in metrics:
            metrics[aggr_type] = {}
        metrics_aggr = metrics[aggr_type]
        for metric_type, metric_value in aggr.items():
            if metric_type not in metrics_aggr:
                metrics_aggr[metric_type] = {}
            metric_aggr = metrics_aggr[metric_type]
            if variant not in metric_aggr:
                metric_aggr[variant] = {}
            variant_metric_aggr = metric_aggr[variant]
            if workers_root not in variant_metric_aggr:
                variant_metric_aggr[workers_root] = []
            variant_metric_aggr[workers_root].append(metric_value)


def append_errorbar_metrics(metrics, to_add, variant, workers_root, iterations_n):
    for i, iteration_metrics in enumerate(to_add):
        for metric_type, metric_value in iteration_metrics.items():
            if metric_type not in metrics:
                metrics[metric_type] = {}
            metric = metrics[metric_type]
            if variant not in metric:
                metric[variant] = {}
            variant_metric = metric[variant]
            if workers_root not in variant_metric:
                variant_metric[workers_root] = [[] for _ in range(iterations_n)]
            variant_metric[workers_root][i].append(metric_value)


def h(aggr_type):
    i = 0
    while i < len(aggr_type) - 1 and aggr_type[i] == '0':
        i += 1
    return (' ' * i) + aggr_type[i:]


def plot_boxplots(prefix, metric, aggr_type, data):
    plt.boxplot([data[v][w] for v, w in product(VARIANTS, range(1, WORKERS_ROOT_MAX + 1))],
                medianprops={'color': BOXPLOT_COLOR},
                positions=BOXPLOT_POSITIONS,
                manage_xticks=True)

    for ((v, w), p) in zip(product(VARIANTS, range(1, WORKERS_ROOT_MAX + 1)), BOXPLOT_POSITIONS):
        if v == VARIANTS[0]:
            plt.plot([p], [median(data[v][w])],
                     color=BOXPLOT_COLOR,
                     marker=PLOT_LEGEND_MARKERS[w - 1],
                     label=PLOT_LEGEND_LABELS[w - 1])
        else:
            plt.plot([p], [median(data[v][w])],
                     color=BOXPLOT_COLOR,
                     marker=PLOT_LEGEND_MARKERS[w - 1])

    plt.xticks(BOXPLOT_LABEL_TICKS, VARIANTS)
    plt.title(f's: {prefix}, m: {metric}, a: {h(aggr_type)}')
    plt.tight_layout(rect=[0, 0, 0.8, 1])
    plt.legend(loc='right', bbox_to_anchor=(1.33, 0.5))

    prefix_dir = os.path.join(IMGS_DIR, prefix)
    os.makedirs(prefix_dir, exist_ok=True)
    plt.savefig(os.path.join(prefix_dir, f'boxplot_{metric}_{aggr_type}.{IMG_EXT}'))
    plt.clf()


def mean_and_std(data):
    mean = sum(data) / len(data)
    std = (sum([(d - mean) ** 2 for d in data]) / len(data)) ** (1 / 2)
    return mean, std


def plot_errorbars(prefix, metric, variant, data):
    base_x = range(1, ITERATIONS[prefix] + 1)
    xticks = range(0, ITERATIONS[prefix] + 1, 100)
    offset = 0
    for w in range(1, WORKERS_ROOT_MAX + 1):
        means_with_stds = list(map(mean_and_std, data[w]))[::ERRORBAR_SKIP]
        means, stds = zip(*means_with_stds)
        x = list(map(lambda v: v + offset, base_x[::ERRORBAR_SKIP]))
        plt.errorbar(x, means,
                     yerr=stds,
                     linewidth=ERRORBAR_LINEWIDTH,
                     markersize=ERRORBAR_MARKERSIZE,
                     color=ERRORBAR_COLORS[w - 1],
                     marker=PLOT_LEGEND_MARKERS[w - 1],
                     label=PLOT_LEGEND_LABELS[w - 1])
        plt.xticks(xticks)
        offset += ERRORBAR_OFFSET_FRAC * ITERATIONS[prefix]

    plt.title(f's: {prefix}, m: {metric}, v: {variant}')
    plt.ylabel(metric)
    plt.xlabel('iteration')
    plt.tight_layout(rect=[0, 0, 0.8, 1])
    plt.legend(loc='right', bbox_to_anchor=(1.33, 0.5))

    prefix_dir = os.path.join(IMGS_DIR, prefix)
    os.makedirs(prefix_dir, exist_ok=True)
    plt.savefig(os.path.join(prefix_dir, f'errorbar_{metric}_{variant}.{IMG_EXT}'))
    plt.clf()


# ======================================================================================================================


def get_log_filename(workers_root, iteration):
    return f'{workers_root}_{iteration}_*.log'


def check_path(path):
    if os.path.exists(path):
        return True
    print(f'"{path}" does not exist, skipping')
    return False


def run(prefix):
    errorbar_metrics = {}
    boxplot_metrics = {}
    print(f'simulation: {prefix}')
    prefix_dir = os.path.join(LOGS_DIR, prefix)
    if not check_path(prefix_dir):
        return
    for variant in VARIANTS:
        print(f'  variant: {variant}')
        variant_dir = os.path.join(prefix_dir, variant)
        if not check_path(variant_dir):
            continue
        for workers_root in range(1, WORKERS_ROOT_MAX + 1):
            print(f'    workers: {workers_root}')
            for iteration in range(1, SAMPLES + 1):
                log_filename_wildcarded = os.path.join(variant_dir, get_log_filename(workers_root, iteration))
                matched_files = glob.glob(log_filename_wildcarded)
                if len(matched_files) == 0:
                    print(f'"{log_filename_wildcarded}" does not match any file, skipping')
                    continue
                log_filename = matched_files[0]
                errorbar, boxplot = parse_metrics(log_filename, ITERATIONS[prefix])
                append_errorbar_metrics(errorbar_metrics, errorbar, variant, workers_root, ITERATIONS[prefix])
                append_boxplot_metrics(boxplot_metrics, boxplot, variant, workers_root)

    # print('  plotting boxplots')
    # for aggr_type in boxplot_metrics:
    #     for metric in boxplot_metrics[aggr_type]:
    #         plot_boxplots(prefix, metric, aggr_type, boxplot_metrics[aggr_type][metric])

    print('  plotting errorbars')
    for metric in errorbar_metrics:
        for variant in VARIANTS:
            plot_errorbars(prefix, metric, variant, errorbar_metrics[metric][variant])

    print('done')


def main():
    import time

    start = time.time()

    [run(prefix) for prefix in PREFIXES]

    end = time.time()
    print(f'{end - start:.5f} s')


if __name__ == '__main__':
    main()
