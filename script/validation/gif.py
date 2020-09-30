import imageio
import os
import re

IMG_DIR = './out/img/proper2'
GIF_DIR = 'out/gif/proper2'
DURATION = 0.4


def makegif(filenames, gifname):
    images = []
    for filename in filenames:
        images.append(imageio.imread(filename))
    imageio.mimsave(gifname, images, format='GIF', duration=DURATION)


def main():
    os.makedirs(GIF_DIR, exist_ok=True)
    for sim_dir in os.listdir(IMG_DIR):
        sim_path = os.path.join(IMG_DIR, sim_dir)
        if os.path.isdir(sim_path):
            pngs = [f for f in os.listdir(sim_path) if f.endswith('.png')]
            metrics = sorted(list(set([f.split('_')[1] for f in pngs])))
            for metric in metrics:
                filenames = sorted(
                    [os.path.join(sim_path, f) for f in pngs if re.match('boxplot_' + metric + r'_\d{4}\.png', f)])
                makegif(filenames, os.path.join(GIF_DIR, '{sim}-{metric}'.format(sim=sim_dir, metric=metric)) + '.gif')
                print('{} {} done'.format(sim_dir, metric))


if __name__ == '__main__':
    main()
