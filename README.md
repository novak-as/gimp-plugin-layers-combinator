# Layers Combinator

## Purpose

To regenerate set of images (with ability to review and manually modify any of them) after dependent resources where changed. Originaly used for tileset generation, i.e. regenarating dozen images after one of a backgrounds were modified

## How to install

Check official documentation at https://docs.gimp.org/en/install-script-fu.html

## How to use

### 1. Tools/Combine Layers
---

Will generate all possible combinations based on setup of the initial image

Expected layers structure:
```
- TopMostLayerGroup[options...]
    - layer1[modificator?]
    - layer2[modificator?]
    - ...
- LayerGroup2[options...]
    - layer1[modificator?]
    - layer2[modificator?]
    - ...
- ...
- BackgroundLayerGroup[options...]
    - layer1[modificator?]
    - layer2[modificator?]
    - ...
```

Possible options:
1) `min` - minimum amount of layers in a valid result group. Default = `1`, expected: `<number>`
2) `max` - maximum amount of layers in a valid result group. Default = `*`, expected: `<number | *>`
    * `*` = no limits
3) `combine` = algorithm to iterate layers in this layer group. Default = `seq`, expected: `<seq | mix>`
    * `seq` = pick every single layer i.e. `[[1], [2]]`
    * `mix` = pick every possible combinations of layer in this layer group, i.e. `[[1], [1, 2], [2]]`

Possible modificators (only 1 can be applied):
1) `^` = use layer name as a suffix
2) `-` = don't add layer name to the final name

**Example**
```
- Decor[min=1 max=2 combine=mix]
    - 1^ 
    - 2^
    - 3^
- Back
    - b1
    - b2
    - b3
```

With prefix = `img_` and separator = `_`

Will generate set of images

1) `Back.b1 + Decor.1` with the name `img_b1_1`
2) `Back.b1 + Decor.2` with the name `img_b1_2`
3) `Back.b1 + Decor.3` with the name `img_b1_3`
4) `Back.b1 + Decor.1 + Decor.2` with the name `img_b1_1_2`
5) ...
6) `Back.b2 + Decor.2 + Decor.3` with the name `img_b2_2_3`

### 2. Tools/Flatten and save all

Will merge all layers for **EVERY** opened images and save them as a .png file

## Bugs and feature requests

https://github.com/novak-as/gimp-plugin-layers-combinator/issues

## Q&A

https://github.com/novak-as/gimp-plugin-layers-combinator/discussions