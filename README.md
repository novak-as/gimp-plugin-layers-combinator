# TILESET GENERATOR

## How to setup

Copy both files to a separate folder and add it to the list under `Edit\Preferences\Folders\Scripts` 

**OR**

Copy both files to the default GIMP scripts directory (`C:\Users\<username>\AppData\Roaming\GIMP\2.10\scripts` or `C:\Program Files\GIMP 2\share\gimp\2.0\scripts`)

## How to use

### 1. Tools/Create all possible tiles

Will generate all possible combinations based on setup of initial image

Expected layers structure:
```
- TopMostLayerGroup
    - layer1^
    - layer2^
    - ...
- LayerGroup2!
    - layer1
    - layer2
    - ...
- ...
- BackgroundLayerGroup
    - layer1
    - layer2
    - ...
```

Adding `!` to the layer group name will force script to use every possible combination of layers inside this group
Adding `^` to the layer name will force script to use layer's name as a suffix

### 2. Tools/Flatten and save all

Will merge all layers for **EVERY** opened images and save them as a .png file