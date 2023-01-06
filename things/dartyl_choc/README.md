# Dartyl Choc Edition

![Dartyl Choc - Top](images/dartyl_choc_top.jpg)
![Dartyl Choc - Outside](images/dartyl_choc_outside.jpg)
![Dartyl Choc - Front](images/dartyl_choc_front.jpg)
![Dartyl Choc vs. Bare PCB](images/dartyl_choc_vs_bare_pcb.jpg)
![Dartyl Choc vs. Dartyl MX](images/dartyl_choc_vs_mx.jpg)
![Dartyl Choc - Wiring](images/dartyl_choc_wiring.jpg)

## Bill of Materials

| Part name                                                    | Amount  |
| -------------------------------------------------------------| --------|
| [Cases][case] (left and right)                               | 2       |
| Choc V1 switches of your choice                              | 34      |
| Ideally MBK caps for alphas                                  | 30 / 34 |
| Convex thumb caps, I resin printed [these][talon] (optional) | 4       |
| Choc [1u pcb][pcb]'s (optional)                              | 34      |
| Pro micro compatible mcu's                                   | 2       |
| Wires (26/28 AWG solid core wire recommended)                | some    |
| [Audio jack][trrs], through hole                             | 2       |
| USB cable for the mcu you used                               | 1       |
| TRRS / male-male audio cable                                 | 1       |
| Rubber / Tape                                                | some    |
| MCU Holders (see below)                                      | 2       |

## Wiring

This case has 34 keys which means you can use QMK's `DIRECT_PINS` feature instead of wiring a grid with diodes.

## Key Caps

I tried [talon][talon] keycaps but they didn't work with the inner column as supplied, MBK caps are the safest option.

## Grip

I used "[Reusable Double-Sided Adhesive Nano Tape][tape]" for grip and it works very well. I've also used the rubber between bumpons from a sheet of bumpons, but it's not as good.

## MCU Holders

I used a modified version of [this mcu holder](https://github.com/dereknheiley/dactyl-manuform-tight/blob/master/things/usb_holder_w_reset.stl) as the usb-c pro micros are a little longer than the regular pro micros. I've included my modified [holder stl](usb_holder_promicro_usbc.stl) and [scad](usb_holder.scad).

[talon]: https://www.thingiverse.com/thing:4858309
[pcb]: https://github.com/ibnuda/single/tree/master/choc
[trrs]: https://www.aliexpress.com/item/1005001928651798.html
[tape]: https://www.aliexpress.com/item/1005001598557779.html
[case]: dartyl_choc.stl
