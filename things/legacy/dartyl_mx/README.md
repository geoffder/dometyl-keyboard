# Dartyl MX Edition

![Dartyl MX - Top](images/top.jpg)

## Bill of Materials

| Part name                                                    | Amount  |
| -------------------------------------------------------------| --------|
| [Cases][case] (left and right)                               | 2       |
| MX switches of your choice                                   | 34      |
| Any MX compatible keycaps should work, I used MT3            | 30 / 34 |
| Convex thumb caps, I resin printed [these][convx] (optional) | 4       |
| MX [1u pcb][pcb]'s (optional)                                | 34      |
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

The [convex keypcap][convx] in the above BOM is credited to [pseudoku](https://github.com/pseudoku/PseudoMakeMeKeyCapProfiles) who also [sells some beautiful custom keycaps](https://www.asymplex.xyz/).

## Grip

I used "[Reusable Double-Sided Adhesive Nano Tape][tape]" for grip and it works very well. I've also used the rubber between bumpons from a sheet of bumpons, but it's not as good.

## MCU Holders

I used a modified version of [this mcu holder](https://github.com/dereknheiley/dactyl-manuform-tight/blob/master/things/usb_holder_w_reset.stl) as the usb-c pro micros are a little longer than the regular pro micros. I've included my modified [holder stl](../dartyl_choc/usb_holder_promicro_usbc.stl) and [scad](../dartyl_choc/usb_holder.scad).

[convx]: MX_DES_Convex_012tol.stl
[pcb]: https://github.com/swanmatch/MxLEDBitPCB/blob/master/readme_en.md
[trrs]: https://www.aliexpress.com/item/1005001928651798.html
[tape]: https://www.aliexpress.com/item/1005001598557779.html
[case]: dartyl_mx.stl
