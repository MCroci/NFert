#!/usr/bin/env python3
"""Conceptual scheme of the crop nitrogen balance computed by N_balance().

Grayscale diagram: the terms that raise the nitrogen requirement (crop demand,
leaching, immobilization) and those that lower it (soil supply, previous crop,
organic contributions, atmospheric deposition) combine into the mineral N to
apply. Run: python fig_nbalance_scheme_pubfig.py
"""
import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch
import pubfig

pubfig.set_style("elsevier")
fig, ax = pubfig.new_figure(width="double", aspect=0.46, journal="elsevier")
ax.set_xlim(0, 12); ax.set_ylim(0, 7); ax.axis("off")

def box(cx, cy, text, w=4.6, h=0.78, fc="white"):
    ax.add_patch(FancyBboxPatch((cx - w/2, cy - h/2), w, h, mutation_scale=5,
                 boxstyle="round,pad=0.02", linewidth=0.8, edgecolor="black",
                 facecolor=fc, zorder=3))
    ax.text(cx, cy, text, ha="center", va="center", zorder=4)

def arrow(x0, y0, x1, y1, lw=1.0):
    ax.add_patch(FancyArrowPatch((x0, y0), (x1, y1), mutation_scale=9,
                 arrowstyle="-|>", linewidth=lw, color="black", zorder=2))

XA, XB = 2.7, 9.3
ax.text(XA, 6.7, "Raise the requirement  (+)", ha="center", fontweight="bold")
ax.text(XB, 6.7, "Lower the requirement  (-)", ha="center", fontweight="bold")

# additive terms (darker), subtractive terms (lighter)
for cy, txt in zip((5.9, 4.9, 3.9),
                   ["A: crop demand (uptake)", "C1, C2: leaching", "D: immobilization"]):
    box(XA, cy, txt, fc="0.82")
for cy, txt in zip((5.9, 4.9, 3.9, 2.9),
                   ["B1, B2: soil supply", "E: previous crop", "F, Forg: organic N",
                    "G: atmospheric deposition"]):
    box(XB, cy, txt, fc="0.93")

# result box
box(6.0, 1.0, r"Mineral N to apply  =  $A - B + C_1 + C_2 + D - E - F - F_{org} - G$",
    w=11.2, h=1.0, fc="white")

arrow(XA, 3.5, 4.2, 1.6, lw=1.2)
arrow(XB, 2.5, 7.8, 1.6, lw=1.2)
pubfig.save_figure(fig, "nbalance_scheme", journal="elsevier",
                   kind="lineart", outdir=os.path.dirname(os.path.abspath(__file__)))
print("done nbalance scheme")
