#!/usr/bin/env python3

import argparse
import os
from datetime import datetime
import sys

def main():
    parser = argparse.ArgumentParser(
        description="Concatena archivos .R ordenados por fecha y agrega secciones"
    )
    parser.add_argument(
        "directorio",
        nargs="?",
        default=".",
        help="Directorio con archivos .R (default: directorio actual)"
    )
    parser.add_argument(
        "-o", "--output",
        default="concat.R",
        help="Archivo de salida (default: concat.R)"
    )

    args = parser.parse_args()
    directorio = os.path.abspath(args.directorio)

    if not os.path.isdir(directorio):
        print(f"❌ El directorio no existe: {directorio}")
        sys.exit(1)

    archivos_r = [
        f for f in os.listdir(directorio)
        if f.endswith(".R") and os.path.isfile(os.path.join(directorio, f))
    ]

    if not archivos_r:
        print("⚠ No se encontraron archivos .R")
        sys.exit(0)

    # Ordenar por fecha de modificación
    archivos_r.sort(
        key=lambda f: os.path.getmtime(os.path.join(directorio, f))
    )

    with open(args.output, "w", encoding="utf-8") as salida:
        for i, archivo in enumerate(archivos_r, start=1):
            ruta = os.path.join(directorio, archivo)
            fecha = datetime.fromtimestamp(os.path.getmtime(ruta))

            salida.write("\n")
            salida.write("# ======================================\n")
            salida.write(f"# Sección {i}\n")
            salida.write(f"# Archivo: {archivo}\n")
            salida.write(f"# Fecha: {fecha.strftime('%Y-%m-%d %H:%M:%S')}\n")
            salida.write("# ======================================\n\n")

            with open(ruta, "r", encoding="utf-8") as f:
                salida.write(f.read())
                salida.write("\n")

    print(f"✔ {len(archivos_r)} archivos concatenados en '{args.output}'")

if __name__ == "__main__":
    main()
