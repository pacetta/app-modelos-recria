"""
Reimplementación en Python de las fórmulas del Excel, para verificar
que la lógica que voy a escribir en R reproduce EXACTAMENTE los mismos
resultados antes de escribirla en R (donde no puedo ejecutar nada acá).
"""
import math
from datetime import date

# --- Parámetros (hoja "Parametros a actualizar") ---
inflacion_mensual = 0.0175       # C7
tasa_pf_tna = 0.19               # C8
tasa_costo_financiero = 0.25     # C10
dolar_hoy = 1515                 # C14
ternero_precio_abril = 6600      # C19
fecha_actualizacion = date(2026, 8, 20)  # C3
fecha_compra = date(2026, 4, 1)
meses_desde_compra = (fecha_actualizacion - fecha_compra).days / 30  # C20
ternero_hoy = ternero_precio_abril * (1 + inflacion_mensual) ** meses_desde_compra  # C21
novillito_270_hoy = 6200         # C23
novillo_gordo_hoy = 4600         # C24
novillo_expo_hoy = 4600          # C25
pct_gastos_compra = 0.05         # C29
pct_gastos_venta = 0.07          # C30
pct_dueno_capitalizacion = 0.45  # C34
pct_engordador_capitalizacion = 0.55  # C35

# --- Etapa común (E7:E20) ---
peso_inicial = 180
precio_compra = ternero_hoy
gastos_compra_pct = pct_gastos_compra
adpv_etapa = 0.7
peso_salida_comun = 270
fecha_salida_comun = date(2026, 10, 1)
duracion_etapa = (fecha_salida_comun - fecha_compra).days  # E15
kg_producidos_etapa = peso_salida_comun - peso_inicial  # E16
costo_compra_neto = peso_inicial * precio_compra * (1 + gastos_compra_pct)  # E20
sanidad_pct = 0.02  # E49
sanidad_monto = precio_compra * sanidad_pct  # G49:K49 (igual para todos)

print(f"Ternero actualizado a pesos de hoy (C21): {ternero_hoy:.6f}  (esperado: 7160.703635)")
print(f"Duración etapa (E15): {duracion_etapa}  (esperado: 183)")
print(f"Costo compra neto (E20): {costo_compra_neto:.6f}  (esperado: 1353372.987037)")
print(f"Sanidad por camino (G49): {sanidad_monto:.6f}  (esperado: 143.214073)")
print()

# --- Definición de los 5 caminos ---
caminos = {
    1: dict(nombre="Venta oct. + plazo fijo",
            peso_salida=270, fecha_salida=date(2026,10,1),
            mortandad=0.0, precio_venta_nominal=novillito_270_hoy,
            costo_alim_kg=0, gastos_estructura=0, es_capitalizacion=False),
    2: dict(nombre="Capitalización (mar)",
            peso_salida=435, fecha_salida=date(2027,3,1),
            mortandad=0.01, precio_venta_nominal=novillo_gordo_hoy,
            costo_alim_kg=0, gastos_estructura=0, es_capitalizacion=True),
    3: dict(nombre="Corral liviano (feb-jun)",
            peso_salida=415, fecha_salida=date(2027,2,1),
            mortandad=0.02, precio_venta_nominal=novillo_gordo_hoy,
            costo_alim_kg=2000, gastos_estructura=25000, es_capitalizacion=False),
    4: dict(nombre="Engorde a pastura (mar)",
            peso_salida=435, fecha_salida=date(2027,3,1),
            mortandad=0.01, precio_venta_nominal=novillo_gordo_hoy,
            costo_alim_kg=1000, gastos_estructura=15000, es_capitalizacion=False),
    5: dict(nombre="Recría+corral propio (may)",
            peso_salida=500, fecha_salida=date(2027,5,1),
            mortandad=0.015, precio_venta_nominal=novillo_expo_hoy,
            costo_alim_kg=1500, gastos_estructura=20000, es_capitalizacion=False),
}

esperados = {
    1: dict(margen=203303.798890, margen_usd=134.193927, margen_post_cf=203303.798890,
            resultado_vs_pf=203303.798890, precio_indif=5390.347276),
    2: dict(margen=-17453.970515, margen_usd=-11.520773, margen_post_cf=-159377.674477,
            resultado_vs_pf=-261816.560770, precio_indif=3733.890393),
    3: dict(margen=-48110.213602, margen_usd=-31.755917, margen_post_cf=-177170.064346,
            resultado_vs_pf=-260044.896656, precio_indif=4752.587841),
    4: dict(margen=154754.155632, margen_usd=102.147958, margen_post_cf=3392.951671,
            resultado_vs_pf=-99045.934622, precio_indif=4206.887576),
    5: dict(margen=145303.058920, margen_usd=95.909610, margen_post_cf=-80822.117429,
            resultado_vs_pf=-226810.336837, precio_indif=4245.710119),
}

peso_entrada = peso_salida_comun  # 270, igual para todos

for i, cam in caminos.items():
    peso_venta = cam["peso_salida"]
    fecha_venta = cam["fecha_salida"]
    mortandad = cam["mortandad"]
    precio_venta_nominal = cam["precio_venta_nominal"]
    costo_alim_kg = cam["costo_alim_kg"]
    gastos_estructura = cam["gastos_estructura"]

    duracion_camino = (fecha_venta - fecha_salida_comun).days  # fila 30
    kg_producidos_camino = peso_venta - peso_entrada  # fila 31
    meses_entre = duracion_camino / 30  # fila 37 (= fila 63)

    precio_venta_deflactado = precio_venta_nominal / (1 + inflacion_mensual) ** meses_entre  # fila 38

    if cam["es_capitalizacion"]:
        # Camino 2: (kg entrada + 45% kg ganados) x precio venta
        ingreso_bruto = (peso_entrada + kg_producidos_camino * pct_dueno_capitalizacion) * precio_venta_deflactado * (1 - mortandad)
    else:
        ingreso_bruto = peso_venta * precio_venta_deflactado * (1 - mortandad)  # fila 43

    ingreso_neto = ingreso_bruto * (1 - pct_gastos_venta)  # fila 45

    costo_alimentacion = kg_producidos_camino * costo_alim_kg  # fila 48
    gastos_directos = costo_alimentacion + sanidad_monto + gastos_estructura  # fila 51

    margen_bruto = ingreso_neto - gastos_directos - costo_compra_neto  # fila 56
    capital_inmovilizado = costo_compra_neto + gastos_directos * 0.5  # fila 57
    margen_usd = margen_bruto / dolar_hoy  # fila 60

    costo_financiero = capital_inmovilizado * tasa_costo_financiero * meses_entre / 12  # fila 64
    margen_post_cf = margen_bruto - costo_financiero  # fila 65

    capital_pf = costo_compra_neto  # fila 68
    valor_pf_final = capital_pf * (1 + tasa_pf_tna) ** (meses_entre / 12)  # fila 69
    renta_pf = valor_pf_final - capital_pf  # fila 70
    resultado_vs_pf = margen_post_cf - renta_pf  # fila 71
    resultado_vs_pf_pct = (resultado_vs_pf / renta_pf) if renta_pf != 0 else None  # fila 72

    precio_indiferencia = (gastos_directos + costo_financiero + costo_compra_neto) / (peso_venta * (1 - pct_gastos_venta) * (1 - mortandad))  # fila 75
    produccion_indiferencia = (gastos_directos + costo_financiero + costo_compra_neto) / (precio_venta_deflactado * (1 - pct_gastos_venta) * (1 - mortandad)) - peso_entrada  # fila 76

    e = esperados[i]
    print(f"--- Camino {i}: {cam['nombre']} ---")
    print(f"  Margen bruto: {margen_bruto:.6f}  vs esperado {e['margen']:.6f}  -> {'OK' if abs(margen_bruto - e['margen']) < 0.01 else 'DIFF!!'}")
    print(f"  Margen USD:   {margen_usd:.6f}  vs esperado {e['margen_usd']:.6f}  -> {'OK' if abs(margen_usd - e['margen_usd']) < 0.01 else 'DIFF!!'}")
    print(f"  Margen post CF: {margen_post_cf:.6f}  vs esperado {e['margen_post_cf']:.6f}  -> {'OK' if abs(margen_post_cf - e['margen_post_cf']) < 0.01 else 'DIFF!!'}")
    print(f"  Result vs PF: {resultado_vs_pf:.6f}  vs esperado {e['resultado_vs_pf']:.6f}  -> {'OK' if abs(resultado_vs_pf - e['resultado_vs_pf']) < 0.01 else 'DIFF!!'}")
    print(f"  Precio indif: {precio_indiferencia:.6f}  vs esperado {e['precio_indif']:.6f}  -> {'OK' if abs(precio_indiferencia - e['precio_indif']) < 0.01 else 'DIFF!!'}")
    print()
