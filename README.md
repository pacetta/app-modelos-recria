# Comparador de Modelos — Recría + Engorde (Smart Farming)

App Shiny que reproduce el motor de `Modelos_Recria_Engorde_2026.xlsx`
(hojas "Parametros a actualizar" + "Modelos") en R, con todos los
parámetros editables desde el navegador.

**Verificación:** antes de escribir la app en R, reimplementé cada fórmula
en Python y comparé los 25 valores clave (margen bruto, margen USD, margen
post costo financiero, resultado vs. plazo fijo y precio de indiferencia
para los 5 caminos) contra los valores cacheados del Excel. Los 25
coincidieron exactamente. El script de verificación queda en
`verificacion_python.py` por si querés correrlo vos también o auditar la
lógica sin necesidad de tener R instalado.

**Importante:** no pude ejecutar la app en R en este entorno (el sandbox
donde trabajo no tiene R instalado ni acceso a CRAN). Verifiqué la lógica
matemática exhaustivamente en Python, pero conviene que la corras vos
localmente y me avises si algo no renderiza como se espera — puede haber
algún detalle de sintaxis de Shiny/bslib que solo aparece al ejecutar.

---

## Opción A — Correr en tu máquina (más rápido para probar)

Requisitos: tener R instalado (lo tenés, ya trabajás con R/Quarto).

```r
install.packages(c("shiny", "bslib", "ggplot2", "scales"))
shiny::runApp("app.R")
```

Se abre en el navegador. Movés los sliders del costado y todo se
recalcula en vivo — es exactamente el mismo comportamiento de cambiar
celdas en "Parametros a actualizar", pero sin abrir Excel.

---

## Opción B — Desplegar en shinyapps.io (servidor R en la nube)

Gratis hasta 25 horas activas/mes, después hay planes pagos.

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "TU_CUENTA",
                          token = "TU_TOKEN",
                          secret = "TU_SECRET")
rsconnect::deployApp("ruta/a/modelos_app")
```

Las credenciales salen de shinyapps.io → tu cuenta → Tokens.

---

## Opción C — GitHub Pages, sin límite de horas (Shinylive)

Esto es lo que preguntaste: alojar sin depender de un servidor R que se
apague o tenga cuota de horas. Shinylive compila R a WebAssembly y corre
todo en el navegador del visitante — el "servidor" es simplemente los
archivos estáticos, así que GitHub Pages alcanza.

```r
install.packages("shinylive")
shinylive::export(appdir = "ruta/a/modelos_app", destdir = "docs")
```

Esto genera una carpeta `docs/` con HTML/JS/WASM. Después:

1. Subís esa carpeta a un repo de GitHub (puede ser el mismo repo del
   proyecto, o uno nuevo solo para esto).
2. En el repo: Settings → Pages → Source → rama y carpeta `/docs`.
3. GitHub te da una URL tipo `https://tuusuario.github.io/turepo/`.

**Contras a tener en cuenta con Shinylive:**
- La primera carga tarda unos segundos más (el navegador descarga el
  runtime de R comprimido, ~unos MB).
- No todos los paquetes de R están soportados en WebAssembly todavía.
  `shiny`, `bslib`, `ggplot2` y `scales` (los que usa esta app) sí
  funcionan bien. Si más adelante querés sumar algo como `plotly` o
  paquetes con dependencias de C++ pesadas, conviene chequear antes en
  la lista de paquetes soportados de Shinylive.
- No hay backend real, así que no sirve si en el futuro quisieras que la
  app lea/escriba en una base de datos compartida (ahí sí necesitarías
  shinyapps.io o un servidor propio).

Para lo que estás pidiendo ahora — una calculadora editable, sin guardar
datos entre usuarios — Shinylive + GitHub Pages es la opción sin límite
de tiempo que buscabas, y además queda versionado en el mismo repo si
más adelante lo mantenés junto con el resto del proyecto.

---

## Qué falta antes de mostrárselo a Fermín (ver también el punteo del
Excel original)

1. **Costos de alimentación de los caminos 3, 4 y 5** están en el Excel
   marcados como "valor de referencia a completar" (sección 12). Ya son
   editables en la app, pero conviene cerrar esos números reales con la
   hoja `00 Sistemas de Engorde Novillo` antes de publicar resultados
   hacia afuera.
2. **Sensibilidad del precio real de venta**: el modelo asume que el
   precio de venta (novillo gordo, novillo exportación) solo cambia por
   inflación esperada — no incorpora escenarios de suba/baja del precio
   real, que es justamente lo que se discute semana a semana en el
   podcast. Si querés, en una segunda vuelta puedo sumar un slider de
   "% variación del precio real esperado" para estresar ese supuesto.
3. Todavía no agregué manejo de errores si algún input queda vacío o en
   cero (ej. tasa de plazo fijo = 0 rompería la división en el % de
   resultado vs. plazo fijo). Lo sumo en la próxima iteración si el MVP
   te convence.
