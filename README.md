# Haski

Un juego de aventura 2D desarrollado en **Haskell** usando la librería Gloss. Proyecto para INFO188 - Paradigmas de Programación.

## Integrantes

- Eduardo Montecinos
- Jose Cristobal Silva
- Matias Soto
- Diego Soto
- Matías Toledo

## Descripcion

Haski es un juego top-down donde controlas a un personaje en un mundo con colisiones, items y objetos destructibles. El juego implementa la monada `State` para manejar el estado del juego de forma funcional.

## Objetivo del Juego

- **Victoria**: Sobrevive durante 3 minutos para ganar.
- **Derrota**: Si tu barra de vida llega a cero, pierdes.

## Como Jugar

- Explora el mapa recogiendo **armas** y **pociones** que encuentres en el suelo.
- Rompe **contenedores** como cajas y vasijas para obtener items adicionales.
- Usa las armas para defenderte y las pociones para recuperar vida o mejorar tus estadisticas.
- Administra tu inventario sabiamente y sobrevive los 3 minutos.

## Caracteristicas

- **Mapa por capas** cargado desde Tiled (formato JSON/TMX)
- **Sistema de colisiones** con formas rectangulares y poligonales
- **Inventario** con 5 slots para items
- **Armas**: Ballesta, Boomerang y Espada
- **Items consumibles**: Curacion, Velocidad, Fuerza
- **Objetos destructibles** (cajas, vasijas) que sueltan items
- **HUD** con barra de vida, inventario y temporizador

## Controles

| Tecla | Accion |
|-------|--------|
| W/A/S/D | Mover personaje |
| Shift | Sprint |
| E | Recoger item |
| F | Soltar item |
| 1-5 | Seleccionar slot del inventario |
| Click | Atacar / Usar item |

## Ejecucion

- Instalar dependencias (requiere cabal)
```bash
make install-deps
```
- Compilar el proyecto
```bash
make
```
- Ejecutar el juego
```bash
make run
```
- Limpiar archivos compilados
```bash
make clean
```
## Estructura del Proyecto

```
info188-haski/
├── main.hs          # Punto de entrada
├── Types.hs         # Definición de tipos y constantes
├── Logic.hs         # Lógica del juego (movimiento, colisiones, input)
├── Render.hs        # Renderizado de gráficos
├── Assets.hs        # Carga de sprites y animaciones
├── MapLoader.hs     # Parser de mapas Tiled (JSON/TSX)
├── HUD/             # Módulos del HUD
│   ├── Rendering.hs
│   ├── HealthBar.hs
│   ├── Inventory.hs
│   └── ...
├── assets/          # Recursos gráficos
│   ├── pantallas/   # Imágenes de menú, victoria, derrota
│   ├── entidades/   # Sprites del jugador y entidades
│   ├── map/         # Archivos del mapa (Tiled)
│   └── ...
└── Makefile
```

## Dependencias

- GHC 9.x
- gloss
- gloss-juicy
- JuicyPixels
- aeson
- containers
- mtl
- vector
- text
- scientific

## Recursos y Assets

Los tilesets y sprites utilizados en este proyecto fueron obtenidos de:

- [Raven Fantasy Icons](https://clockworkraven.itch.io/raven-fantasy-icons) - Assets de armas y pociones
- [Pioxel Art Top Down - Basic](https://cainos.itch.io/pixel-art-top-down-basic) - Tileset y Assets usados para el diseño del mapa
- [Pixelart ruins](https://www.reddit.com/r/PixelArt/comments/fvdl43/pixelart_ruins/) - Arte para la pantalla de inicio
- [Pixel art of a winding dirt path through a sunlit forest](https://www.craiyon.com/en/image/Dhn_mOTBSSiD1F49ffZRjg) - Arte para la pantalla de victoria.
- [Graveyard](https://www.reddit.com/r/PixelArt/comments/i2x94p/graveyard/) - Arte para la pantalla de derrota.