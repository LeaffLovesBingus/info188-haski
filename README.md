# Haski

Un juego de supervivencia 2D desarrollado en **Haskell** usando la librería Gloss. Proyecto para INFO188 - Paradigmas de Programación.

## Integrantes

- Eduardo Montecinos
- Jose Cristobal Silva
- Matias Soto
- Diego Soto
- Matías Toledo

## Descripcion

Haski es un juego top-down de supervivencia donde controlas a un personaje en un mundo con colisiones, items y objetos destructibles que revelan pociones mágicas. El juego implementa la monada `State` para manejar el estado del juego de forma funcional.

## Objetivo del Juego

Aparecerás en una isla frente a una tumba, tal vez recordando a un ser querido derrotado en batalla. A tu lado derecho, encontrarás tres plataformas, cada una contiene un arma diferente, una espada, una ballesta y un boomerang de batalla. Estas armas fueron forjadas por tus antepasados y fueron muy utilizadas para batallar poderosos enemigos y ganar grandes guerras.

Tendrás un corto tiempo para recoger estas armas antes de que los enemigos te alcancen, tu objetivo es sobrevivir 3 minutos sin que estos te derroten, hasta que lleguen los refuerzos.

Alrededor del mapa, podrás encontrar adornos y objetos que contienen tesoros (estos tienen un contorno dorado), destrúyelos con tus armas para poder descubrir pócimas mágicas que te pueden ayudar en tu misión de derrotar enemigos.

- **Victoria**: Sobrevive durante 3 minutos para ganar.
- **Derrota**: Si tu barra de vida llega a cero, pierdes.

## Como Jugar

- Explora el mapa recogiendo **armas** que encuentres en el suelo y pociones que encuentres dentro de **contenedores**.
- Rompe **contenedores** como cajas y vasijas para obtener pociones.
- Usa las armas para defenderte y las pociones para recuperar vida o mejorar tus estadisticas.
- Administra tu inventario sabiamente y sobrevive los 3 minutos.

## Caracteristicas

- **Mapa por capas** cargado desde Tiled (formato JSON/TMX)
- **Sistema de colisiones** con formas rectangulares y poligonales
- **Inventario** con 5 slots para items
- **Armas**: Ballesta, Boomerang y Espada
- **Items consumibles**: Curacion, Velocidad, Fuerza
- **Objetos destructibles** (cajas, vasijas) que sueltan items (están marcados con bordes dorados)
- **HUD** con barra de vida, inventario y temporizador


## Estadísticas de balance
- **PV**: Puntos de vida

| Item | Funcionalidad | Estadísticas |
|------|---------------|-------------|
| Espada | Inflinge daño en un área corta alrededor del jugador | -50 PV |
| Ballesta | Dispara un proyectil en dirección al cursor | -50 PV, 800 ms de cooldown |
| Boomerang | Sale disparado hasta alcanzar su distancia máxima o hasta chocar contra un enemigo u objeto, para devolverse a las manos del jugador, inflingiendo daño en el camino | -15 PV |
| Poción de curación | Recuperas vida al consumirla | +50 PV |
| Poción de velocidad | Aumenta tu velocidad al consumirla durante 15 segundos | +200 Puntos de velocidad
| Poción de fuerza | Aumenta tu daño de ataque al consumirla por 10 segundos | x2.0 multiplicador de daño



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


## Dependencias extras del sistema
Algunos paquetes tienen dependencias del sistema, por lo que si falla la instalación, puede ser por que falten ciertos paquetes:

Primero que nada:
```bash
sudo apt update
```

- Gloss requiere OpenGL, GLFW y FreeGlut
```bash
sudo apt install freeglut3-dev libglfw3-dev libglew-dev libxi-dev libxrandr-dev libxinerama-dev libxcursor-dev libxxf86vm-dev
```

## Recursos y Assets

Los tilesets y sprites utilizados en este proyecto fueron obtenidos de:

- [Raven Fantasy Icons](https://clockworkraven.itch.io/raven-fantasy-icons) - Assets de armas y pociones
- [Pixel Art Top Down - Basic](https://cainos.itch.io/pixel-art-top-down-basic) - Tileset y Assets usados para el diseño del mapa
- [Pixelart ruins](https://www.reddit.com/r/PixelArt/comments/fvdl43/pixelart_ruins/) - Arte para la pantalla de inicio
- [Pixel art of a winding dirt path through a sunlit forest](https://www.craiyon.com/en/image/Dhn_mOTBSSiD1F49ffZRjg) - Arte para la pantalla de victoria.
- [Graveyard](https://www.reddit.com/r/PixelArt/comments/i2x94p/graveyard/) - Arte para la pantalla de derrota.
- [Aseprite](https://www.aseprite.org) - Herramienta de dibujo utilizada para dibujar al personaje principal (diseño original).