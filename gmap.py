################################################
import pygame
import sys, time, random
from pyswip import Prolog, Functor, Variable, Query

from threading import Thread
import pathlib
current_path = str(pathlib.Path().resolve())

from heapq import heappop, heappush
from collections import deque



auto_play_tempo = 0.05
auto_play = True # desligar para controlar manualmente
show_map = False

scale = 50
size_x = 12
size_y = 12
width = size_x * scale  #Largura Janela
height = size_y * scale #Altura Janela

player_pos = (1,1,'norte')
energia = 0
pontuacao = 0


mapa=[['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','',''],
      ['','','','','','','','','','','','']]

visitados = []
certezas = []

prolog = Prolog()
prolog.consult((current_path + '\\main.pl').replace('\\','/'))
prolog.consult((current_path + '\\mapa.pl').replace('\\','/'))


last_action = ""


def astar_neigh_safe(x_neigh, y_neigh, grid):
    return (
        # Vicino già stato visitato o con certezza
        ((x_neigh+1, y_neigh+1) in visitados or (x_neigh+1, y_neigh+1) in certezas) and
        # e non un burrone/teletrasporto/mostro
        grid[y_neigh][x_neigh] not in ['P', 'T', 'D']
    )

def astar_neigh_monster(x_neigh, y_neigh, grid):
    return (
        # Vicino già stato visitato o con certezza e non un burrone/teletrasporto/mostro
        astar_neigh_safe(x_neigh, y_neigh, grid) or
        (
            # oppure player con più di 80 di energia (per poter sopravvivere al mostro),
            energia > 80 and
            # vicino già stato visitato o con certezza
            ((x_neigh+1, y_neigh+1) in visitados or (x_neigh+1, y_neigh+1) in certezas) and
            # e non un burrone/teletrasporto (potrebbe essere un mostro)
            grid[y_neigh][x_neigh] not in ['P', 'T']
        )
    )

def astar_neigh_unsafe(x_neigh, y_neigh, grid):
    return (
        # Vicino già stato visitato o con certezza e non un burrone/teletrasporto/mostro
        # oppure player con più di 80 di energia (per poter sopravvivere al mostro), vicino già stato visitato o con certezza
        # e non un burrone/teletrasporto (potrebbe essere un mostro)
        astar_neigh_monster(x_neigh, y_neigh, grid) or
        # oppure non ancora visitato nè con certezza
        ((x_neigh+1, y_neigh+1) not in visitados and (x_neigh+1, y_neigh+1) not in certezas)
    )

# Restituisce il path come vettore ordinato di coordinate (riga, colonna), da start a end
# I parametri start e end sono in coordinate (riga, colonna)
def astar(start, end, grid, neigh=astar_neigh_safe):
    # Funzione per calcolare la distanza di Manhattan (euristica per A*)
    def heuristic(a, b):
        return abs(a[0] - b[0]) + abs(a[1] - b[1])
    
    # Movimento nelle 4 direzioni
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    open_set = []
    heappush(open_set, (0, start))
    came_from = {}
    g_score = {start: 0}
    f_score = {start: heuristic(start, end)}


    while open_set:
        current = heappop(open_set)[1]

        if current == end:
            # Ricostruzione del percorso ottimo
            path = []
            while current in came_from:
                path.append(current)
                current = came_from[current]
            #path.append(start) # Non serve in quanto l'info di start si ha in player_pos
            return path[::-1]   # Inversione lista

        for d in directions:
            neighbor = (current[0] + d[0], current[1] + d[1])
            x_neigh = neighbor[1]
            y_neigh = neighbor[0]
                    
            if (    
                    # Verifica che il vicino sia nei limiti della mappa,
                    0 <= x_neigh < size_x and
                    0 <= y_neigh < size_y and
                    neigh(x_neigh, y_neigh, grid)
            ):
                tentative_g_score = g_score[current] + 1

                if tentative_g_score < g_score.get(neighbor, float('inf')):
                    came_from[neighbor] = current
                    g_score[neighbor] = tentative_g_score
                    f_score[neighbor] = tentative_g_score + heuristic(neighbor, end)
                    heappush(open_set, (f_score[neighbor], neighbor))

    return []  # Nessun percorso trovato


def get_rotations(coordXYTargetAdjPlayer):
    xPlayer = player_pos[0]
    yPlayer = player_pos[1]
    xTarget = coordXYTargetAdjPlayer[0]
    yTarget = coordXYTargetAdjPlayer[1]

    # Calcolo direzione in cui andare
    if xTarget == xPlayer+1:
        dir = "leste"
    elif xTarget == xPlayer-1:
        dir = "oeste"
    elif yTarget == yPlayer+1:
        dir = "norte"
    else:
        dir = "sul"
    
    dirPlayer = player_pos[2]
    # Player già rivolto nella direzione in cui andare
    if dirPlayer == dir:
        return []

    if dirPlayer == "norte":
        if dir == "leste":
            return ["virar_direita"]
        elif dir == "sul":
            return ["virar_direita", "virar_direita"]
        else:   # dir == oeste
            return ["virar_esquerda"]
        
    if dirPlayer == "leste":
        if dir == "norte":
            return ["virar_esquerda"]
        elif dir == "sul":
            return ["virar_direita"]
        else:   # dir == oeste
            return ["virar_direita", "virar_direita"]
        
    if dirPlayer == "sul":
        if dir == "norte":
            return ["virar_direita", "virar_direita"]
        elif dir == "leste":
            return ["virar_esquerda"]
        else:   # dir == oeste
            return ["virar_direita"]
        
    if dirPlayer == "oeste":
        if dir == "norte":
            return ["virar_direita"]
        elif dir == "leste":
            return ["virar_direita", "virar_direita"]
        else:   # dir == sul
            return ["virar_esquerda"]

def next_movements_prolog(coordRowColTargAdjPlayer):
    # Conversione da coordinate (riga, colonna) a coordinate (x, y)
    coordXYTarget = (coordRowColTargAdjPlayer[1]+1, coordRowColTargAdjPlayer[0]+1)
    actions = get_rotations(coordXYTarget)
    actions.append("andar")
    return actions

def execute_movements_player(movementsProlog):
    for mov in movementsProlog:
        exec_prolog(mov)
        update_prolog()
        #print("Now in:", player_pos[0], player_pos[1], player_pos[2])
        time.sleep(auto_play_tempo)

def move_player_with_certeza_to(xTarget, yTarget):
    path = astar((player_pos[1]-1, player_pos[0]-1), (yTarget-1, xTarget-1), mapa)

    if (path == []):
        path = astar((player_pos[1]-1, player_pos[0]-1), (yTarget-1, xTarget-1), mapa, astar_neigh_monster)

    if (path == []):
        path = astar((player_pos[1]-1, player_pos[0]-1), (yTarget-1, xTarget-1), mapa, astar_neigh_unsafe)

    pathCoda = deque(path)

    while len(pathCoda) > 0 and player_pos[2] != "morto":
        coordRowColTarget = pathCoda.popleft()
        movements = next_movements_prolog(coordRowColTarget)
        execute_movements_player(movements)



def decisao():

    acao = ""    
    
    acoes = list(prolog.query("executa_acao(X)"))
    if len(acoes) > 0:
        acao = acoes[0]['X']

    return acao

    
class Th(Thread):

    
    def __init__ (self, mapa, alg):
        Thread.__init__(self)

    def run(self):

        time.sleep(1)
        #turno = 0

        update_prolog()
        while player_pos[2] != 'morto':
            #turno  += 1
            #print("\nTurno", turno)

            acao = decisao()
            #print(acao)

            if "esplorare" in acao:
                acaoModif = acao.replace("esplorare_", "")
                coordStartInd = acaoModif.find("(")
                coordTarget = acaoModif[coordStartInd+1: len(acaoModif)-1].split(",")
                xTarg, yTarg = int(coordTarget[0]), int(coordTarget[1])
                #print("Moving to", xTarg, yTarg, "with energy:", energia)
                
                mode = acaoModif[:coordStartInd]
                #print(mode)
                #print("Caselle certe ma non visitate:", set(certezas)-set(visitados))
                if mode == "incerteza":
                    movements = next_movements_prolog((yTarg-1, xTarg-1))
                    execute_movements_player(movements)
                else:
                    move_player_with_certeza_to(xTarg, yTarg)

            elif acao == "tornare":
                move_player_with_certeza_to(1, 1)
                print("WIN!")
                time.sleep(100)

            elif acao == "prendere":
                exec_prolog("pegar")

            elif acao == "scappare":
                exec_prolog("andar")

            if "esplorare" not in acao: # la execute_movements_player già fa la update e la sleep
                update_prolog()
                time.sleep(auto_play_tempo)


def exec_prolog(a):
    global last_action
    if a != "":
        list(prolog.query(a))
    last_action = a

def update_prolog():
    global player_pos, mapa, energia, pontuacao,visitados, show_map

    list(prolog.query("atualiza_obs, verifica_player"))

    x = Variable()
    y = Variable()
    visitado = Functor("visitado", 2)
    visitado_query = Query(visitado(x,y))
    visitados.clear()
    while visitado_query.nextSolution():
        visitados.append((x.value,y.value))
    visitado_query.closeQuery()
    
    x = Variable()
    y = Variable()
    certeza = Functor("certeza", 2)
    certeza_query = Query(certeza(x,y))
    certezas.clear()
    while certeza_query.nextSolution():
        certezas.append((x.value,y.value))
    certeza_query.closeQuery()
        
    if show_map:    
        x = Variable()
        y = Variable()
        z = Variable()    
        tile = Functor("tile", 3)
        tile_query = Query(tile(x,y,z))
        while tile_query.nextSolution():
            mapa[y.get_value()-1][x.get_value()-1] = str(z.value)
        tile_query.closeQuery()

    else:

        y = 0
        for j in mapa:
            x = 0
            for i in j:
                mapa[y][x] = ''
                x  += 1
            y +=  1

        x = Variable()
        y = Variable()
        z = Variable()    
        memory = Functor("memory", 3)
        memory_query = Query(memory(x,y,z))
        while memory_query.nextSolution():
            for s in z.value:
                
                if str(s) == 'brisa':
                    mapa[y.get_value()-1][x.get_value()-1] += 'P'
                elif str(s) == 'palmas':
                    mapa[y.get_value()-1][x.get_value()-1] += 'T'
                elif str(s) == 'passos':
                    mapa[y.get_value()-1][x.get_value()-1] += 'D'
                elif str(s) == 'reflexo':
                    mapa[y.get_value()-1][x.get_value()-1] += 'U'
                elif str(s) == 'brilho':
                    mapa[y.get_value()-1][x.get_value()-1] += 'O'
            
        memory_query.closeQuery()

    x = Variable()
    y = Variable()
    z = Variable()

    posicao = Functor("posicao", 3)
    position_query = Query(posicao(x,y,z))
    position_query.nextSolution()
    player_pos = (x.value,y.value,str(z.value))
    position_query.closeQuery()

    x = Variable()
    energia = Functor("energia", 1)
    energia_query = Query(energia(x))
    energia_query.nextSolution()
    energia = x.value
    energia_query.closeQuery()

    x = Variable()
    pontuacao = Functor("pontuacao", 1)
    pontuacao_query = Query(pontuacao(x))
    pontuacao_query.nextSolution()
    pontuacao = x.value
    pontuacao_query.closeQuery()

    #print(mapa)
    #print(player_pos)


def load():
    global sys_font, clock, img_wall, img_grass, img_start, img_finish, img_path
    global img_gold,img_health, img_pit, img_bat, img_enemy1, img_enemy2,img_floor
    global bw_img_gold,bw_img_health, bw_img_pit, bw_img_bat, bw_img_enemy1, bw_img_enemy2,bw_img_floor
    global img_player_up, img_player_down, img_player_left, img_player_right, img_tomb

    sys_font = pygame.font.Font(pygame.font.get_default_font(), 20)
    clock = pygame.time.Clock() 

    img_wall = pygame.image.load('wall.jpg')
    #img_wall2_size = (img_wall.get_width()/map_width, img_wall.get_height()/map_height)
    img_wall_size = (width/size_x, height/size_y)
    
    img_wall = pygame.transform.scale(img_wall, img_wall_size)

    
    img_player_up = pygame.image.load('player_up.png')
    img_player_up_size = (width/size_x, height/size_y)
    img_player_up = pygame.transform.scale(img_player_up, img_player_up_size)

    img_player_down = pygame.image.load('player_down.png')
    img_player_down_size = (width/size_x, height/size_y)
    img_player_down = pygame.transform.scale(img_player_down, img_player_down_size)

    img_player_left = pygame.image.load('player_left.png')
    img_player_left_size = (width/size_x, height/size_y)
    img_player_left = pygame.transform.scale(img_player_left, img_player_left_size)

    img_player_right = pygame.image.load('player_right.png')
    img_player_right_size = (width/size_x, height/size_y)
    img_player_right = pygame.transform.scale(img_player_right, img_player_right_size)


    img_tomb = pygame.image.load('tombstone.png')
    img_tomb_size = (width/size_x, height/size_y)
    img_tomb = pygame.transform.scale(img_tomb, img_tomb_size)



    img_grass = pygame.image.load('grass.jpg')
    img_grass_size = (width/size_x, height/size_y)
    img_grass = pygame.transform.scale(img_grass, img_grass_size)

    img_floor = pygame.image.load('floor.png')
    img_floor_size = (width/size_x, height/size_y)
    img_floor = pygame.transform.scale(img_floor, img_floor_size)

    img_gold = pygame.image.load('gold.png')
    img_gold_size = (width/size_x, height/size_y)
    img_gold = pygame.transform.scale(img_gold, img_gold_size)

    img_pit = pygame.image.load('pit.png')
    img_pit_size = (width/size_x, height/size_y)
    img_pit = pygame.transform.scale(img_pit, img_pit_size)

    img_enemy1 = pygame.image.load('enemy1.png')
    img_enemy1_size = (width/size_x, height/size_y)
    img_enemy1 = pygame.transform.scale(img_enemy1, img_enemy1_size)

    img_enemy2 = pygame.image.load('enemy2.png')
    img_enemy2_size = (width/size_x, height/size_y)
    img_enemy2 = pygame.transform.scale(img_enemy2, img_enemy2_size)

    img_bat = pygame.image.load('bat.png')
    img_bat_size = (width/size_x, height/size_y)
    img_bat = pygame.transform.scale(img_bat, img_bat_size)

    img_health = pygame.image.load('health.png')
    img_health_size = (width/size_x, height/size_y)
    img_health = pygame.transform.scale(img_health, img_health_size)    
    
    bw_img_floor = pygame.image.load('bw_floor.png')
    bw_img_floor_size = (width/size_x, height/size_y)
    bw_img_floor = pygame.transform.scale(bw_img_floor, bw_img_floor_size)

    bw_img_gold = pygame.image.load('bw_gold.png')
    bw_img_gold_size = (width/size_x, height/size_y)
    bw_img_gold = pygame.transform.scale(bw_img_gold, bw_img_gold_size)

    bw_img_pit = pygame.image.load('bw_pit.png')
    bw_img_pit_size = (width/size_x, height/size_y)
    bw_img_pit = pygame.transform.scale(bw_img_pit, bw_img_pit_size)

    bw_img_enemy1 = pygame.image.load('bw_enemy1.png')
    bw_img_enemy1_size = (width/size_x, height/size_y)
    bw_img_enemy1 = pygame.transform.scale(bw_img_enemy1, bw_img_enemy1_size)

    bw_img_enemy2 = pygame.image.load('bw_enemy2.png')
    bw_img_enemy2_size = (width/size_x, height/size_y)
    bw_img_enemy2 = pygame.transform.scale(bw_img_enemy2, bw_img_enemy2_size)

    bw_img_bat = pygame.image.load('bw_bat.png')
    bw_img_bat_size = (width/size_x, height/size_y)
    bw_img_bat = pygame.transform.scale(bw_img_bat, bw_img_bat_size)

    bw_img_health = pygame.image.load('bw_health.png')
    bw_img_health_size = (width/size_x, height/size_y)
    bw_img_health = pygame.transform.scale(bw_img_health, bw_img_health_size)  

def update(dt, screen):
    pass

def key_pressed():
    
    global show_map
    #leitura do teclado
    for event in pygame.event.get():
        if event.type == pygame.KEYDOWN:
    
            if event.key==pygame.K_LEFT: #tecla esquerda
                exec_prolog("virar_esquerda")
                update_prolog()

            elif event.key==pygame.K_RIGHT: #tecla direita
                exec_prolog("virar_direita")
                update_prolog()

            elif event.key==pygame.K_UP: #tecla  cima
                exec_prolog("andar")
                update_prolog()
    
            if event.key==pygame.K_m:
                show_map = not show_map
                update_prolog()

            if event.key==pygame.K_SPACE:
                exec_prolog("pegar")
                update_prolog()
            


def draw_screen(screen):

    
    screen.fill((0,0,0))
 
    y = 0
    for j in mapa:
        x = 0
        for i in j:

            if (x+1,12-y) in visitados:
                screen.blit(img_floor, (x * img_floor.get_width(), y * img_floor.get_height()))
            else:
                screen.blit(bw_img_floor, (x * bw_img_floor.get_width(), y * bw_img_floor.get_height()))

            if mapa[11-y][x].find('P') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_pit, (x * img_pit.get_width(), y * img_pit.get_height()))                            
                else:
                    screen.blit(bw_img_pit, (x * bw_img_pit.get_width(), y * bw_img_pit.get_height()))                            

            if mapa[11-y][x].find('T') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_bat, (x * img_bat.get_width(), y * img_bat.get_height()))
                else:
                    screen.blit(bw_img_bat, (x * bw_img_bat.get_width(), y * bw_img_bat.get_height()))

            if mapa[11-y][x].find('D') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_enemy1, (x * img_enemy1.get_width(), y * img_enemy1.get_height()))                                               
                else:
                    screen.blit(bw_img_enemy1, (x * bw_img_enemy1.get_width(), y * bw_img_enemy1.get_height()))                                               
                            
            if mapa[11-y][x].find('d') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_enemy2, (x * img_enemy2.get_width(), y * img_enemy2.get_height()))                                               
                else:
                    screen.blit(bw_img_enemy2, (x * bw_img_enemy2.get_width(), y * bw_img_enemy2.get_height()))                                               

            if mapa[11-y][x].find('U') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_health, (x * img_health.get_width(), y * img_health.get_height()))                               
                else:
                    screen.blit(bw_img_health, (x * bw_img_health.get_width(), y * bw_img_health.get_height()))                               

            if mapa[11-y][x].find('O') > -1:
                if (x+1,12-y) in certezas:
                    screen.blit(img_gold, (x * img_gold.get_width(), y * img_gold.get_height()))                
                else:
                    screen.blit(bw_img_gold, (x * bw_img_gold.get_width(), y * bw_img_gold.get_height()))                
            
            if x ==  player_pos[0]-1  and  y == 12 -player_pos[1] :
                if player_pos[2] == 'norte':
                    screen.blit(img_player_up, (x * img_player_up.get_width(), y * img_player_up.get_height()))                                               
                elif player_pos[2] == 'sul':
                    screen.blit(img_player_down, (x * img_player_down.get_width(), y * img_player_down.get_height()))                                               
                elif player_pos[2] == 'leste':
                    screen.blit(img_player_right, (x * img_player_right.get_width(), y * img_player_right.get_height()))                                               
                elif player_pos[2] == 'oeste':
                    screen.blit(img_player_left, (x * img_player_left.get_width(), y * img_player_left.get_height()))                                                                                                           
                else:
                    screen.blit(img_tomb, (x * img_tomb.get_width(), y * img_tomb.get_height()))                                                                                                           
            x  += 1
        y +=  1

    t = sys_font.render("Pontuação: " + str(pontuacao), False, (255,255,255))
    screen.blit(t, t.get_rect(top = height + 5, left=40))

    t = sys_font.render(last_action, False, (255,255,255))
    screen.blit(t, t.get_rect(top = height + 5, left=width/2-40))
    
    t = sys_font.render("Energia: " + str(energia), False, (255,255,255))
    screen.blit(t, t.get_rect(top = height + 5, left=width-140))

def main_loop(screen):  
    global clock
    running = True
    while running:
        for e in pygame.event.get(): 
            if e.type == pygame.QUIT:
                running = False
                break

        # Define FPS máximo
        clock.tick(60)        
 
        # Calcula tempo transcorrido desde
        # a última atualização 
        dt = clock.get_time()

        key_pressed()
        
        # Atualiza posição dos objetos da tela
        update(dt, screen)

        # Desenha objetos na tela 
        draw_screen(screen)

        # Pygame atualiza o seu estado
        pygame.display.update() 




update_prolog()



pygame.init()
pygame.display.set_caption('INF1771 Trabalho 2 - Agente Lógico')
screen = pygame.display.set_mode((width, height+30))
load()

if auto_play:
    a = Th("","")
    a.start() 

main_loop(screen)
pygame.quit()



