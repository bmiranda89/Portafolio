from bs4 import BeautifulSoup as bs
import requests
import pandas as pd
import os
import time as t
from selenium import webdriver

archivo="datos.csv"
# Borrar el archivo si existe
if os.path.isfile(archivo):
    os.remove(archivo)
    print(f"{archivo} eliminado ya que ya existia. Se va a generar uno nuevo")
else:
    print(f"{archivo} no existe todavía.")

url_1 = "https://www.argenprop.com/casas-o-departamentos/venta/mar-del-plata"
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "es-ES,es;q=0.9",
    "Referer": "https://www.argenprop.com/",
    "DNT": "1",
    "Connection": "keep-alive",
    "Upgrade-Insecure-Requests": "1",
    "Sec-Fetch-Dest": "document",
    "Sec-Fetch-Mode": "navigate",
    "Sec-Fetch-Site": "same-origin",
    "Sec-Fetch-User": "?1"
}

# Usar Selenium para obtener la cookie del sitio

driver = webdriver.Chrome()
driver.get(url_1)

# Extraer cookies del navegador para usarlas en requests
selenium_cookies = driver.get_cookies()
session = requests.Session()
for cookie in selenium_cookies:
    session.cookies.set(cookie['name'], cookie['value'])

#Request
response = session.get(url_1,headers=headers)
soup = bs(response.text, "html.parser")

#Obtengo la cantidad de paginas de avisos que hay.
links_pagina = soup.select("li.pagination__page a")
# Extraemos solo los textos numéricos
ultima_pagina = [int(a.text) for a in links_pagina if a.text.isdigit()]
# Nos aseguramos de que haya al menos un número
if ultima_pagina:
    ultima_pagina = max(ultima_pagina)
    print(f"Última página: {ultima_pagina}")
else:
    print("No se encontraron números de página.")

#inicializo contador
#recorro todas las paginas.
for pagina in range(1,ultima_pagina):
    url = f'{url_1}?pagina-{pagina}'
    try:
        response = session.get(url, timeout=10,headers=headers)
        if response.status_code == 200:
            soup = bs(response.content, "html.parser")
        elif response.status_code == 405:
            print(f"Error 405 (posible bloqueo por CAPTCHA), abriendo con Selenium: {url}")
            driver.get(url)
            print("Esperando que resuelvas el CAPTCHA manualmente...")
            input("Presioná Enter cuando termines el CAPTCHA y la página cargue completamente.")
            selenium_cookies = driver.get_cookies()
            session.cookies.clear()  # limpiar cookies viejas de requests
            for cookie in selenium_cookies:
            # requests espera cookies con keys específicas
                session.cookies.set(cookie['name'], cookie['value'], domain=cookie.get('domain'))
            soup = bs(driver.page_source, "html.parser")

    except Exception as e:
        print(f"Error con requests: {e} - Fallback a Selenium: {url}")
        try:
            driver.get(url)
            print("Esperando que resuelvas el CAPTCHA manualmente...")
            input("Presioná Enter cuando termines el CAPTCHA y la página cargue completamente.")
            selenium_cookies = driver.get_cookies()
            session.cookies.clear()  # limpiar cookies viejas de requests
            for cookie in selenium_cookies:
            # requests espera cookies con keys específicas
                session.cookies.set(cookie['name'], cookie['value'], domain=cookie.get('domain'))
            soup = bs(driver.page_source, "html.parser")
        except Exception as ex:
            print(f"Falla total en {url} incluso con Selenium: {ex}")
            soup = None

    print(f'Arrancando pagina {pagina}')

    # Buscar todos los bloques de aviso
    try:
        tarjetas = soup.find_all("div", class_="listing__item")
    except:
        print("No encontró soup.find_all")

    #valores_main_features_no_contemplados = []
    datos = []
    for tarjeta in tarjetas:

        #OBTENGO EL ENLACE DE CADA AVISO
        enlace = tarjeta.find("a", class_="card")
        href = enlace["href"] if enlace and enlace.has_attr("href") else None
        url_aviso = f"https://www.argenprop.com{href}" if href else "No disponible"
        
        # Hago scrapping en cada aviso
        try:
            response = session.get(url_aviso, headers=headers, timeout=10)
            aviso = bs(response.text, "html.parser")
            t.sleep(0.3)
        except Exception as e:
            print(f"Error con {url_aviso}: {e}")
            continue

        #busco los datos que necesito
        direccion= aviso.find("h2", class_="titlebar__address").text.strip() if aviso.find("h2", class_="titlebar__address") else "No disponible"
        precio = aviso.find("p", class_="titlebar__price").text.strip() if aviso.find("p", class_="titlebar__price") else "No disponible"
        barrio = aviso.find("h2", class_="titlebar__title").text.strip() if aviso.find("h2", class_="titlebar__title") else "No disponible"
        expensas= aviso.find("p", class_="titlebar__expenses hide-in-mobile").text.strip() if aviso.find("p", class_="titlebar__expenses hide-in-mobile") else "No disponible"

        # Recorro los main features
        main_features = aviso.find_all("ul", class_="property-main-features")

        # Dentro de los main features busco los elementos
        valores_main_features = []
        validacion_contemplado=False

        for ul in main_features:
            lis = ul.find_all("li")
            for li in lis:
                desktop = li.find("div", class_="desktop")
                if desktop:
                    texto = desktop.find("p", class_="strong")
                    if texto:
                        valores_main_features.append(texto.get_text(strip=True))

        #print(valores_main_features)
        #recorro la lista y dependiendo si contiene alguna palabra asigno el valor a la variable
        metros_cuadrados = "No disponible"
        tipo= "No disponible"
        habitaciones = "No disponible"
        banios = "No disponible"
        ambientes = "No disponible"
        cochera= "No disponible"
        condicion = "No disponible"
        vista = "No disponible"
        antiguedad="No disponible"
        permite_mascotas="No"
        orientacion = "No disponible"
        toilettes= "No disponible"
        
        
        for i in range(len(valores_main_features)):
            #print(valores_main_features[i])
            #Si el valor de i contiene la palabra  "Cubierta" lo asigno a metros_cuadrados
            validacion_contemplado=False
            
            if "Cubierta" in valores_main_features[i]:
                metros_cuadrados = valores_main_features[i]
                validacion_contemplado=True

            elif ("Departamento" in valores_main_features[i] or "Casa" in valores_main_features[i]):
                tipo = valores_main_features[i]
                validacion_contemplado=True

            elif("dormi" in valores_main_features[i]):
                habitaciones = valores_main_features[i]
                validacion_contemplado=True
                
            elif("baño" in valores_main_features[i]):
                banios = valores_main_features[i]
                validacion_contemplado=True
            
            elif("ambiente" in valores_main_features[i]):
                ambientes = valores_main_features[i]
                validacion_contemplado=True

            elif("cochera" in valores_main_features[i]):
                cochera = valores_main_features[i]
                validacion_contemplado=True

            elif("bueno" in valores_main_features[i] or "Estrenar" in valores_main_features[i] or "Excelente" in valores_main_features[i] or "Nuevo" in valores_main_features[i] or "A estrenar" in valores_main_features[i] or "Muy bueno" in valores_main_features[i] or "Bueno" in valores_main_features[i] ):   
                condicion = valores_main_features[i]
                validacion_contemplado=True

            elif("frente" in valores_main_features[i] or "Frente" in valores_main_features[i] or "contrafrente" in valores_main_features[i] or "Contrafrente" in valores_main_features[i]):
                vista = valores_main_features[i]
                validacion_contemplado=True

            elif("este" in valores_main_features[i] or "norte" in valores_main_features[i] or "oeste" in valores_main_features[i] or "sur" in valores_main_features[i] or "Norte" in valores_main_features[i] or "Este" in valores_main_features[i] or "Oeste" in valores_main_features[i] or "Sur" in valores_main_features[i]):
                orientacion = valores_main_features[i] 
                validacion_contemplado=True   
            
            elif "años" in valores_main_features[i] and "baños" not in valores_main_features[i]:
                antiguedad = valores_main_features[i]
                validacion_contemplado=True
            
            elif("mascota" in valores_main_features[i]):
                permite_mascotas = "Si"
                validacion_contemplado=True

            elif("toilette" in valores_main_features[i]):
                toilettes = valores_main_features[i]
                validacion_contemplado=True

            #if not validacion_contemplado:
               # valores_main_features_no_contemplados.append(valores_main_features[i])

        #agrego descripcion de la publicacion
        descripcion=aviso.find("h2",class_="section-description--title")
        if(descripcion):
            descripcion=descripcion.get_text(strip=True)
        else:
            descripcion="No disponible"

        #agrego ubicacion detallada
        ubicacion=aviso.find("p",class_="location-container")
        if(ubicacion):
            ubicacion=ubicacion.get_text(strip=True)
        else:
            ubicacion="No disponible"

        #Agrego todos los ambientes del departamento en una lista

        ambientes_ul = aviso.find("ul", id="section-ambientes-depto")
        ambientes_lista = []
        if ambientes_ul:
            li_items = ambientes_ul.find_all("li", class_="property-features-item")
            for li in li_items:
                texto = li.get_text(strip=True)
                ambientes_lista.append(texto)
        
        ambientes_lista = " - ".join(ambientes_lista)
        ##################### REVISO LA LISTA Y OBTENGO LOS AMBIENTES.
        #COMPLETAR---

        #Hago lo mismo con las instalaciones de departamento

        ambientes_ul = aviso.find("ul", id="section-instalaciones-depto")
        instalaciones_dpto = []
        if ambientes_ul:
            li_items = ambientes_ul.find_all("li", class_="property-features-item")
            for li in li_items:
                texto = li.get_text(strip=True)
                instalaciones_dpto.append(texto)
        
        instalaciones_dpto = " - ".join(instalaciones_dpto)

        #hago lo mismo con servicios del departamento

        ambientes_ul = aviso.find("ul", id="section-servicios-depto")
        servicios_dpto = []
        if ambientes_ul:
            li_items = ambientes_ul.find_all("li", class_="property-features-item")
            for li in li_items:
                texto = li.get_text(strip=True)
                servicios_dpto.append(texto)
        
        servicios_dpto = " - ".join(servicios_dpto)


        #ARMO UN DICCIONARIO PARA EXTRAER LAS CARACTERISTICAS.
        caracteristicas_ul = aviso.find("ul", id="section-caracteristicas")
        caracteristicas_diccionario = {}

        if caracteristicas_ul:
            items = caracteristicas_ul.find_all("li")
            for item in items:
                clave_valor = item.get_text(separator=" ", strip=True).split(":")
                if len(clave_valor) == 2:
                    clave = clave_valor[0].strip()
                    valor_tag = item.find("strong")
                    valor = valor_tag.text.strip() if valor_tag else clave_valor[1].strip()
                    caracteristicas_diccionario[clave] = valor
        
        #Me fijo si la key en el diccionaro existe y si existe agrega la informacion en otra variable en el datos.append
        if caracteristicas_diccionario.get("Cant. Ambientes"):
            cant_ambientes_dict= caracteristicas_diccionario["Cant. Ambientes"]
        else:
            cant_ambientes_dict= "No disponible"

        if caracteristicas_diccionario.get("Cant. Dormitorios"):
            cant_dormitorios_dict= caracteristicas_diccionario["Cant. Dormitorios"]
        else:
            cant_dormitorios_dict= "No disponible"
            
        if caracteristicas_diccionario.get("Cant. Baños"):
            cant_banios_dict= caracteristicas_diccionario["Cant. Baños"]
        else:
            cant_banios_dict= "No disponible"

        if caracteristicas_diccionario.get("Estado"):
            estado_dict= caracteristicas_diccionario["Estado"]
        else:
            estado_dict= "No disponible"
        
        if caracteristicas_diccionario.get("Disposición"):
            disposicion_dict= caracteristicas_diccionario["Disposición"]
        else:
            disposicion_dict= "No disponible"
        
        if caracteristicas_diccionario.get("Expensas"):
            expensas_dict= caracteristicas_diccionario["Expensas"]
        else:
            expensas_dict= "No disponible"

        if caracteristicas_diccionario.get("Tipo Expensas"):
            tipo_expensas_dict= caracteristicas_diccionario["Tipo Expensas"]
        else:
            tipo_expensas_dict= "No disponible"
        
        if caracteristicas_diccionario.get("Cant. Toilettes"):
            toilettes_dict= caracteristicas_diccionario["Cant. Toilettes"]
        else:
            toilettes_dict= "No disponible"
        
        if caracteristicas_diccionario.get("Cant. Cocheras"):
            cocheras_dict= caracteristicas_diccionario["Cant. Cocheras"]
        else:
            cocheras_dict= "No disponible"
        
        if caracteristicas_diccionario.get("Antiguedad"):
            antiguedad_dict= caracteristicas_diccionario["Antiguedad"]
        else:
            antiguedad_dict= "No disponible"

        if caracteristicas_diccionario.get("Orientación"):
            orientacion_dict= caracteristicas_diccionario["Orientación"]
        else:
            orientacion_dict= "No disponible"

        if caracteristicas_diccionario.get("Tipo de Balcón"):
            tipo_balcon_dict= caracteristicas_diccionario["Tipo de Balcón"]
        else:
            tipo_balcon_dict= "No disponible"

        if caracteristicas_diccionario.get("Tipo de Costa"):
            tipo_costa_dict= caracteristicas_diccionario["Tipo de Costa"]
        else:
            tipo_costa_dict= "No disponible"
        
        if caracteristicas_diccionario.get("Tipo de Vista"):
            tipo_vista_dict= caracteristicas_diccionario["Tipo de Vista"]
        else:
            tipo_vista_dict= "No disponible"
        
        if caracteristicas_diccionario.get("Tipo de Piso"):
            tipo_piso_dict= caracteristicas_diccionario["Tipo de Piso"]
        else:
            tipo_piso_dict= "No disponible"

        #Lo mismo pero con datos básicos
        #ARMO UN DICCIONARIO PARA EXTRAER los datos básicos.
        caracteristicas_ul = aviso.find("ul", id="section-datos-basicos")
        caracteristicas_diccionario = {}
        
        if caracteristicas_ul:
            items = caracteristicas_ul.find_all("li")
            for item in items:
                clave_valor = item.get_text(separator=" ", strip=True).split(":")
                if len(clave_valor) == 2:
                    clave = clave_valor[0].strip()
                    valor_tag = item.find("strong")
                    valor = valor_tag.text.strip() if valor_tag else clave_valor[1].strip()
                    caracteristicas_diccionario[clave] = valor

        if caracteristicas_diccionario.get("Tipo de Unidad"):
            tipo_unidad_dict= caracteristicas_diccionario["Tipo de Unidad"]
        else:
            tipo_unidad_dict= "No disponible"

        if caracteristicas_diccionario.get("Tipo de operación"):
            tipo_operacion_dict= caracteristicas_diccionario["Tipo de operación"]
        else:
            tipo_operacion_dict= "No disponible"

        if caracteristicas_diccionario.get("Precio"):
            precio_basico_dict= caracteristicas_diccionario["Precio"]
        else:
            precio_basico_dict= "No disponible"

        #Agregar a los datos con append
        datos.append({
            "pagina":pagina,
            "url_aviso": url_aviso,
            "direccion": direccion,
            "ubicacion": ubicacion,
            "descripcion": descripcion,
            "precio": precio,
            "barrio": barrio,
            "expensas": expensas,
            "tipo": tipo,
            "metros_cuadrados": metros_cuadrados,
            "habitaciones": habitaciones,
            "banios": banios,
            "ambientes": ambientes,
            "cochera": cochera,
            "condicion": condicion,
            "vista": vista,
            "antiguedad": antiguedad,
            "permite_mascotas": permite_mascotas,
            "orientacion": orientacion,
            "toilettes": toilettes,
            "ambientes_lista": ambientes_lista,
            "cantidad_ambientes_caracteristicas": cant_ambientes_dict,
            "cantidad_dormitorios_caracteristicas": cant_dormitorios_dict,
            "cantidad_banios_caracteristicas": cant_banios_dict,
            "estado_caracteristicas":estado_dict,
            "disposicion_caracteristicas": disposicion_dict,
            "expensas_caracteristicas":expensas_dict,
            "tipo_expensas_caracteristicas":tipo_expensas_dict,
            "toilettes_caracteristicas":toilettes_dict,
            "cocheras_caracteristicas":cocheras_dict,
            "antiguedad_caracteristicas":antiguedad_dict,
            "orientacion_caracteristicas":orientacion_dict,
            "tipo_balcon_caracteristicas":tipo_balcon_dict,
            "tipo_costa_caracteristicas":tipo_costa_dict,
            "tipo_vista_caracteristicas":tipo_vista_dict,
            "tipo_piso_caracteristicas":tipo_piso_dict,
            "tipo_operacion_basico":tipo_operacion_dict,
            "tipo_unidad_basico":tipo_unidad_dict,
            "precio_basico":precio_basico_dict,
            "instalaciones_dpto":instalaciones_dpto,
            "servicios_dpto":servicios_dpto
        })

    df = pd.DataFrame(datos)
    
    #Exporto a csv
    existe = os.path.isfile(archivo)
    df.to_csv(archivo, index=False, encoding="utf-8",mode="a",header=not existe)

print("Finaliza el Scrapping")