# generic_utilities
Conjunto de utilidades genéricas de distintos ámbitos: spool, conversiones, etc. 

/ 

Set of generic utilities from different areas: spool, conversions, etc.

# ¿Que utilidades hay? / What utilities are there?

Las utilidades están clasificadas en carpetas para un mejor entendimiento / 
The utilities are classified in folders for a better understanding

Directorios / Directories:

* SPOOL --> Utilidades del SPOOL / Utilities for SPOOL
  * Class ZCL_CA_UTIL_SPOOL -> Clase que convierte un SPOOL a PDF u OTF / Class that converts a SPOOL to PDF or OTF
  * Ejemplo que muestra en una dynpro el contenido de una orden SPOOL en PDF / Example that shows in a dynpro the content of an SPOOL order in PDF
* ITAB --> Utilidades tabla interna dinámicas / Dynamic internal table utilities
  * Class ZCL_CA_DYNAMIC_TABLES -> Clase para crear tablas internas dinámicas / Class to create dynamic internal tables
  * Ejemplos de como crear tablas a partir de un catalogo de campos y un catalogo de campos + campos de una estructura / Examples of how to create tables from a catalog of fields and a catalog of fields + fields of a structure
* HTTP -->Utilidades en comunicaciones HTTP / Utilities for HTTP comunications
  * Class ZCL_CA_HTTP --> Contiene los siguientes métodos / Contains the next methods:
    * GET_HOST --> Devuelve servidor y puerto donde se esta ejecutando la clase / Returns server and port where the class is running
    * GENERATE_TMP_URL --> Genera una URL temporal con el contenido pasado por parámetro (fichero, imagen, etc.) / Generates a temporary URL with the content passed by parameter (file, image, etc.)
    * UNESCAPE_HTML --> Convierte código HTML escapado a texto legible. Ejemplo: &amp; pasa a & / Convert escaped HTML code to readable text. Example: & amp; pass to &
    * GET_ATTACH_HTTP --> En clases que implementan la interface IF_HTTP_EXTENSION para la captura de las llamadas en servicios SICF, permite recuperar los adjuntos que vienen en la cabecera HTTP / In classes that implement the IF_HTTP_EXTENSION interface for the capture of calls in SICF services, it allows to recover the attachments that come in the HTTP header
    * GET_HTTP_PROTOCOL --> Devuelve si la comunicación HTTP se esta haciendo por HTTP o HTTP / Returns if HTTP communication is being made through HTTP or HTTP
* ALINK --> Utilidades para Archivelink/GOS/Otros / Utilities for Archivelinki/GOS/Others   
