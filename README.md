# @CuantasVacunas
Documentación del bot de Twitter @CuantasVacunas


# Cómo funciona

El bot diariamente consulta los web services de los sitios https://covidstats.com.ar y https://www.argentina.gob.ar/coronavirus/vacuna/aplicadas, y el shapefile de Argentina por departamento de INDEC disponible en https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_dpto_con_datos.zip, los procesa y genera gráficos que se guardan como archivos .png en el directorio. Los datos de autenticación de Twitter están guardados como variables en el environment y se cargan a la sesion con la función  ```get_token()```.


