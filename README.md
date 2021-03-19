# @CuantasVacunas
Documentación del bot de Twitter @CuantasVacunas


# Cómo funciona

El bot diariamente consulta los web services de los sitios https://covidstats.com.ar/vacunados y https://www.argentina.gob.ar/coronavirus/vacuna/aplicadas, los procesa y genera cuatro gráficos que se guardan como archivos .png en el directorio. Los datos de autenticación de Twitter están guardados como variables en el environment y se cargan a la sesion con la función  ```get_token()```.


