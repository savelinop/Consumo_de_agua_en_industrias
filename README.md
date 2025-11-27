# 📘 Proyecto: Consumo de Agua en Industrias

Análisis estadístico aplicado al consumo de agua en industias.  
El proyecto utiliza técnicas descriptivas, bivariadas, inferenciales y modelos de regresión.

---

## Contenido del Proyecto
- Estadística descriptiva  
- Frecuencias de variables categóricas  
- Histogramas y boxplots  
- Matriz de correlación  
- Prueba t (Welch)  
- ANOVA de un factor  
- Prueba Chi-Cuadrado  
- Comparación de proporciones  
- Regresión lineal simple  

---

## Objetivo General
Analizar el consumo de agua industrial e identificar los factores que influyen en su variación.

---

## Objetivos Específicos
- Comparar consumo entre industrias que usan o no agua en sus procesos.  
- Evaluar diferencias entre sectores industriales.  
- Analizar la relación empleados–consumo.  
- Comparar uso de agua de pozo entre Quito y Guayaquil.  
- Determinar si la actividad industrial influye en tener planta de tratamiento.

---

## Estadística Descriptiva
Se analizaron variables numéricas:
- **volumen_consumido_m3**  
- **costo_consumo_mensual**  
- **costo_mantenimiento_tratamiento**  
- **numero_empleados**

Incluye:
- Media, mediana, moda  
- Desviación estándar, cuartiles, rango  
- Histogramas y boxplots  

También se estudiaron frecuencias de:
- Ciudad  
- Actividad industrial  
- Uso de agua en proceso  
- Tratamiento de aguas  
- Fuente de agua  

---

## Análisis Bivariado
- Matriz de correlación  
- Gráficos de dispersión  
- Boxplots segmentados  
- Tablas de contingencia  

---

## Pruebas Inferenciales

### **Prueba t – Welch**
✔ Las industrias que **usan agua en proceso** consumen significativamente más.

### **ANOVA**
✔ No existen diferencias significativas entre sectores industriales.

### **Chi-Cuadrado**
✔ La actividad industrial **no está asociada** a tener planta de tratamiento.

### **Proporciones (Quito vs Guayaquil)**
✔ No hay diferencia significativa en el uso de agua de pozo.

---

## Regresión Lineal Simple
**Variable dependiente:** volumen_consumido_m3  
**Variable independiente:** numero_empleados  

Resultados:
- Relación positiva significativa  
- Pendiente ≈ **5.86 m³** por cada empleado adicional  
- Modelo con ajuste adecuado  

---

## Conclusiones Principales
- El uso de agua en procesos productivos aumenta considerablemente el consumo.  
- El sector industrial NO determina diferencias en el nivel de consumo.  
- El número de empleados sí se relaciona con consumos mayores.  
- La mayoría de industrias tiene plantas de tratamiento, aunque no todas.  
- Quito y Guayaquil presentan comportamientos similares en uso de agua de pozo.  

---

## Tecnologías Utilizadas
- **RStudio**  
- Packages: `ggplot2`, `dplyr`, `corrplot`, `moments`, `stats`



