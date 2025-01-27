-****************************************************************************--
--                      SUMATIVA N° 2 : LILIANA TAPIA                         --
--****************************************************************************--

-- ----------------------------------------------------------------------------
-- 1. Declaraciones iniciales y setup
-- ----------------------------------------------------------------------------
SET SERVEROUTPUT ON;  --  Para mostrar mensajes en consola con DBMS_OUTPUT

-- Declarar variables 
VARIABLE p_fecha_proceso VARCHAR2(10);
VARIABLE p_limite_asign  NUMBER;

-- Asignar valores a las variables 
EXECUTE :p_fecha_proceso := '01/06/2021';
EXECUTE :p_limite_asign  := 250000;

--------------------------------------------------------------------------------
-- 2. Truncar tabla de errores y forzar la secuencia a empezar en 1
--  Esto asegura que la tabla ERRORES_PROCESO comience limpia y que la
--  secuencia de errores (SQ_ERRORES) se reinicie.
--------------------------------------------------------------------------------
TRUNCATE TABLE ERRORES_PROCESO;

BEGIN
   EXECUTE IMMEDIATE 'DROP SEQUENCE SQ_ERRORES';
EXCEPTION
   WHEN OTHERS THEN
      -- Si no existía la secuencia, se ignora el error y se continua
      NULL;
END;
/

CREATE SEQUENCE SQ_ERRORES
  START WITH 1
  INCREMENT BY 1
  NOCACHE
  NOCYCLE;
/

-- ----------------------------------------------------------------------------
-- 3. Bloque PL/SQL principal para el procesamiento de asignaciones
-- ----------------------------------------------------------------------------
DECLARE
   ----------------------------------------------------------------------------
   -- Declaración de un VARRAY para porcentajes de movilización extra.
   -- Cada elemento representa un porcentaje diferente aplicado según
   -- ciertos criterios de comuna y tope de sueldos.
   ----------------------------------------------------------------------------
   TYPE t_movil IS VARRAY(5) OF NUMBER;
   v_movil            t_movil := t_movil(0.02, 0.04, 0.05, 0.07, 0.09);

   ----------------------------------------------------------------------------
   -- Variables para año y mes, calculados a partir de la fecha de proceso.
   ----------------------------------------------------------------------------
   v_ano    NUMBER := TO_NUMBER(
                        TO_CHAR(
                          TO_DATE(:p_fecha_proceso, 'DD/MM/YYYY'), 
                          'YYYY'
                         )
                       );
               
   v_mes    NUMBER := TO_NUMBER(
                        TO_CHAR(
                          TO_DATE(:p_fecha_proceso, 'DD/MM/YYYY'), 
                          'MM'
                         )
                       );

   ----------------------------------------------------------------------------
   -- Cursor que obtendrá la información básica de cada profesional.
   ----------------------------------------------------------------------------
   CURSOR c_profesionales (p_ano NUMBER, p_mes NUMBER)
  IS
     SELECT p.numrun_prof,
            p.nombre,
            p.appaterno,
            p.apmaterno,
            p.cod_profesion,
            p.cod_comuna,
            p.cod_tpcontrato
       FROM profesional p
      ORDER BY (SELECT nombre_profesion 
                  FROM profesion 
                 WHERE cod_profesion = p.cod_profesion),
               p.appaterno,
               p.nombre;

   ----------------------------------------------------------------------------
   -- Cursor sin parámetros que obtendrá información adicional de profesión
   ----------------------------------------------------------------------------
   CURSOR c_profesion_info IS
     SELECT cod_profesion, nombre_profesion
       FROM profesion;
   ----------------------------------------------------------------------------
   -- Variables auxiliares para almacenar información intermedia del profesional.
   ----------------------------------------------------------------------------
   v_profe               c_profesionales%ROWTYPE; -- Variable para almacenar una fila completa del cursor c_profesionales
   v_profesion_info c_profesion_info%ROWTYPE;     -- Variable para almacenar una fila completa del cursor c_profesion_info 
   v_num_asesorias       NUMBER := 0;             -- Número de asesorías del profesional en el mes
   v_total_honorarios    NUMBER := 0;             -- Total de honorarios acumulados del profesional en el mes
   v_monto_movil_extra   NUMBER := 0;             -- Monto adicional asignado por concepto de movilización
   v_asig_tipo_cont      NUMBER := 0;             -- Monto asignado según el tipo de contrato del profesional
   v_asig_profesion      NUMBER := 0;             -- Asignación por profesión
   v_total_asignaciones  NUMBER := 0;             -- Total de todas las asignaciones calculadas para el profesional
   v_comuna_nombre       VARCHAR2(50);            -- Nombre de la comuna correspondiente al profesional

BEGIN
   ----------------------------------------------------------------------------
    -- Limpiar las tablas de detalle y de resumen antes de iniciar el proceso.
   ----------------------------------------------------------------------------
   EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_ASIGNACION_MES';
   EXECUTE IMMEDIATE 'TRUNCATE TABLE RESUMEN_MES_PROFESION';

   ----------------------------------------------------------------------------
   -- Uso del cursor sin parámetros para profesión 
   ----------------------------------------------------------------------------
   OPEN c_profesion_info;
   LOOP
      FETCH c_profesion_info INTO v_profesion_info;
      EXIT WHEN c_profesion_info%NOTFOUND;
      
      DBMS_OUTPUT.PUT_LINE(
         'Profesión: ' || v_profesion_info.nombre_profesion 
         || ' (código:' || v_profesion_info.cod_profesion || ')'
    
      );
   END LOOP;
   CLOSE c_profesion_info;

   ----------------------------------------------------------------------------
    -- Abrir el cursor y recorrer cada profesional.
   ----------------------------------------------------------------------------
   OPEN c_profesionales(v_ano, v_mes);
   LOOP
      FETCH c_profesionales INTO v_profe;
      EXIT WHEN c_profesionales%NOTFOUND;

      ----------------------------------------------------------------------------
      -- Obtener número de asesorías y total de honorarios para el profesional
      -- en el año y mes indicados.
      ----------------------------------------------------------------------------
      SELECT NVL(COUNT(*), 0),
             NVL(SUM(a.honorario), 0)
        INTO v_num_asesorias, v_total_honorarios
        FROM asesoria a
       WHERE a.numrun_prof = v_profe.numrun_prof
         AND EXTRACT(YEAR  FROM a.inicio_asesoria) = v_ano
         AND EXTRACT(MONTH FROM a.inicio_asesoria) = v_mes;

      ----------------------------------------------------------------------------
      -- Si el profesional no tiene asesorías en el periodo, se omite.
      ----------------------------------------------------------------------------
      IF v_num_asesorias = 0 THEN
         CONTINUE;
      END IF;

      ----------------------------------------------------------------------------
       -- Calcular la asignación por movilización extra según la comuna
       --  y los topes de sueldos establecidos.
      ----------------------------------------------------------------------------
      BEGIN
         SELECT c.nom_comuna
           INTO v_comuna_nombre
           FROM comuna c
          WHERE c.cod_comuna = v_profe.cod_comuna;

         IF v_comuna_nombre = 'Santiago' AND v_total_honorarios < 350000 THEN
            v_monto_movil_extra := ROUND(v_total_honorarios * v_movil(1));
         ELSIF v_comuna_nombre = 'Ñuñoa' THEN
            v_monto_movil_extra := ROUND(v_total_honorarios * v_movil(2));
         ELSIF v_comuna_nombre = 'La Reina' AND v_total_honorarios < 400000 THEN
            v_monto_movil_extra := ROUND(v_total_honorarios * v_movil(3));
         ELSIF v_comuna_nombre = 'La Florida' AND v_total_honorarios < 800000 THEN
            v_monto_movil_extra := ROUND(v_total_honorarios * v_movil(4));
         ELSIF v_comuna_nombre = 'Macul' AND v_total_honorarios < 680000 THEN
            v_monto_movil_extra := ROUND(v_total_honorarios * v_movil(5));
         ELSE
            v_monto_movil_extra := 0;
         END IF;

      EXCEPTION
         ----------------------------------------------------------------------------
        -- Si la comuna del profesional no existe en la tabla comuna,
         -- se asigna 0 en la movilización extra y se continúa.
         ----------------------------------------------------------------------------
         WHEN NO_DATA_FOUND THEN
            v_monto_movil_extra := 0;
      END;

      ----------------------------------------------------------------------------
      -- Calcular la asignación por tipo de contrato.
      -- Se basa en un porcentaje (incentivo) definido en la tabla tipo_contrato.
      ----------------------------------------------------------------------------
      BEGIN
         SELECT incentivo
           INTO v_asig_tipo_cont
           FROM tipo_contrato
          WHERE cod_tpcontrato = v_profe.cod_tpcontrato;
          
     -- El incentivo se guarda en tanto por ciento, por lo que se divide entre 100.
         v_asig_tipo_cont := ROUND(v_total_honorarios * (v_asig_tipo_cont / 100));
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            v_asig_tipo_cont := 0;
      END;

      ----------------------------------------------------------------------------
      -- Calcular la asignación por profesión.
      -- Se basa en la tabla porcentaje_profesion, que indica un porcentaje
      -- diferente por cada profesión.
      ----------------------------------------------------------------------------
      BEGIN
   SELECT asignacion
     INTO v_asig_profesion
     FROM porcentaje_profesion
    WHERE cod_profesion = v_profe.cod_profesion;

   v_asig_profesion := ROUND(v_total_honorarios * (v_asig_profesion / 100));

EXCEPTION
   WHEN NO_DATA_FOUND THEN
      v_asig_profesion := 0;
      INSERT INTO errores_proceso (
         error_id,
         mensaje_error_oracle,
         mensaje_error_usr
      ) 
      VALUES (
         SQ_ERRORES.NEXTVAL,
         'ORA-01403: No se ha encontrado ningún dato',
         'Error al obtener porcentaje de asignacion para el run Nro. ' || v_profe.numrun_prof
      );
END;
      ----------------------------------------------------------------------------
      -- Sumar las asignaciones y validar que no superen el límite máximo
      -- establecido en la variable :p_limite_asign.
      ----------------------------------------------------------------------------
      
      v_total_asignaciones := v_monto_movil_extra
                        + v_asig_tipo_cont
                        + v_asig_profesion;

     IF v_total_asignaciones > :p_limite_asign THEN
     INSERT INTO errores_proceso (
      error_id,
      mensaje_error_oracle,
      mensaje_error_usr
     ) 
     VALUES (
      SQ_ERRORES.NEXTVAL,
      'Error, profesional supera el monto límite de asignaciones. Run Nro. '
         || v_profe.numrun_prof,
      'Se reemplazó el monto total de las asignaciones calculadas de '
         || v_total_asignaciones
         || ' por el monto límite de '
         || :p_limite_asign
   );

   -- Se asigna el monto máximo permitido.
   v_total_asignaciones := :p_limite_asign;
END IF;

      ----------------------------------------------------------------------------
      -- Insertar el detalle de asignaciones en la tabla DETALLE_ASIGNACION_MES.
      ----------------------------------------------------------------------------
      INSERT INTO DETALLE_ASIGNACION_MES (
         mes_proceso,
         anno_proceso,
         numrun_prof,     
         nombre_prof,     
         profesion,
         nro_asesorias,
         monto_honorarios,
         monto_movil_extra,
         monto_asig_tipocont,
         monto_asig_profesion,
         monto_total_asignaciones
      ) 
      VALUES (
         v_mes,
         v_ano,
         v_profe.numrun_prof,
         v_profe.nombre || ' ' || v_profe.appaterno,
         (SELECT nombre_profesion 
            FROM profesion 
           WHERE cod_profesion = v_profe.cod_profesion),
         v_num_asesorias,
         v_total_honorarios,
         v_monto_movil_extra,
         v_asig_tipo_cont,
         v_asig_profesion,
         v_total_asignaciones
      );

   END LOOP; -- Fin del recorrido de profesionales

   CLOSE c_profesionales;

   ----------------------------------------------------------------------------
   -- Insertar el resumen por profesión en la tabla RESUMEN_MES_PROFESION.
   -- Se agrupan los datos de DETALLE_ASIGNACION_MES por la profesión
   --  y se hace una suma de asesorías, honorarios y asignaciones.
   ----------------------------------------------------------------------------
   INSERT INTO RESUMEN_MES_PROFESION (
       annomes_proceso,
       profesion,
       total_asesorias,
       monto_total_honorarios,
       monto_total_movil_extra,
       monto_total_asig_tipocont,
       monto_total_asig_prof,
       monto_total_asignaciones
   )
   SELECT
       -- Se forma un identificador YYYYMM (202106)
       TO_CHAR(v_ano) || LPAD(v_mes, 2, '0') AS annomes_proceso,
       p.nombre_profesion AS profesion,
       NVL(SUM(dam.nro_asesorias), 0) AS total_asesorias,
       NVL(SUM(dam.monto_honorarios), 0) AS monto_total_honorarios,
       NVL(SUM(dam.monto_movil_extra), 0) AS monto_total_movil_extra,
       NVL(SUM(dam.monto_asig_tipocont), 0) AS monto_total_asig_tipocont,
       NVL(SUM(dam.monto_asig_profesion), 0) AS monto_total_asig_prof,
       NVL(SUM(dam.monto_total_asignaciones), 0) AS monto_total_asignaciones
   FROM profesion p
   LEFT JOIN DETALLE_ASIGNACION_MES dam
          ON p.nombre_profesion = dam.profesion
         AND dam.anno_proceso   = v_ano
         AND dam.mes_proceso    = v_mes
   GROUP BY p.nombre_profesion
   -- Orden personalizado: primero ABOGADO, último INGENIERO PREVENCIONISTA,
   -- y en medio, el resto de profesiones.
   ORDER BY 
       CASE
         WHEN UPPER(TRIM(p.nombre_profesion)) = 'ABOGADO' THEN 1
         WHEN UPPER(TRIM(p.nombre_profesion)) = 'INGENIERO PREVENCIONISTA' THEN 99
         ELSE 2
       END,
       UPPER(TRIM(p.nombre_profesion));

   ----------------------------------------------------------------------------
   -- Confirmar la transacción para que todos los cambios se hagan permanentes.
   ----------------------------------------------------------------------------
   COMMIT;

EXCEPTION
   ----------------------------------------------------------------------------
   -- En caso de error inesperado, se hace un ROLLBACK y se muestra el mensaje.
   ----------------------------------------------------------------------------
   WHEN OTHERS THEN
      ROLLBACK;
      DBMS_OUTPUT.PUT_LINE('Error general: ' || SQLERRM);
END;
/

-- ----------------------------------------------------------------------------
-- Consultas de verificación
-- Estas consultas permiten verificar el contenido de las tablas una vez
-- finalizado el proceso de inserción.
-- ----------------------------------------------------------------------------
SELECT * FROM RESUMEN_MES_PROFESION ORDER BY profesion;
SELECT * FROM DETALLE_ASIGNACION_MES;
SELECT * FROM ERRORES_PROCESO;




/* Esto lo realice antes de comenzar el script, luego de poblar las tablas, para
no tener errores al momento de generar las salidas de las tablas que se pedían.
--------------------------------------------------------------------------------
Para actualizar la profesión 'Inform�tico' corrigiendo los caracteres especiales.
Se utiliza la cláusula LIKE para abarcar cualquier variación de caracteres raros.
--------------------------------------------------------------------------------
UPDATE profesion 
SET nombre_profesion = 'Ingeniero Informático' 
WHERE nombre_profesion LIKE '%Inform�tico%';

-- Se confirman los cambios realizados en la tabla.
COMMIT;

-- Verifica el resultado de la actualización en la tabla profesion.
SELECT * 
FROM profesion;

--------------------------------------------------------------------------------
-- Actualiza el nombre de algunas comunas para corregir o estandarizar caracteres.
--------------------------------------------------------------------------------
UPDATE comuna 
SET nom_comuna = 'Ñuñoa' 
WHERE cod_comuna = 83;

UPDATE comuna 
SET nom_comuna = 'Maipú' 
WHERE cod_comuna = 87;

UPDATE comuna 
SET nom_comuna = 'Peñalolén' 
WHERE cod_comuna = 91;

-- Se confirman los cambios realizados en la tabla.
COMMIT;

-- Verifica los resultados de la actualización en la tabla comuna.
SELECT * 
FROM comuna;

--------------------------------------------------------------------------------
-- Actualiza el nombre de la isapre para corregir el uso de caracteres especiales.
--------------------------------------------------------------------------------
UPDATE isapre
SET nombre_isapre = 'Banmédica'
WHERE cod_isapre = 3;

-- Se confirman los cambios realizados en la tabla.
COMMIT;

-- Verifica los resultados de la actualización en la tabla isapre.
SELECT * 
FROM isapre;
*/
