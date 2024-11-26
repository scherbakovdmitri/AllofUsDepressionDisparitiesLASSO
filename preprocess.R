library(tidyverse)
library(bigrquery)

# This query represents dataset "depression_SDOH" for domain "condition" and was generated for All of Us Controlled Tier Dataset v7
dataset_40020483_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                       concept_id IN (192978, 193275, 193277, 198499, 35622934, 35624743, 35624748, 3656234, 36712702, 36713698, 36717092, 37016718, 37111697, 377527, 4025677, 4038495, 4049623, 4063306, 4063307, 4065737, 4077577, 4086393, 4098302, 4103574, 4125611, 4128845, 4141454, 4148630, 4149320, 4149321, 4151170, 4152280, 4154309, 4161678, 4163851, 4176002, 4180630, 4195572, 4228802, 4239471, 4250023, 4263748, 4269493, 4282096, 4282316, 42872411, 42872722, 43021839, 4307111, 4314692, 4323418, 4327337, 432975, 4336957, 4338031, 433991, 435220, 43531624, 435520, 437942, 438406, 439093, 439254, 439390, 440161, 440383, 440795, 441534, 441641, 442069, 443247, 443929, 444114, 45757176, 762503)         
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )  
            AND (
                c_occurrence.PERSON_ID IN (SELECT
                    distinct person_id  
                FROM
                    `cb_search_person` cb_search_person  
                WHERE
                    cb_search_person.person_id IN (SELECT
                        person_id 
                    FROM
                        `cb_search_person` p 
                    WHERE
                        has_ehr_data = 1 ) 
                    AND cb_search_person.person_id IN (SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id 
                            FROM
                                `cb_criteria` c 
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id       
                                FROM
                                    `cb_criteria` cr       
                                WHERE
                                    concept_id IN (1586140, 1585879, 1586135, 1585375, 1585370, 1585892)       
                                    AND full_text LIKE '%_rank1]%'      ) a 
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                                    OR c.path LIKE CONCAT('%.', a.id) 
                                    OR c.path LIKE CONCAT(a.id, '.%') 
                                    OR c.path = a.id) 
                            WHERE
                                is_standard = 0 
                                AND is_selectable = 1) 
                            AND is_standard = 0 )) criteria ) 
                    AND cb_search_person.person_id IN (SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (concept_id IN (9201) 
                            AND is_standard = 1 )) criteria ) 
                    AND cb_search_person.person_id IN (SELECT
                        criteria.person_id 
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id 
                            FROM
                                `cb_criteria` c 
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id       
                                FROM
                                    `cb_criteria` cr       
                                WHERE
                                    concept_id IN (40192439, 40192446, 40192419, 40192398, 40192442, 40192497, 40192402, 40192491, 40192401, 40192506, 40192449, 40192388, 40192399, 40192425, 40192511, 40192452, 40192469, 40192396, 40192525, 40192471, 40192470, 40192494, 40192480, 40192501, 40192503, 40192456, 40192420, 40192475, 40192441, 40192381, 40192462, 40192445, 40192386, 40192390, 40192517, 40192528, 40192443, 40192415, 40192498, 40192496, 40192526, 40192516, 40192416, 40192426)       
                                    AND full_text LIKE '%_rank1]%'      ) a 
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                                    OR c.path LIKE CONCAT('%.', a.id) 
                                    OR c.path LIKE CONCAT(a.id, '.%') 
                                    OR c.path = a.id) 
                            WHERE
                                is_standard = 0 
                                AND is_selectable = 1) 
                            AND is_standard = 0 )) criteria ) )
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_40020483_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_40020483",
  "condition_40020483_*.csv")
message(str_glue('The data will be written to {condition_40020483_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_40020483_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_40020483_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_40020483_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_40020483_condition_df <- read_bq_export_from_workspace_bucket(condition_40020483_path)

dim(dataset_40020483_condition_df)

head(dataset_40020483_condition_df, 5)
library(tidyverse)
library(bigrquery)

# This query represents dataset "depression_SDOH" for domain "person" and was generated for All of Us Controlled Tier Dataset v7
dataset_40020483_person_sql <- paste("
    SELECT
        person.person_id,
        person.gender_concept_id,
        p_gender_concept.concept_name as gender,
        person.birth_datetime as date_of_birth,
        person.race_concept_id,
        p_race_concept.concept_name as race,
        person.ethnicity_concept_id,
        p_ethnicity_concept.concept_name as ethnicity,
        person.sex_at_birth_concept_id,
        p_sex_at_birth_concept.concept_name as sex_at_birth 
    FROM
        `person` person 
    LEFT JOIN
        `concept` p_gender_concept 
            ON person.gender_concept_id = p_gender_concept.concept_id 
    LEFT JOIN
        `concept` p_race_concept 
            ON person.race_concept_id = p_race_concept.concept_id 
    LEFT JOIN
        `concept` p_ethnicity_concept 
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id 
    LEFT JOIN
        `concept` p_sex_at_birth_concept 
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id  
    WHERE
        person.PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_ehr_data = 1 ) 
            AND cb_search_person.person_id IN (SELECT
                criteria.person_id 
            FROM
                (SELECT
                    DISTINCT person_id, entry_date, concept_id 
                FROM
                    `cb_search_all_events` 
                WHERE
                    (concept_id IN(SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (SELECT
                            CAST(cr.id as string) AS id       
                        FROM
                            `cb_criteria` cr       
                        WHERE
                            concept_id IN (1586140, 1585879, 1586135, 1585375, 1585370, 1585892)       
                            AND full_text LIKE '%_rank1]%'      ) a 
                            ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                            OR c.path LIKE CONCAT('%.', a.id) 
                            OR c.path LIKE CONCAT(a.id, '.%') 
                            OR c.path = a.id) 
                    WHERE
                        is_standard = 0 
                        AND is_selectable = 1) 
                    AND is_standard = 0 )) criteria ) 
            AND cb_search_person.person_id IN (SELECT
                criteria.person_id 
            FROM
                (SELECT
                    DISTINCT person_id, entry_date, concept_id 
                FROM
                    `cb_search_all_events` 
                WHERE
                    (concept_id IN (9201) 
                    AND is_standard = 1 )) criteria ) 
            AND cb_search_person.person_id IN (SELECT
                criteria.person_id 
            FROM
                (SELECT
                    DISTINCT person_id, entry_date, concept_id 
                FROM
                    `cb_search_all_events` 
                WHERE
                    (concept_id IN(SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (SELECT
                            CAST(cr.id as string) AS id       
                        FROM
                            `cb_criteria` cr       
                        WHERE
                            concept_id IN (40192439, 40192446, 40192419, 40192398, 40192442, 40192497, 40192402, 40192491, 40192401, 40192506, 40192449, 40192388, 40192399, 40192425, 40192511, 40192452, 40192469, 40192396, 40192525, 40192471, 40192470, 40192494, 40192480, 40192501, 40192503, 40192456, 40192420, 40192475, 40192441, 40192381, 40192462, 40192445, 40192386, 40192390, 40192517, 40192528, 40192443, 40192415, 40192498, 40192496, 40192526, 40192516, 40192416, 40192426)       
                            AND full_text LIKE '%_rank1]%'      ) a 
                            ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                            OR c.path LIKE CONCAT('%.', a.id) 
                            OR c.path LIKE CONCAT(a.id, '.%') 
                            OR c.path = a.id) 
                    WHERE
                        is_standard = 0 
                        AND is_selectable = 1) 
                    AND is_standard = 0 )) criteria ) )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_40020483_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_40020483",
  "person_40020483_*.csv")
message(str_glue('The data will be written to {person_40020483_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_40020483_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_40020483_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_40020483_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(gender = col_character(), race = col_character(), ethnicity = col_character(), sex_at_birth = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_40020483_person_df <- read_bq_export_from_workspace_bucket(person_40020483_path)

dim(dataset_40020483_person_df)

head(dataset_40020483_person_df, 5)
library(tidyverse)
library(bigrquery)

# This query represents dataset "depression_SDOH" for domain "survey" and was generated for All of Us Controlled Tier Dataset v7
dataset_40020483_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (SELECT
                DISTINCT concept_id                         
            FROM
                `cb_criteria` c                         
            JOIN
                (SELECT
                    CAST(cr.id as string) AS id                               
                FROM
                    `cb_criteria` cr                               
                WHERE
                    concept_id IN (1586134, 40192389)                               
                    AND domain_id = 'SURVEY') a 
                    ON (c.path like CONCAT('%', a.id, '.%'))                         
            WHERE
                domain_id = 'SURVEY'                         
                AND type = 'PPI'                         
                AND subtype = 'QUESTION')
        )  
        AND (
            answer.PERSON_ID IN (SELECT
                distinct person_id  
            FROM
                `cb_search_person` cb_search_person  
            WHERE
                cb_search_person.person_id IN (SELECT
                    person_id 
                FROM
                    `cb_search_person` p 
                WHERE
                    has_ehr_data = 1 ) 
                AND cb_search_person.person_id IN (SELECT
                    criteria.person_id 
                FROM
                    (SELECT
                        DISTINCT person_id, entry_date, concept_id 
                    FROM
                        `cb_search_all_events` 
                    WHERE
                        (concept_id IN(SELECT
                            DISTINCT c.concept_id 
                        FROM
                            `cb_criteria` c 
                        JOIN
                            (SELECT
                                CAST(cr.id as string) AS id       
                            FROM
                                `cb_criteria` cr       
                            WHERE
                                concept_id IN (1586140, 1585879, 1586135, 1585375, 1585370, 1585892)       
                                AND full_text LIKE '%_rank1]%'      ) a 
                                ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                                OR c.path LIKE CONCAT('%.', a.id) 
                                OR c.path LIKE CONCAT(a.id, '.%') 
                                OR c.path = a.id) 
                        WHERE
                            is_standard = 0 
                            AND is_selectable = 1) 
                        AND is_standard = 0 )) criteria ) 
                AND cb_search_person.person_id IN (SELECT
                    criteria.person_id 
                FROM
                    (SELECT
                        DISTINCT person_id, entry_date, concept_id 
                    FROM
                        `cb_search_all_events` 
                    WHERE
                        (concept_id IN (9201) 
                        AND is_standard = 1 )) criteria ) 
                AND cb_search_person.person_id IN (SELECT
                    criteria.person_id 
                FROM
                    (SELECT
                        DISTINCT person_id, entry_date, concept_id 
                    FROM
                        `cb_search_all_events` 
                    WHERE
                        (concept_id IN(SELECT
                            DISTINCT c.concept_id 
                        FROM
                            `cb_criteria` c 
                        JOIN
                            (SELECT
                                CAST(cr.id as string) AS id       
                            FROM
                                `cb_criteria` cr       
                            WHERE
                                concept_id IN (40192439, 40192446, 40192419, 40192398, 40192442, 40192497, 40192402, 40192491, 40192401, 40192506, 40192449, 40192388, 40192399, 40192425, 40192511, 40192452, 40192469, 40192396, 40192525, 40192471, 40192470, 40192494, 40192480, 40192501, 40192503, 40192456, 40192420, 40192475, 40192441, 40192381, 40192462, 40192445, 40192386, 40192390, 40192517, 40192528, 40192443, 40192415, 40192498, 40192496, 40192526, 40192516, 40192416, 40192426)       
                                AND full_text LIKE '%_rank1]%'      ) a 
                                ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                                OR c.path LIKE CONCAT('%.', a.id) 
                                OR c.path LIKE CONCAT(a.id, '.%') 
                                OR c.path = a.id) 
                        WHERE
                            is_standard = 0 
                            AND is_selectable = 1) 
                        AND is_standard = 0 )) criteria ) )
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_40020483_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_40020483",
  "survey_40020483_*.csv")
message(str_glue('The data will be written to {survey_40020483_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_40020483_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_40020483_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_40020483_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_40020483_survey_df <- read_bq_export_from_workspace_bucket(survey_40020483_path)

dim(dataset_40020483_survey_df)

head(dataset_40020483_survey_df, 5)
library(tidyverse)
library(bigrquery)

# This query represents dataset "depression_SDOH" for domain "zip_code_socioeconomic" and was generated for All of Us Controlled Tier Dataset v7
dataset_40020483_zip_code_socioeconomic_sql <- paste("
    SELECT
        observation.person_id,
        observation.observation_datetime,
        zip_code.zip3_as_string as zip_code,
        zip_code.fraction_assisted_income as assisted_income,
        zip_code.fraction_high_school_edu as high_school_education,
        zip_code.median_income,
        zip_code.fraction_no_health_ins as no_health_insurance,
        zip_code.fraction_poverty as poverty,
        zip_code.fraction_vacant_housing as vacant_housing,
        zip_code.deprivation_index,
        zip_code.acs as american_community_survey_year 
    FROM
        `zip3_ses_map` zip_code 
    JOIN
        `observation` observation 
            ON CAST(SUBSTR(observation.value_as_string, 0, STRPOS(observation.value_as_string, '*') - 1) AS INT64) = zip_code.zip3  
    WHERE
        observation.PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_ehr_data = 1 ) 
            AND cb_search_person.person_id IN (SELECT
                criteria.person_id 
            FROM
                (SELECT
                    DISTINCT person_id, entry_date, concept_id 
                FROM
                    `cb_search_all_events` 
                WHERE
                    (concept_id IN(SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (SELECT
                            CAST(cr.id as string) AS id       
                        FROM
                            `cb_criteria` cr       
                        WHERE
                            concept_id IN (1586140, 1585879, 1586135, 1585375, 1585370, 1585892)       
                            AND full_text LIKE '%_rank1]%'      ) a 
                            ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                            OR c.path LIKE CONCAT('%.', a.id) 
                            OR c.path LIKE CONCAT(a.id, '.%') 
                            OR c.path = a.id) 
                    WHERE
                        is_standard = 0 
                        AND is_selectable = 1) 
                    AND is_standard = 0 )) criteria ) 
            AND cb_search_person.person_id IN (SELECT
                criteria.person_id 
            FROM
                (SELECT
                    DISTINCT person_id, entry_date, concept_id 
                FROM
                    `cb_search_all_events` 
                WHERE
                    (concept_id IN (9201) 
                    AND is_standard = 1 )) criteria ) 
            AND cb_search_person.person_id IN (SELECT
                criteria.person_id 
            FROM
                (SELECT
                    DISTINCT person_id, entry_date, concept_id 
                FROM
                    `cb_search_all_events` 
                WHERE
                    (concept_id IN(SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (SELECT
                            CAST(cr.id as string) AS id       
                        FROM
                            `cb_criteria` cr       
                        WHERE
                            concept_id IN (40192439, 40192446, 40192419, 40192398, 40192442, 40192497, 40192402, 40192491, 40192401, 40192506, 40192449, 40192388, 40192399, 40192425, 40192511, 40192452, 40192469, 40192396, 40192525, 40192471, 40192470, 40192494, 40192480, 40192501, 40192503, 40192456, 40192420, 40192475, 40192441, 40192381, 40192462, 40192445, 40192386, 40192390, 40192517, 40192528, 40192443, 40192415, 40192498, 40192496, 40192526, 40192516, 40192416, 40192426)       
                            AND full_text LIKE '%_rank1]%'      ) a 
                            ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                            OR c.path LIKE CONCAT('%.', a.id) 
                            OR c.path LIKE CONCAT(a.id, '.%') 
                            OR c.path = a.id) 
                    WHERE
                        is_standard = 0 
                        AND is_selectable = 1) 
                    AND is_standard = 0 )) criteria ) ) 
        AND observation_source_concept_id = 1585250 
        AND observation.value_as_string NOT LIKE 'Res%'", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
zip_code_socioeconomic_40020483_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "zip_code_socioeconomic_40020483",
  "zip_code_socioeconomic_40020483_*.csv")
message(str_glue('The data will be written to {zip_code_socioeconomic_40020483_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_40020483_zip_code_socioeconomic_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  zip_code_socioeconomic_40020483_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {zip_code_socioeconomic_40020483_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(zip3_as_string = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_40020483_zip_code_socioeconomic_df <- read_bq_export_from_workspace_bucket(zip_code_socioeconomic_40020483_path)

dim(dataset_40020483_zip_code_socioeconomic_df)

head(dataset_40020483_zip_code_socioeconomic_df, 5)
z=dataset_40020483_zip_code_socioeconomic_df
p=dataset_40020483_person_df
c=dataset_40020483_condition_df
s=dataset_40020483_survey_df

s.2=
  s%>% arrange(person_id) %>% select(-matches("survey|question_concept_id|answer_concept_id")) %>% group_by(person_id) %>% 
  pivot_wider(names_from=question,values_from = answer,values_fn=function(x) x[length(x)]) 

c$standard_concept_name %>% table %>% sort
s %>% filter(survey=='Social Determinants of Health') %>% pull(survey_datetime) %>% as.Date %>%  summary
### sdoh survey started in Nov 2021, but we can consider ~Jan 2020 as cutoff 
# c.2=
#   c %>% 
#   filter(as.Date(condition_start_datetime)>="2020-01-01") %>% ### depressed since 2020
#   mutate(
#     depression=ifelse(str_detect(tolower(standard_concept_name),"depres|dysthym"),1,0),
#     delivery=ifelse(str_detect(tolower(standard_concept_name),"deliver|pregnan|born|birth|labor|fetal|postpartum|perineal|uterine inertia|twin|triplet"),1,0)
#   ) 

c.2=
  c %>%
#  filter(as.Date(condition_start_datetime)>="2020-01-01") %>% ### depressed since 2020
  mutate(condition=NA,
    condition=ifelse(str_detect(tolower(standard_concept_name),"depres|dysthym|seasonal|dysphoric disorder"),"depression",NA),
    condition=ifelse(is.na(condition),"delivery",'depression')
  ) %>% 
  pivot_wider(id_cols=c(person_id),names_from=condition,values_from=condition_start_datetime,values_fn=function(x) sort(x)[length(x)] ) %>% 
  mutate(across(c(delivery,depression),lubridate::as_date)) %>% 
  mutate(delivery.2=ifelse(!is.na(delivery)&!is.na(depression)&depression<delivery,0,ifelse(!is.na(delivery),ifelse(delivery>="2020-01-01",1,0),0))) %>% 
  mutate(depression.2=ifelse(!is.na(depression),ifelse(depression>="2020-01-01",1,0),0)) 
  

c.2$condition_start_datetime %>% as.Date %>% summary
depressed=c.2 %>% filter(depression.2==1) %>% pull(person_id) %>% unique
delivered=c.2 %>% filter(delivery.2==1) %>% pull(person_id) %>% unique
c$standard_concept_name %>% table

excluded=c %>% 
  filter(as.Date(condition_start_datetime)<"2020-01-01") %>% pull(person_id) %>% unique
#excluded=NULL
s.3=s.2 %>% 
  filter(!person_id %in% setdiff(excluded,union(depressed,delivered))) %>% 
  mutate(depression=ifelse(person_id %in% depressed,"Yes","No"),delivered=ifelse(person_id %in% delivered,"Yes","No"))
s.3$person_id %>% n_distinct

s %>% View

acs=read.csv(colClasses='character','https://raw.githubusercontent.com/scherbakovdmitri/MIMIC-III-MaritalStatus/main/children.csv')
rel=read.csv(colClasses='character','https://raw.githubusercontent.com/scherbakovdmitri/MIMIC-III-MaritalStatus/main/relig.csv') %>% 
  select(ZIP3,TOTRATE_2020) %>% mutate(TOTRATE_2020=as.double(TOTRATE_2020))

#rel=read.csv('religion_county.csv')

comb=
s.3 %>% ungroup %>% 
  left_join(p) %>% 
  left_join(z %>% mutate(ZIP3=gsub("\\*","",zip_code))) %>% 
  left_join(acs) %>% 
  left_join(rel) %>% 
  select(-person_id) 
  

comb.2=comb %>% 
  select(names(comb)[which(colSums(is.na(comb))<100)]) %>% 
  select(-contains('_id'),-X,-ZIP3,-american_community_survey_year,-matches("GEOID|moe")) %>% 
  drop_na 


del_noanswer=function(x)
{
  lvl=levels(x)
  if (any(str_detect(lvl,'PMI: Prefer Not To Answer')))
    new=(fct_recode(x,
                    #NULL="PMI: Skip",
                    NULL="PMI: Prefer Not To Answer"
                    #NULL="PMI: Skip",
                    #NULL="PMI: Dont Know"
                    
    ))
  else new=x
  return(new)
} 

group_factors=function(x)
{
  lvl=levels(x)
  if (any(str_detect(lvl,'Most of the time|Almost everyday|Sometimes')))
    new=(fct_recode(x,"None/Little"="A little of the time",
                       "None/Little"="None of the time",
                       "None/Little"="Never",
                       "None/Little"="Rarely",
                       "Often"="Most of the time",
                       "Often"="All of the time",
                       "Often"="Always",
                       "Often"="Sometimes",
                       "None/Little"="Never",
                       "None/Little"="Less than once a year",
                       'None/Little'='Rarely',
                       "Often"="Almost everyday",
                       "Often"="At least once a week",
                       "Often"="A few times a month",
                       "Skip/Dont know"="PMI: Skip",
                       "Skip/Dont know"="PMI: Dont Know"
                       #NULL="PMI: Skip",
                       #NULL="PMI: Dont Know"
                       
    ))
  else if (any(str_detect(tolower(lvl),'agree')))
    new=(fct_recode(x,"Agree"="Agree",
                       "Agree"="Strongly agree",
                       "Agree"="Somewhat agree",
                       "Disagree"="Strongly disagree",
                       "Disagree"="Disagree",
                       "Disagree"="Somewhat disagree",
                        "Skip/Dont know"="PMI: Skip",
                        "Skip/Dont know"="PMI: Dont Know"
                       #NULL="PMI: Skip",
                       #NULL="PMI: Dont Know"
    ))
  else if (any(str_detect(lvl,'I am not religious')))
    new=(fct_recode(x,
                       "None/Little"="Never or almost never",
                       "None/Little"="Rarely",
                       "None/Little"='Less than once per month',
                       "Often"="Many times a day",
                       "Often"="Most days",
                       "Often"="Every day",
                       "Often"="1 to 3 times per month",
                       "Some days"="Once in a while",
                       "Often"='More than once a week',
                       'Often'='Once a week',
                       "Skip/Dont know"="PMI: Skip",
                       "Skip/Dont know"="PMI: Dont Know"
                       #NULL="PMI: Skip",
                       #NULL="PMI: Dont Know"
                       
    ))
  else if (any(str_detect(lvl,'Gender Identity: Additional Options')))
    new=(fct_recode(x,
                       "Non-binary"="Gender Identity: Additional Options",
                       "Non-binary"="Gender Identity: Non Binary",
                       "Non-binary"='PMI: Prefer Not To Answer',
                       "Non-binary"="Gender Identity: Transgender",
                       NULL="PMI: Skip",
                       NULL="PMI: Dont Know"
    ))
  else if (any(str_detect(lvl,'What Race Ethnicity: Black')))
    new=(fct_recode(x,
                       #"Other"="What Race Ethnicity: MENA",
                       #"Other"="What Race Ethnicity: Asian",
                       #"Other"='What Race Ethnicity: NHPI',
                       "Other"="What Race Ethnicity: Race Ethnicity None Of These",
                       NULL="PMI: Prefer Not To Answer",
                       NULL="PMI: Skip",
                       NULL="PMI: Dont Know"
    ))
  else if (any(str_detect(lvl,'Sexual Orientation: Bisexual')))
    new=(fct_recode(x,
                    "Non-heterosexual"="Sexual Orientation: Bisexual",
                    "PMI: Skip"="PMI: Prefer Not To Answer",
                    "Non-heterosexual"='Sexual Orientation: Gay',
                    "Non-heterosexual"="Sexual Orientation: Lesbian",
                    "Non-heterosexual"="Sexual Orientation: None",
    ))
  else if (any(str_detect(lvl,'Highest Grade: Advanced Degree')))
    new=(fct_recode(x,
                    "Unfinished school"="Highest Grade: One Through Four",
                    "Unfinished school"="Highest Grade: Five Through Eight",
                    "Unfinished school"='Highest Grade: Nine Through Eleven',
                    "Unfinished school"="Never Attended"
    ))
   else if (any(str_detect(lvl,'Birthplace: USA')))
    new=(fct_recode(x,
                       "Outside USA"="PMI: Other",
                       NULL="PMI: Skip"
                       #NULL="PMI: Dont Know"
    ))
   else new=x
   levels(new)=gsub(".*?: ","",levels(new))
   toplevel=(table(new) %>% sort(d=T))[1] %>% names
   swap_index <- match("Skip/Dont know", levels(new))
   apple_index <- match(toplevel, levels(new))
   levels(new)[swap_index] <- levels(new)[apple_index]
   swap_index <- match("Skip", levels(new))
   apple_index <- match(toplevel, levels(new))
   levels(new)[swap_index] <- levels(new)[apple_index]
   new[which(is.na(new))]=toplevel
   return(new)
} 

comb.2.2=
  comb.2 %>% 
  select(-zip_code,-observation_datetime,-gender,-race,-ethnicity,-sex_at_birth,-assisted_income,-high_school_education,#-median_income,
         -no_health_insurance,-poverty,-vacant_housing,-`Health Insurance: Health Insurance Type`#-contains('deprivation')
         ) %>% 
  select(!(contains('How often do you feel')|contains('how often have you felt')|contains('In the last month')|contains('How often do you have someone to prepare your meals')|contains('In your day-to-day life, how often are you treated with less courtesy than other people')|
             contains('Think about the place you live')|
             contains('Within the past 12 months, were you worried whether the food you had bought just')|contains('How often do you have someone to turn to for suggestions about how to deal with a personal problem')|
             contains('How often do you find strength and comfort in your religion')|contains('How often do you have someone to help you if you were confined')|
             contains('Estimate..Total...Women.who.did.not.have.a.birth.in.the.past.12.months...Has.a.spouse.present')
             )) %>%
  select(!(contains('neighborhood'))) %>% 
  #filter(sex_at_birth!="Intersex") %>% 
  #select(depression,matches("child|kid|relig|attend|income|race|ethnic|sex"),Estimate..Total...Women.who.had.a.birth.in.the.past.12.months.) %>% 
  #select(-race) %>% 
  #select(-contains('Women.who.did.not.have')) %>% 
  mutate(across(contains("Estimate"),as.numeric)) %>% 
  #mutate_if(is.character,~ifelse(.x=="PMI: Skip",NA,.x)) %>% 
  mutate(`Living Situation: People Under 18`=fct_recode(`Living Situation: People Under 18`,"0"="Response removed due to invalid value",'1 or 2'='1','1 or 2'='2','3 or more'='3','3 or more'='4','3 or more'='5','3 or more'='6 or more household members under the age of 18')) %>% 
  mutate(across(c(
                `In the last 12 months, how many times have you or your family moved from one home to another? Number of moves in past 12 months:`),
                as.integer)
           ) %>% 
  select(!contains('Estimate..Total...Women.who.did.not.have.a.birth.in.the.past.12.months.')) %>% 
  select(-`Estimate..Total.`) %>% 
  select(!matches("how often|how many|how much|would you say that"),matches("religious|respect|how many people")) %>% 
  #mutate_if(is.integer,~ifelse(is.na(.x),"Missing",.x)) %>%
  mutate_if(is.character,as.factor) %>% #%>% pull(`Living Situation: How Many People`) %>% str
    #pull(`Do you speak a language other than English at home?`) %>% table
  filter(!`Education Level: Highest Grade` %in% c("PMI: Prefer Not To Answer","Never Attended")) %>% 
  filter(!`Employment: Employment Status` %in% c("PMI: Prefer Not To Answer","PMI: Skip")) %>% 
  filter(!`Do you speak a language other than English at home?` %in% c("PMI: Prefer Not To Answer","PMI: Skip")) %>%
  mutate_if(is.factor,del_noanswer) %>% ggplot2::remove_missing() %>% 
  mutate_if(is.factor,group_factors) %>% 
  mutate(TOTRATE_2020=case_when(TOTRATE_2020<(49.6252-8.080272)~factor("Low"),
                                TOTRATE_2020>(49.6252+8.080272)~factor("High"),
                                T~factor("Average")
                                )) %>% 
  rename(`Rate of religious adherence (county-level)`=TOTRATE_2020)  %>% 
  rename("Recent childbirth"=delivered) %>% 
  rename("Median income in the area"=median_income) %>% 
  rename("Area deprivation index"=deprivation_index) %>% 
  # mutate(across(`Median income in the area`,~factor(case_when(.x<mean(.x)-sd(.x)~"Low",
  #                                                        .x>mean(.x)+sd(.x)~"High",
  #                                                        .default="Average"
  # ))))
  #mutate(across(c(`Median income in the area`,`Area deprivation index`),scale,center=T)) %>% 
  mutate(across(matches("Median income in the area|deprivation|Estimate"),~factor(case_when(.x<mean(.x)-sd(.x)~"Low",
                                                                                .x>mean(.x)+sd(.x)~"High",
                                                                                .default="Average"
  )))
  ) %>% 
  mutate(age=as.integer(Sys.Date()-as.Date(date_of_birth))/365) %>% select(-date_of_birth)  %>% 
  mutate(age=case_when(age<40~"Younger adults",
                       age<70~"Adults",
                       .default="Older adults"
                       )) %>% mutate(age=factor(age))  %>% 
  mutate(people=as.integer(str_extract(`Living Situation: How Many People`,"\\d+"))) %>% select(-`Living Situation: How Many People`) %>% 
  mutate(income=as.integer(str_extract(`Income: Annual Income`,"\\d+"))) %>%  select(-`Income: Annual Income`) %>% 
  mutate(across(where(is.numeric), ~ replace(., is.na(.), median(., na.rm = TRUE)))) %>% 
  mutate(incomecapitaK=income/(people+1)) %>% select(-income) %>% 
  mutate(incomecapitaK=factor(case_when(incomecapitaK<17~"Low",
                                        incomecapitaK<75~"Medium",
                                        incomecapitaK<=200~"High"
  ))) %>% 
  mutate(household=factor(case_when(people==0~"Single person household",
                                 people==1~"Two person household ",
                                 people>=2~"Three and more person household"
  ))) %>% select(-people) %>% 
  mutate(`Race: What Race Ethnicity`=fct_relevel(`Race: What Race Ethnicity`,"White")) %>%
  mutate(`Marital Status: Current Marital Status`=fct_relevel(`Marital Status: Current Marital Status`,"Married")) %>%
  mutate(`Gender: Gender Identity`=fct_relevel(`Gender: Gender Identity`,"Man")) %>% 
  mutate(`Employment: Employment Status`=fct_relevel(`Employment: Employment Status`,"Employed For Wages")) %>% 
  mutate(`How often are you treated with less respect than other people when you go to a doctor's office or other health care provider?`=
           fct_relevel(`How often are you treated with less respect than other people when you go to a doctor's office or other health care provider?`
           ,"None/Little")) %>%
  mutate(`In your day-to-day life, how often are you treated with less respect than other people?`=
           fct_relevel(`In your day-to-day life, how often are you treated with less respect than other people?`
                       ,"None/Little")) %>%
  mutate(`Rate of religious adherence (county-level)`=fct_relevel(`Rate of religious adherence (county-level)`,"Average")) %>% 
  mutate(`household`=fct_relevel(`household`,"Two person household ")) %>% 
  mutate(`How often do you go to religious meetings or services?`=fct_relevel(`How often do you go to religious meetings or services?`,"None/Little")) %>% 
  mutate(`Education Level: Highest Grade`=fct_relevel(`Education Level: Highest Grade`,"College Graduate")) %>% 
  mutate(`Employment: Employment Status`=fct_relevel(`Employment: Employment Status`,"Employed For Wages")) %>% 
  mutate(`Home Own: Current Home Own`=fct_relevel(`Home Own: Current Home Own`,"Own"))
  
  
  
  # mutate(`Race: What Race Ethnicity`=fct_relevel(`Race: What Race Ethnicity`,"Other")) %>%
  # mutate(`Marital Status: Current Marital Status`=fct_relevel(`Marital Status: Current Marital Status`,"Separated")) %>%
  # mutate(`Gender: Gender Identity`=fct_relevel(`Gender: Gender Identity`,"Non-binary")) %>% 
  # mutate(`The Basics: Birthplace`=fct_relevel(`The Basics: Birthplace`,"Outside USA")) %>% 
  # mutate(`The Basics: Sexual Orientation`=fct_relevel(`The Basics: Sexual Orientation`,"Non-heterosexual")) %>% 
  # mutate(`age`=fct_relevel(`age`,"Younger adults")) 

