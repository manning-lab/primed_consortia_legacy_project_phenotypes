--postgresql CDM Primary Key Constraints for OMOP Common Data Model 5.4
ALTER TABLE cdm.5.4.person  ADD CONSTRAINT xpk_person PRIMARY KEY (person_id);
ALTER TABLE cdm.5.4.observation_period  ADD CONSTRAINT xpk_observation_period PRIMARY KEY (observation_period_id);
ALTER TABLE cdm.5.4.visit_occurrence  ADD CONSTRAINT xpk_visit_occurrence PRIMARY KEY (visit_occurrence_id);
ALTER TABLE cdm.5.4.visit_detail  ADD CONSTRAINT xpk_visit_detail PRIMARY KEY (visit_detail_id);
ALTER TABLE cdm.5.4.condition_occurrence  ADD CONSTRAINT xpk_condition_occurrence PRIMARY KEY (condition_occurrence_id);
ALTER TABLE cdm.5.4.drug_exposure  ADD CONSTRAINT xpk_drug_exposure PRIMARY KEY (drug_exposure_id);
ALTER TABLE cdm.5.4.procedure_occurrence  ADD CONSTRAINT xpk_procedure_occurrence PRIMARY KEY (procedure_occurrence_id);
ALTER TABLE cdm.5.4.device_exposure  ADD CONSTRAINT xpk_device_exposure PRIMARY KEY (device_exposure_id);
ALTER TABLE cdm.5.4.measurement  ADD CONSTRAINT xpk_measurement PRIMARY KEY (measurement_id);
ALTER TABLE cdm.5.4.observation  ADD CONSTRAINT xpk_observation PRIMARY KEY (observation_id);
ALTER TABLE cdm.5.4.note  ADD CONSTRAINT xpk_note PRIMARY KEY (note_id);
ALTER TABLE cdm.5.4.note_nlp  ADD CONSTRAINT xpk_note_nlp PRIMARY KEY (note_nlp_id);
ALTER TABLE cdm.5.4.specimen  ADD CONSTRAINT xpk_specimen PRIMARY KEY (specimen_id);
ALTER TABLE cdm.5.4.location  ADD CONSTRAINT xpk_location PRIMARY KEY (location_id);
ALTER TABLE cdm.5.4.care_site  ADD CONSTRAINT xpk_care_site PRIMARY KEY (care_site_id);
ALTER TABLE cdm.5.4.provider  ADD CONSTRAINT xpk_provider PRIMARY KEY (provider_id);
ALTER TABLE cdm.5.4.payer_plan_period  ADD CONSTRAINT xpk_payer_plan_period PRIMARY KEY (payer_plan_period_id);
ALTER TABLE cdm.5.4.cost  ADD CONSTRAINT xpk_cost PRIMARY KEY (cost_id);
ALTER TABLE cdm.5.4.drug_era  ADD CONSTRAINT xpk_drug_era PRIMARY KEY (drug_era_id);
ALTER TABLE cdm.5.4.dose_era  ADD CONSTRAINT xpk_dose_era PRIMARY KEY (dose_era_id);
ALTER TABLE cdm.5.4.condition_era  ADD CONSTRAINT xpk_condition_era PRIMARY KEY (condition_era_id);
ALTER TABLE cdm.5.4.episode  ADD CONSTRAINT xpk_episode PRIMARY KEY (episode_id);
ALTER TABLE cdm.5.4.metadata  ADD CONSTRAINT xpk_metadata PRIMARY KEY (metadata_id);
ALTER TABLE cdm.5.4.concept  ADD CONSTRAINT xpk_concept PRIMARY KEY (concept_id);
ALTER TABLE cdm.5.4.vocabulary  ADD CONSTRAINT xpk_vocabulary PRIMARY KEY (vocabulary_id);
ALTER TABLE cdm.5.4.domain  ADD CONSTRAINT xpk_domain PRIMARY KEY (domain_id);
ALTER TABLE cdm.5.4.concept_class  ADD CONSTRAINT xpk_concept_class PRIMARY KEY (concept_class_id);
ALTER TABLE cdm.5.4.relationship  ADD CONSTRAINT xpk_relationship PRIMARY KEY (relationship_id);
