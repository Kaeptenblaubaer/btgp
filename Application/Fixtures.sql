

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('8a3639ac-aa00-49f4-82fd-cf5b6dea0c3d', 'm.f@hamburg.de', 'sha256|17|sUxRVj5bbutZnMi4EANj0A==|Br+dKaTVmReuWCvs5xX2dQEvxussBdDJc1u6CJ8W/dI=', NULL, 0);

ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.workflows DISABLE TRIGGER ALL;

ALTER TABLE public.workflows ENABLE TRIGGER ALL;


ALTER TABLE public.histories DISABLE TRIGGER ALL;

ALTER TABLE public.histories ENABLE TRIGGER ALL;

ALTER TABLE public.adresses DISABLE TRIGGER ALL;

ALTER TABLE public.adresses ENABLE TRIGGER ALL;


ALTER TABLE public.versions DISABLE TRIGGER ALL;

ALTER TABLE public.versions ENABLE TRIGGER ALL;

ALTER TABLE public.adress_states DISABLE TRIGGER ALL;

ALTER TABLE public.adress_states ENABLE TRIGGER ALL;

ALTER TABLE public.contract_partners DISABLE TRIGGER ALL;

ALTER TABLE public.contract_partners ENABLE TRIGGER ALL;

ALTER TABLE public.contracts DISABLE TRIGGER ALL;

ALTER TABLE public.contracts ENABLE TRIGGER ALL;

ALTER TABLE public.contract_states DISABLE TRIGGER ALL;

ALTER TABLE public.contract_states ENABLE TRIGGER ALL;

ALTER TABLE public.partners DISABLE TRIGGER ALL;

ALTER TABLE public.partners ENABLE TRIGGER ALL;


ALTER TABLE public.partner_states DISABLE TRIGGER ALL;

ALTER TABLE public.partner_states ENABLE TRIGGER ALL;

ALTER TABLE public.contract_partner_states DISABLE TRIGGER ALL;

ALTER TABLE public.contract_partner_states ENABLE TRIGGER ALL;

ALTER TABLE public.contract_tariffs DISABLE TRIGGER ALL;

ALTER TABLE public.contract_tariffs ENABLE TRIGGER ALL;

ALTER TABLE public.tariffs DISABLE TRIGGER ALL;

ALTER TABLE public.tariffs ENABLE TRIGGER ALL;

ALTER TABLE public.tariff_states DISABLE TRIGGER ALL;

ALTER TABLE public.tariff_states ENABLE TRIGGER ALL;

ALTER TABLE public.contract_tariff_states DISABLE TRIGGER ALL;

ALTER TABLE public.contract_tariff_states ENABLE TRIGGER ALL;

ALTER TABLE public.partner_adresses DISABLE TRIGGER ALL;

ALTER TABLE public.partner_adresses ENABLE TRIGGER ALL;

ALTER TABLE public.partner_adress_states DISABLE TRIGGER ALL;

ALTER TABLE public.partner_adress_states ENABLE TRIGGER ALL;

ALTER TABLE public.roles DISABLE TRIGGER ALL;

INSERT INTO public.roles (id, rolename) VALUES ('c867431c-5fa3-4edb-acf3-73065d5855c9', 'admin');

ALTER TABLE public.roles ENABLE TRIGGER ALL;

ALTER TABLE public.tariff_partners DISABLE TRIGGER ALL;

ALTER TABLE public.tariff_partners ENABLE TRIGGER ALL;

ALTER TABLE public.tariff_partner_states DISABLE TRIGGER ALL;

ALTER TABLE public.tariff_partner_states ENABLE TRIGGER ALL;

ALTER TABLE public.userroles DISABLE TRIGGER ALL;

ALTER TABLE public.userroles ENABLE TRIGGER ALL;

SELECT pg_catalog.setval('public.adress_states_id_seq', 1, true);

SELECT pg_catalog.setval('public.adress_states_ref_entity_seq', 1, false);

SELECT pg_catalog.setval('public.adresses_id_seq', 1, true);

SELECT pg_catalog.setval('public.contract_partner_states_id_seq', 1, false);

SELECT pg_catalog.setval('public.contract_partner_states_ref_entity_seq', 1, false);

SELECT pg_catalog.setval('public.contract_partner_states_ref_target_seq', 1, false);

SELECT pg_catalog.setval('public.contract_partners_id_seq', 1, false);

SELECT pg_catalog.setval('public.contract_states_id_seq', 1, false);

SELECT pg_catalog.setval('public.contract_states_ref_entity_seq', 1, false);

SELECT pg_catalog.setval('public.contract_tariff_states_id_seq', 1, false);

SELECT pg_catalog.setval('public.contract_tariff_states_ref_entity_seq', 1, false);

SELECT pg_catalog.setval('public.contract_tariff_states_ref_target_seq', 1, false);

SELECT pg_catalog.setval('public.contract_tariffs_id_seq', 1, false);

SELECT pg_catalog.setval('public.contracts_id_seq', 1, false);

SELECT pg_catalog.setval('public.partner_adress_states_id_seq', 1, false);

SELECT pg_catalog.setval('public.partner_adress_states_ref_entity_seq', 1, false);

SELECT pg_catalog.setval('public.partner_adress_states_ref_target_seq', 1, false);

SELECT pg_catalog.setval('public.partner_adresses_id_seq', 1, false);

SELECT pg_catalog.setval('public.partner_states_id_seq', 1, true);

SELECT pg_catalog.setval('public.partner_states_ref_entity_seq', 1, false);

SELECT pg_catalog.setval('public.partners_id_seq', 1, true);

SELECT pg_catalog.setval('public.tariff_partner_states_id_seq', 1, false);

SELECT pg_catalog.setval('public.tariff_partner_states_ref_entity_seq', 1, false);

SELECT pg_catalog.setval('public.tariff_partner_states_ref_target_seq', 1, false);

SELECT pg_catalog.setval('public.tariff_partners_id_seq', 1, false);

SELECT pg_catalog.setval('public.tariff_states_id_seq', 1, false);

SELECT pg_catalog.setval('public.tariff_states_ref_entity_seq', 1, false);

SELECT pg_catalog.setval('public.tariffs_id_seq', 1, false);

SELECT pg_catalog.setval('public.versions_id_seq', 2, true);