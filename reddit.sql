--
-- PostgreSQL database dump
--

-- Dumped from database version 14.1
-- Dumped by pg_dump version 14.1

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

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: comment; Type: TABLE; Schema: public; Owner: cmoore
--

CREATE TABLE public.comment (
    id text NOT NULL,
    parent text,
    author text NOT NULL,
    link_id text NOT NULL,
    flair_text text,
    score integer NOT NULL,
    body_html text NOT NULL,
    bulk text
);


ALTER TABLE public.comment OWNER TO cmoore;

--
-- Name: comment_votes; Type: TABLE; Schema: public; Owner: cmoore
--

CREATE TABLE public.comment_votes (
    id text NOT NULL,
    scores integer[] NOT NULL
);


ALTER TABLE public.comment_votes OWNER TO cmoore;

--
-- Name: link; Type: TABLE; Schema: public; Owner: cmoore
--

CREATE TABLE public.link (
    id text NOT NULL,
    bulk text NOT NULL,
    favorite boolean NOT NULL,
    hidden boolean NOT NULL,
    written numeric NOT NULL,
    created_utc numeric NOT NULL,
    url text NOT NULL,
    permalink text NOT NULL,
    author text NOT NULL,
    subreddit_id text NOT NULL,
    media_only boolean NOT NULL,
    title text NOT NULL,
    author_fullname text NOT NULL,
    body text,
    selftext text NOT NULL,
    subreddit text NOT NULL,
    nsfw boolean NOT NULL,
    shadow boolean DEFAULT false NOT NULL,
    store text,
    remotefile text,
    ts_index tsvector GENERATED ALWAYS AS (to_tsvector('english'::regconfig, ((((COALESCE(title, ''::text) || ' '::text) || COALESCE(body, ''::text)) || ' '::text) || COALESCE(selftext, ''::text)))) STORED
);


ALTER TABLE public.link OWNER TO cmoore;

--
-- Name: media; Type: TABLE; Schema: public; Owner: cmoore
--

CREATE TABLE public.media (
    id integer NOT NULL,
    link_id text NOT NULL,
    url text NOT NULL,
    hash text NOT NULL
);


ALTER TABLE public.media OWNER TO cmoore;

--
-- Name: media_id_seq; Type: SEQUENCE; Schema: public; Owner: cmoore
--

CREATE SEQUENCE public.media_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.media_id_seq OWNER TO cmoore;

--
-- Name: media_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: cmoore
--

ALTER SEQUENCE public.media_id_seq OWNED BY public.media.id;


--
-- Name: seen; Type: TABLE; Schema: public; Owner: cmoore
--

CREATE TABLE public.seen (
    id text NOT NULL
);


ALTER TABLE public.seen OWNER TO cmoore;

--
-- Name: subreddit; Type: TABLE; Schema: public; Owner: cmoore
--

CREATE TABLE public.subreddit (
    id text NOT NULL,
    url text NOT NULL,
    display_name text NOT NULL
);


ALTER TABLE public.subreddit OWNER TO cmoore;

--
-- Name: up_history; Type: TABLE; Schema: public; Owner: cmoore
--

CREATE TABLE public.up_history (
    id text NOT NULL,
    scores integer[] NOT NULL
);


ALTER TABLE public.up_history OWNER TO cmoore;

--
-- Name: media id; Type: DEFAULT; Schema: public; Owner: cmoore
--

ALTER TABLE ONLY public.media ALTER COLUMN id SET DEFAULT nextval('public.media_id_seq'::regclass);


--
-- Name: comment comment_pkey; Type: CONSTRAINT; Schema: public; Owner: cmoore
--

ALTER TABLE ONLY public.comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (id);


--
-- Name: comment_votes comment_votes_pkey; Type: CONSTRAINT; Schema: public; Owner: cmoore
--

ALTER TABLE ONLY public.comment_votes
    ADD CONSTRAINT comment_votes_pkey PRIMARY KEY (id);


--
-- Name: link link_pkey; Type: CONSTRAINT; Schema: public; Owner: cmoore
--

ALTER TABLE ONLY public.link
    ADD CONSTRAINT link_pkey PRIMARY KEY (id);


--
-- Name: media media_pkey; Type: CONSTRAINT; Schema: public; Owner: cmoore
--

ALTER TABLE ONLY public.media
    ADD CONSTRAINT media_pkey PRIMARY KEY (id);


--
-- Name: seen seen_pkey; Type: CONSTRAINT; Schema: public; Owner: cmoore
--

ALTER TABLE ONLY public.seen
    ADD CONSTRAINT seen_pkey PRIMARY KEY (id);


--
-- Name: subreddit subreddit_pkey; Type: CONSTRAINT; Schema: public; Owner: cmoore
--

ALTER TABLE ONLY public.subreddit
    ADD CONSTRAINT subreddit_pkey PRIMARY KEY (id);


--
-- Name: up_history up_history_pkey; Type: CONSTRAINT; Schema: public; Owner: cmoore
--

ALTER TABLE ONLY public.up_history
    ADD CONSTRAINT up_history_pkey PRIMARY KEY (id);


--
-- Name: comment_id_index; Type: INDEX; Schema: public; Owner: cmoore
--

CREATE UNIQUE INDEX comment_id_index ON public.comment USING btree (id);


--
-- Name: comment_votes_id_index; Type: INDEX; Schema: public; Owner: cmoore
--

CREATE UNIQUE INDEX comment_votes_id_index ON public.comment_votes USING btree (id);


--
-- Name: link_id_index; Type: INDEX; Schema: public; Owner: cmoore
--

CREATE UNIQUE INDEX link_id_index ON public.link USING btree (id);


--
-- Name: media_link_id_url; Type: INDEX; Schema: public; Owner: cmoore
--

CREATE UNIQUE INDEX media_link_id_url ON public.media USING btree (link_id, url);


--
-- Name: media_url_index; Type: INDEX; Schema: public; Owner: cmoore
--

CREATE UNIQUE INDEX media_url_index ON public.media USING btree (url);


--
-- Name: seen_id_index; Type: INDEX; Schema: public; Owner: cmoore
--

CREATE UNIQUE INDEX seen_id_index ON public.seen USING btree (id);


--
-- Name: subreddit_id_index; Type: INDEX; Schema: public; Owner: cmoore
--

CREATE UNIQUE INDEX subreddit_id_index ON public.subreddit USING btree (id);


--
-- Name: up_history_id_index; Type: INDEX; Schema: public; Owner: cmoore
--

CREATE UNIQUE INDEX up_history_id_index ON public.up_history USING btree (id);


--
-- PostgreSQL database dump complete
--

