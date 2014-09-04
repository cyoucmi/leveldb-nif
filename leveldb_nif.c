#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "leveldb-1.15.0/include/leveldb/c.h"
#include "erl_nif.h"

#define MAX_PATH 256

static ErlNifResourceType *LeveldbResourceType; 

static leveldb_t *pDb;
static leveldb_readoptions_t *pReadOptions;
static leveldb_writeoptions_t *pWriteOptions;
static leveldb_options_t *pOptions;


static void
LeveldbResourceDtor(ErlNifEnv *env, void *obj){
    struct leveldb_iterator_t **ppIterator = obj;
    leveldb_iter_destroy(*ppIterator);
}


/*加载leveldb_nif*/
static int leveldb_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);

/*卸载leveldb_nif*/
static void leveldb_unload(ErlNifEnv *env, void *priv_data);

static ERL_NIF_TERM leveldb_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static int
leveldb_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info){
    pDb = NULL;
	pReadOptions = NULL;
	pWriteOptions = NULL;
    ErlNifResourceFlags tried;
    LeveldbResourceType = enif_open_resource_type(env, NULL, "leveldb", LeveldbResourceDtor, ERL_NIF_RT_CREATE,&tried);
	return 0;
};


static void 
leveldb_unload(ErlNifEnv *env, void *priv_data){
    if(pDb){
		leveldb_close(pDb);
	}
    if(pReadOptions){
        leveldb_readoptions_destroy(pReadOptions);
    }
    if(pWriteOptions){
        leveldb_writeoptions_destroy(pWriteOptions);
    }
    if(pOptions){
        leveldb_options_destroy(pOptions);
    }
	
}

static ERL_NIF_TERM
make_fatal_tuple_info(char *zMsg, ErlNifEnv *env){
    ERL_NIF_TERM Ret = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, zMsg, ERL_NIF_LATIN1));
    free(zMsg);
    return Ret;
}


static ERL_NIF_TERM 
leveldb_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    char buf[MAX_PATH];
    size_t buf_size = MAX_PATH;
    if( enif_get_atom(env, argv[0], buf, buf_size, ERL_NIF_LATIN1) <= 0){/*获取失败*/
        strcpy(buf, "leveldb_test.db");/**读取默认值*/
    }
    pOptions = leveldb_options_create();
    leveldb_options_set_create_if_missing(pOptions, 1);
    pWriteOptions = leveldb_writeoptions_create();
    pReadOptions = leveldb_readoptions_create();
    
    char* err = NULL;
    pDb = leveldb_open(pOptions, buf, &err);
    if(err){
        return make_fatal_tuple_info(err, env);
    }

   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM 
leveldb_set(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    ErlNifBinary key_bin;
    ErlNifBinary value_bin;
    enif_inspect_binary(env, argv[0], &key_bin);
    enif_inspect_binary(env, argv[1], &value_bin);
    char* err = NULL;
    leveldb_put(pDb,pWriteOptions, key_bin.data, key_bin.size, value_bin.data, value_bin.size, &err);
    if(err){
        return make_fatal_tuple_info(err, env);
    }
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM 
leveldb1_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    ErlNifBinary key_bin;
    enif_inspect_binary(env, argv[0], &key_bin);
    char* err = NULL;
    size_t val_len;
    char* val;
    val = leveldb_get(pDb, pReadOptions, key_bin.data, key_bin.size, &val_len, &err);
	if( val_len == 0 ){
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "not_found"));
	}
    if(err){
        return make_fatal_tuple_info(err, env);
    }

    ErlNifBinary value_bin;
    if (!enif_alloc_binary(val_len, &value_bin)) { 
	return enif_make_badarg(env);
    }
    memcpy(value_bin.data, val, val_len);
    free(val);
    return enif_make_binary(env, &value_bin);
}

static ERL_NIF_TERM 
leveldb1_del(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    ErlNifBinary key_bin;
    enif_inspect_binary(env, argv[0], &key_bin);
    char* err = NULL;
    leveldb_delete(pDb, pWriteOptions, key_bin.data, key_bin.size, &err);
    if(err){
        return make_fatal_tuple_info(err, env);
    }

    return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM
leveldb_init_iterator(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct leveldb_iterator_t **pp_iterator = (struct leveldb_iterator_t**)enif_alloc_resource(LeveldbResourceType, sizeof(leveldb_iterator_t*));
    *pp_iterator = leveldb_create_iterator(pDb, pReadOptions);
    ERL_NIF_TERM ret = enif_make_resource(env, (void*)pp_iterator);
    enif_release_resource(pp_iterator);
    return ret;
}

static ERL_NIF_TERM
leveldb_iterator_first_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct leveldb_iterator_t **pp_iterator = NULL;
    if (!enif_get_resource(env, argv[0], LeveldbResourceType, (void**)&pp_iterator)) { 
	    return enif_make_badarg(env);
    }
    leveldb_iter_seek_to_first(*pp_iterator);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
leveldb_iterator_last_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct leveldb_iterator_t **pp_iterator = NULL;
    if (!enif_get_resource(env, argv[0], LeveldbResourceType, (void**)&pp_iterator)) { 
	    return enif_make_badarg(env);
    }
    leveldb_iter_seek_to_last(*pp_iterator);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
leveldb_iterator_valid_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct leveldb_iterator_t **pp_iterator = NULL;
    if (!enif_get_resource(env, argv[0], LeveldbResourceType, (void**)&pp_iterator)) { 
	    return enif_make_badarg(env);
    }
    unsigned char rc = leveldb_iter_valid(*pp_iterator);
	if( rc ){
        return enif_make_atom(env, "true");
	}else{
        return enif_make_atom(env, "false");
    }
}

static ERL_NIF_TERM
leveldb_iterator_next_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct leveldb_iterator_t **pp_iterator = NULL;
    if (!enif_get_resource(env, argv[0], LeveldbResourceType, (void**)&pp_iterator)) { 
	    return enif_make_badarg(env);
    }
    leveldb_iter_next(*pp_iterator);
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
leveldb_iterator_prev_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct leveldb_iterator_t **pp_iterator = NULL;
    if (!enif_get_resource(env, argv[0], LeveldbResourceType, (void**)&pp_iterator)) { 
	    return enif_make_badarg(env);
    }
    leveldb_iter_prev(*pp_iterator);
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
leveldb_iterator_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct leveldb_iterator_t **pp_iterator = NULL;
    if (!enif_get_resource(env, argv[0], LeveldbResourceType, (void**)&pp_iterator)) { 
	    return enif_make_badarg(env);
    }
    
    size_t key_len, value_len;
    const char *key_data;
    const char *value_data;

    key_data = leveldb_iter_key(*pp_iterator, &key_len);

    ErlNifBinary key_bin;
    if (!enif_alloc_binary(key_len, &key_bin)) { 
	return enif_make_badarg(env);
    }
    memcpy(key_bin.data, key_data, key_len);


    value_data = leveldb_iter_value(*pp_iterator, &value_len);

    ErlNifBinary value_bin;
    if (!enif_alloc_binary(value_len, &value_bin)) { 
	return enif_make_badarg(env);
    }
    memcpy(value_bin.data, value_data, value_len);

    return enif_make_tuple2(env, enif_make_binary(env, &key_bin), enif_make_binary(env, &value_bin));
}





static ErlNifFunc nif_funcs[] = {
	{"init", 1, leveldb_init},
    {"set", 2, leveldb_set},
    {"get", 1, leveldb1_get},
    {"del", 1, leveldb1_del},
    {"init_iterator", 0, leveldb_init_iterator},
    {"iterator_first", 1, leveldb_iterator_first_entry},
    {"iterator_last", 1, leveldb_iterator_last_entry},
    {"iterator_valid", 1, leveldb_iterator_valid_entry},
    {"iterator_next", 1, leveldb_iterator_next_entry},
    {"iterator_prev", 1, leveldb_iterator_prev_entry},
    {"iterator_entry", 1, leveldb_iterator_entry},

};

ERL_NIF_INIT(leveldb_nif, nif_funcs, leveldb_load, NULL, NULL, leveldb_unload);

