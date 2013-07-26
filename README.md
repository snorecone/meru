# meru

[UNDER CONSTRUCTION] Add methods to your erlang modules that map to riak objects.

## installation

Install as a rebar dependency:

```erlang
{meru, ".*", {git, "git@github.com:assplecake/meru.git", "master"}}
```

## usage

Add the `meru_transform` parse transform to a module that you would like to wrap a riak object:

```erlang
-module(user).

-compile({parse_transform, meru_transform}).
```

Define a record that represents the data stored in riak. In this case, we'll use a user record:

```erlang
-record(user, {
  name,
  email,
  encrypted_password
}).
```

Now we'll need a key function, which serves to generate the key for the object in riak. Usually we'll define a binary clause to represent the key, as well as a clause that takes the record as an input. 

```
make_key(#user{ email = Email }) when is_binary(Email) ->
  Email;
make_key(Key) when is_binary(Key) -> Key.
```

When saved objects are updated, we'll need the merge logic, so next define a merge function that will be passed 3 arguments - the saved record, the new record, and a list of options that you can pass through when updating. If a record is not found to merge, meru will call the merge function with `notfound` as the first argument:

```
merge(notfound, NewUser, _) ->
  NewUser;
merge(OldUser, NewUser, Options) ->
  OldUser#user{
    name = NewUser#user.name
    encrypted_password = NewUser#user.encrypted_password
  }.
```

Set the module attributes for riak bucket, record name, key function, and merge function:

```erlang
-meru_bucket(<<"users">>).
-meru_record(user).
-meru_keyfun(make_key).
-meru_mergefun(merge).
```

Note: meru serializes records as proplists for storage in riak. It's possible to have different serialization strategies in the future.

Once your module is compiled, meru will add and export the following functions:

```
Record    = ?MODULE:new()
Record    = ?MODULE:new(Proplist)
{ok, Obj} = ?MODULE:get(KeyOrRecord) % KeyOrRecord is whatever arguments your keyfun can take
{ok, Key} = ?MODULE:put(Record)      % meru returns a key
{ok, Key} = ?MODULE:put_merge(Record, Options) % the riak object is read and merged with your mergefun
{ok, Key} = ?MODULE:put_merge(Key, Record, Options) % in the case that you want to pass your key explicitly
{ok, Key} = ?MODULE:delete(KeyOrRecord)
Proplist  = ?MODULE:record_to_proplist(Record)
Record    = ?MODULE:proplist_to_record(Proplist)
```

See the [examples directory](https://github.com/assplecake/meru/tree/master/examples) for complete examples.

## helper modules

### meru_riak

`meru_riak` takes the same commands as riak protobuffs client `riakc_pb_socket`, but doesn't require obtaining a pid, it uses a connection from the meru pool.

```erlang
{ok, RiakObject} = meru_riak:get(Bucket, Key).
```

## license

Apache 2.0 http://www.apache.org/licenses/LICENSE-2.0.html
