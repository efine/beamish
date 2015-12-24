

# Module beamish #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This is a simple module that provides useful BEAM-related utility
functions that at some time one or more of us wished we had.

<a name="description"></a>

## Description ##
Like one that will tell us which types a BEAM exports, so we can
us them in our function specs.

<a name="types"></a>

## Data Types ##




### <a name="type-beam_spec">beam_spec()</a> ###


<pre><code>
beam_spec() = module() | <a href="file.md#type-filename">file:filename()</a> | binary()
</code></pre>




### <a name="type-type_spec">type_spec()</a> ###


<pre><code>
type_spec() = {Type::atom(), Arity::non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exported_types-1">exported_types/1</a></td><td>
Return the module name and a list of types that were exported by a
BEAM file, presumably using <code>-export_type</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="exported_types-1"></a>

### exported_types/1 ###

<pre><code>
exported_types(Beam) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Beam = <a href="#type-beam_spec">beam_spec()</a></code></li><li><code>Result = {ok, {Mod::module(), Types::[<a href="#type-type_spec">type_spec()</a>]}} | {error, beam_lib, Reason::term()}</code></li></ul>

Return the module name and a list of types that were exported by a
BEAM file, presumably using `-export_type`.

If there is no abstract code in the BEAM file, return the module
name and an empty list.

