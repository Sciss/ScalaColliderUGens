# ScalaColliderUGens

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/Sciss/ScalaCollider?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/Sciss/ScalaColliderUGens.svg?branch=master)](https://travis-ci.org/Sciss/ScalaColliderUGens)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/scalacolliderugens-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/scalacolliderugens-core_2.11)

## statement

Specification and base API of [ScalaCollider](https://github.com/Sciss/ScalaCollider) UGens, as well as a core library of generated UGen classes.

This project is (C)opyright 2008&ndash;2017 by Hanns Holger Rutz. All rights reserved. All sub projects released under the GNU LGPL v2.1+, except for the specification which is released under a BSD-style license. All code comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

## sub projects

 - `spec` contains an XML specification of the standard SuperCollider UGens, suitable for synthesising ScalaCollider UGen classes or other meta data purposes
 - `api` contains the base classes for ugens, graph elements, ugen and synth graphs.
 - `gen` generates the source codes of the `core` project from the descriptions provided by the `spec` project
 - `core` contains ScalaCollider classes for the standard UGens

## linking

All artifacts are published to Maven Central, and are available as follows:

    "de.sciss" %  "scalacolliderugens-spec" % v
    "de.sciss" %% "scalacolliderugens-api"  % v
    "de.sciss" %% "scalacolliderugens-core" % v
    "de.sciss" %% "scalacolliderugens-plugins" % v

The current stable version `v` is `"1.16.7"`.

The `spec` contains the XML meta data, `api` contains basic types without specific UGens, `core` contains the standard UGens included with SuperCollider, and `plugins` will include the third-party plugins managed by the [sc3-plugins](https://github.com/supercollider/sc3-plugins) project (still incomplete).

## building

The project builds with sbt 0.13 against Scala 2.12, 2.11. To compile, run `sbt compile`.

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

### generating additional UGen class files

The UGen descriptions reside in XML files. The `spec` sub project contains files for the standard UGens included with a plain SuperCollider installation. You will need to create additional XML files if you wish to compile sources for third party UGens.

To synthesize the source code for a given UGen description XML file, run as follows:

    $ sbt
    ...
    > project scalacolliderugens-gen
    > run -d path/to/scala/source/output path/to/descriptions.xml

The generated source files then need to be compiled against `scalacolliderugens-core`.

If you want to contribute UGen descriptions, there are two existing sbt sub-projects to consider:

- `core` creates class files for the UGens included in the standard SuperCollider distribution. 
  If you are not editing an existing XML file, you need to ensure a new XML file is included 
  in `UGenSpec.standardPlugins`.
- `plugins` creates class files for the UGens included in the
  [sc3-plugins](https://github.com/supercollider/sc3-plugins) umbrella project. Again, if you are not
  editing an existing XML file, you need to ensure a new XML file is included, this time in the list 
  `UGenSpec.thirdPartyPlugins`.
- if you are planning to contribute the description of a plugin that is neither in the standard UGens nor in
  the sc3-plugins project, please consult first where those classes should be published. Most likely, we
  will add another sbt sub-project similar to the `plugins` project. If you look at its code, you will see that
  you have to change the call to `runUGenGenerator` so that instead of `args = "--plugins" :: Nil` you
  provide an explicit list of paths to the XML files you wish to generate sources from.
  
Please have a good look at the current XML files and try to follow the documentation style used there. In particular

- try to be precise about the technical description of the behaviour of the UGen and its arguments. Try to think
  of the typical case where you would consult the API doc of the UGen. Which are the behavioural aspects you would
  want to look up, such as initialisation conditions, precise triggering behaviour, interaction of parameters,
  differences to related UGens.
- each argument should be described in terms of its behaviour, the physical units (if any), the allowed or
  expected value ranges, and corner cases.
- ideally, each UGen should be documented along with a concise example (in the XML file).

### format of ugen XML descriptions

There is no DTD yet. But the structure of the XML file is as follows:

    <ugens revision="<num>">
      <ugen name="UGenName" [ ugenAttrs ]>
        <rate name="RateName" [ implied="true" [ method="MethodName" ]]>
          [ <arg ... /> ]
        </rate>
        <rate ... />
        [ <output ... /> ]
        <arg name="ArgumentName" [ argAttrs ]>
          [ <doc>Argument description</doc> ]
        </arg>
        <arg ... />
        [ <doc>
          <text>UGen description</text>
          [ <example name="description">
              Example code demonstrating the UGen
            </example>
            <example ... /> ]
          [ <see>ugen.RelatedUGenName</see>
            <see ... /> ]
          </doc> ]
      </ugen>
      [ <ugen ... /> ]
    </ugens>

All UGens within one file are considered to be part of that particular `.scx` plugin. Their synthesized classes will also be grouped in a file by that name.

#### UGen Attributes

UGen Attributes (`ugenAttr`) are boolean flags (all false by default) which can be set to characterize a UGen:

|Attribute Name|Meaning when value is `"true"`   |Example   |
|--------------|---------------------------------|----------|
|`reads-bus`   |UGen reads from a bus            |`In`      |
|`writes-bus`  |UGen writes to a bus             |`Out`     |
|`reads-buf`   |UGen reads audio buffer data     |`BufRd`   |
|`writes-buf`  |UGen overwrites audio buffer data|`BufWr`   |
|`reads-fft`   |UGen reads from an FFT buffer    |`IFFT`    |
|`writes-fft`  |UGen writes to an FFT buffer     |`FFT`     |
|`done-flag`   |UGen sets a 'done flag'          |`Line`    |
|`side-effect` |UGen has another side effect, for example causing a 'done-action', sending OSC commands, or printing to the console |`SendTrig`|
|`random`      |UGen depends on random seeding   |`WhiteNoise`|
|`indiv`       |Each UGen is otherwise individual, even with identical inputs|Demand UGens advance their inputs|
|`helper`      |A helper element that is not a genuine UGen itself|`Nyquist`|
|`sourcecode`  |Manually written source code is provided|`Nyquist`|

Part of this information is used by ScalaCollider when building the UGen graph. For example, subtrees which do not have any side effects are automatically removed. UGens which have side effects are those for which either of the following flags is set: `writes-bus` | `writes-buf` | `writes-fft` | `side-effect`. Furthermore, multiple occurrences of UGens which are functionally equivalent are collapsed. UGens are _functionally not equivalent_ if either of the following flags is set: _any of the side effects_ | _any of the resource readers_ | `random` | `indiv`. That is to say, if there are two `WhiteNoise` UGens, they are functionally distinct by definition and will thus not be collapsed. The same is true for two `Out` UGens, even if their inputs are the same, as they have accumulative side effects on the bus to which they write. On the other hand, two `SinOsc` UGens with the same frequency and rate inputs are functionally equivalent and thus one can be replaced for the other.

More flags and meta data are planned in future version, e.g. oscillator signal ranges, filter coefficients.

#### UGen Rates

The possible rate names are `"scalar"`, `"control"`, `"audio"`, and `"demand"`. Each supported rate should have its own element. There are three extra attributes, `implied`, `method`, and `method-alias`.

`implied` says that the UGen not only has exactly one supported rate (an exception is thrown if you have a UGen with multiple rate elements and an `implied` attribute), but that this a natural precondition for the type of UGen. That way, the `case class` for that UGen does not carry a `rate` argument, but mixes in a trait which provides it. As a consequence, there is no argument for the rate when using pattern matching against that UGen. For example, `K2A` makes only sense at audio rate, `A2K` makes only sense at control rate, `FreeVerb` and `Pitch` make only sense at audio rate. Using this attribute, we have `case class K2A(in: GE)` (with mixin `AudioRated`) instead of the redundant `case class K2A(rate: Rate, in: GE)`.

Be very __careful__ with this attribute, it should not be used if another rate could be added in a future SuperCollider version, as this would break binary compatibility. This is why `implied` has been removed from `DiskIn`, for example (there is no reason, why `DiskIn` could not support control rate reading in the future).

The second attribute, `method`, builds up `implied` and requires that `implied` has been specified. It states that instead of the default method name in the companion object&mdash;`ir` for scalar rate, `kr` for control rate, `ar` for audio rate, and `dr` for demand rate&mdash;an alternative method name is used. The method name is typically `apply`, so that instead of `FFT.kr(buf, sig)` you have to write `FFT.apply(buf, sig)` or short `FFT(buf, sig)`, which is more convenient.

`method-alias` adds _an additional_ method for the rate. An example is `IFFT` which specifies `<rate name="audio" method-alias="apply"/>`. This means the default method `ar` is created, plus an `apply` method as an alias.

#### Argument Attributes

|Attribute Name|Value                            |Example   |
|--------------|---------------------------------|----------|
|`default`     |Default expression for the argument. This can be either a number literal or a special string such `"nyquist"` or `"doNothing"` (see below) |`440`, `1.0`, `inf` |
|`type`        |Argument type when it is not `ge` (generic graph element). (See below) |`PanAz`, `Poll` |
|`rate`|Constrains the supported rate for this argument. The only values presently recognized are `"ugen"` which means the argument is required to run at the _same rate as the UGen_, or a rate name such as `"audio"` which enforces that particular rate. Please see also the section below about rate specific argument settings. | `DiskOut` (`in`) |

The following table lists the allowed `type` values, and corresponding ways of defining default values. If the default value is unambiguous, the type is automatically inferred, e.g. using `default="high"` implies a `type="trig"`. If the type and default value are incompatible, the parser will throw an exception.

|Type name      |Description                                  |Example defaults    |
|---------------|---------------------------------------------|--------------------|
|`ge` (default) |Generic graph element                        | `-1.0`, `440.0`    |
|`ge-int`       |Graph element used as integer                | `-1`, `18`         |
|`ge-string`    |String converted to variadic float constants | `"poll"`           |
|`bus`          |Bus index                                    | no default allowed |
|`buf`          |Buffer identifier                            | no default allowed |
|`fft`          |FFT buffer phase chain signal                | no default allowed |
|`trig`         |Trigger signal (transition <= 0 to >0)       | `low`, `high`      |
|`switch`       |Off/On signal (zero versus non-zero)         | `false`, `true`    |
|`gate`         |Gating signal (open above zero)              | `closed`, `open`   |
|`mul`          |Synthetic multiplier input                   | `1.0`              |
|`action`       |Done action                                  | `freeSelf`, `doNothing` |
|`done-flag`    |UGen which sets a done flag                  | no default allowed |
|`int`          |Static integer (no graph element)            | `-1`, `18`         |

A special default value `"nyquist"` can be used which is understood as `SampleRate.ir/2`. Note that expressions such as `"60.midicps"` have been currently disallowed for simplicity and language neutrality.

The following three argument attributes have boolean values, and are `"false"` by default:

|Attribute Name|Meaning when value is `"true"`   |Example   |
|--------------|---------------------------------|----------|
|`ugen-in` |Forces an `Int` type argument to be used as actual UGen input and not just auxiliary type. | `MFCC` |
|`variadic`|Indicates an argument which expands over multiple UGen inputs. | `RecordBuf` (`in`), `Dseq` (`seq`) |
|`prepend-size`|Must be combined with `variadic`: the variadic arg's size is prepended as additional UGen input. | `PackFFT` |

Arguments should be chosen careful not to conflict with methods available on `GEOps`. This is the reason, why various arguments which are named `rate` in SCLang have been renamed for example to `speed`, `freq` etc. It is recommended to take a look at the naming of the arguments in the default plugins (rather than relying on the naming in SCLang which is often unreflected and irregular) and try to reuse them whenever possible, and to be as consistent as possible with abbreviations. Care is also needed with the default values. There are some default values in SCLang which are insensible, while other useful defaults are missing. The aim is not to provide default values for every possible argument, but to require to fill in arguments for which defaults do not make sense.

#### Argument Positions

For some UGens, the actual positions of the arguments as they are coded in the underlying Plugin are either unintuitive (e.g. with respect to argument priority), inconvenient (e.g. with respect to default values), or irregular (e.g. different from an otherwise very similar other UGen). In those cases you are permitted to change the argument order as it is presented to the ScalaCollider user. To make sure the arguments are properly wired in the resulting SynthDef, explicit argument positions must be given.

If a UGen's arguments do not have `pos` attributes, they are considered in the order in which they appear in the XML file. Otherwise, the order of appearance in the XML file corresponds with the order __in the underlying Plugin__, whereas the values of the `pos` attributes specify the positions as presented to the user (counting from zero). Please read the previous sentence __very carefully__, as a common mistake is to falsely believe the correspondence to be the other way around.

As an example, consider `XOut` which has the unintuitive argument order of _bus_, followed by _cross-fade level_, followed by _input signal_. Compare this to `Out` which has the two arguments of _bus_, followed by _input signal_. We decided to make the `XOut` arguments appear to the user in the order of _bus_, then _input signal_ (just like `Out`), then followed by the distinguishing parameter of the _cross-fade level_. Thus we assign `pos="1"` to the `in` argument and `pos="2"` to the `xfade` argument, so they switch their positions. To minimize mistakes, ScalaCollider-UGens requires that we also add `pos="0"` to the `bus` argument, even if that does not affect its final position. The whole UGen specification thus becomes:

    <ugen name="XOut" writes-bus="true">
       <no-outputs/>
       <rate name="audio">
          <arg name="in" rate="ugen"/>
       </rate>
       <rate name="control"/>
       <arg name="bus" pos="0"/>
       <arg name="xfade" pos="2"/>
       <arg name="in" variadic="true" pos="1"/>
       <doc warn-pos="true"/>
    </ugen>

__Note__ how the attribute `warn-pos="true"` was added to the `doc` element. This makes Scaladoc add an extra note to alert the user of the change in argument order. This is particularly important, as it may create confusion when coming from SCLang. It is recommended to apply argument reordering only after careful consideration, and to abstain from them when in doubt.

#### Rate Specific Argument Settings

Sometimes it is necessary to change the default value or the description of an argument _with respect to the rate_ at which the UGen runs. And sometimes the rate constraint for an argument only applies to particular rates at which the UGen runs. In this case, it is permitted to embed an auxiliary `arg` element inside the `rate` element. This auxiliary `arg` element must have a corresponding element in the outer scope (inside the `ugen` element). Their correspondence is established by using the same `name` attribute, and the auxiliary element may provide an additional `default` or `rate` attribute and may contain an additional `doc` element.

As an example for different default values, here is the full text of `LeakDC`:

    <ugen name="LeakDC">
       <rate name="control">
          <arg name="coeff" default="0.9"/>    <!-- provide a default value for the `kr` method -->
       </rate>
       <rate name="audio">
          <arg name="coeff" default="0.995"/>  <!-- provide a default value for the `ar` method -->
       </rate>
       <arg name="in" rate="ugen"/>
       <arg name="coeff"/>  <!-- the outer argument must still be provided -->
    </ugen>

An an example of restricting the argument's rate only in certain cases is `Out`:

    <ugen name="Out" writes-bus="true">
       <no-outputs/>
       <rate name="audio">
          <arg name="in" rate="ugen"/>
       </rate>
       <rate name="control"/>
       <rate name="scalar"/>
       <arg name="bus"/>
       <arg name="in" variadic="true"/>
    </ugen>

Here, the "outer" definition of argument `in` says that the argument is a multi-channel argument, but it does not enforce a particular rate. Only for the case that `Out` is run at audio rate, the auxiliary entry for `in` enforces that `in` in this case must run at the same rate as the UGen (thus audio rate, too).

#### Outputs

By default the UGen is considered to have one monophonic output. All other UGens must explicitly contain either a `<no-outputs/>` element, or one or more `<output ... />` elements. An output element may have a `name` and `type` attribute, and one element may have a `variadic="<id>"` attribute, where `<id>` is the name of the input argument determining the number of channels. A `<doc>` element may be nested inside a `<output>` node. Examples:

|Example                                       |UGen           |Note                             |
|----------------------------------------------|---------------|---------------------------------|
|`<no-outputs/>`                               | `Out`         |                                 |
|`<output name="left"/><output name="right"/>` | `Pan2`        |                                 |
|`<output variadic="in"/>`                     | `Demand`      | `in` is a `GE` type input       |
|`<output variadic="numChannels"/>`            | `DiskIn`      | `numChannels` is an `Int` input |
|`<output name="chain" type="fft"/>`           | `PV_MagShift` |                                 |

#### Descriptions

The description text for arguments is the text inside the argument's `<doc></doc>` element. The description text for the UGen is inside the `<text></text>` element inside the `<doc></doc>` element. In each case, standard [Scaladoc formatting](https://wiki.scala-lang.org/display/SW/Syntax) is allowed. Cross-links are provided through any number of `<see></see>` elements.

Please follow carefully the style of the descriptions used for the standard UGens. They adhere mostly to [Javadoc style practice](http://www.oracle.com/technetwork/java/javase/documentation/index-137868.html#styleguide), and not so much to the more colloquial style of SCLang docs. The purpose here is not to include lengthy examples, but to be technically precise in the meanings of the argument values and the exact functioning of the UGen, if possible covering corner cases, providing details about underlying formulas, phase behavior of oscillators, typical ranges and scale.

Whenever the argument order has been significantly changed from the SCLang counterpart, the UGen's `doc` element should contain the attribute `warn-pos="true"` which will create a special highlight in the Scala docs to alert the reader of this change.

