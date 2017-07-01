---
title: Audio Fact(s) of the Day
language: english
description: A mix of some sound-related curiosities I discovered while reading
    Wikipedia.
---

Hi there!

I just wanted to post some random facts about sound and how we perceive it. I
encourage you to follow the links and find out more. Well, here you go:

* [loudness][loudness] is *not* directly proportional to the pressure that
  sound wave applies to your ear;
* sounds of the same pressure but different tones would appear to have
  different loudness. Humans are most sensitive to frequencies from about 2 kHz
  to 5 kHz. [A-weighting curve][a-weighting] (along with a bunch of others)
  describe the connection;
* furthermore, [loudness depends on the duration of the
  tone][loudness-integral]. Basically, it is an integral of the sound intensity
  (air pressure) during the previous 600–1 000 ms;
* there's no single unit to measure loudness. We have
  [dBSPL][decibel-acoustics], decibel using 20 micropascals as a reference
  value. There's dB(A), which is just dBSPL corrected in accordance with
  A-weighting curve. There's also [phon][phon], defined as an equal to 1 dBSPL
  at a frequency of 1 kHz, and then scaled for other frequencies, forming
  [equal-loudness contour][equal-loudness-contour];
* in digital systems it's all backwards: 0 [dBFS ("decibel relative to full
  scale")][dbfs] is the loudest sound possible, and anything else is encoded by
  negative values (e.g., -3 dBFS is half as loud as possible);
* [bands of proportional width are perceived as if they have the same
  width][pink-noise]. E.g., difference between 20 Hz and 30 Hz seems to be the
  same as between 200 Hz and 300 Hz;
* science that studies perception of sound is called
  [psychoacoustics][psychoacoustics];
* [vuvuzela horn at 1 meter away][sound-pressure-levels-examples] from your
  ears produces 120 dB(A) sound. Threshold of pain is 130 dB;
* [you need about half a minute of listening to 115 dB sound to damage your
  ears][deafness];
* specifics of human hearing can be described with [psychoacoustic
  model][psychoacoustic-model]. It is then used in "lossy" codecs (MP3, Ogg
  Vorbis, etc.) to exclude sounds that you are unlikely to hear, thus allowing
  for better compression;
* striving to make songs stand out, companies apply dynamic compression. It
  makes quiet sounds louder, increasing overall loudness while robbing songs of
  their emotional power. This phenomenon is called [loudness
  war][loudness-war].

Hope you learned something new. See you!

[loudness]: https://en.wikipedia.org/wiki/Loudness
[loudness-integral]: https://en.wikipedia.org/wiki/Loudness#Explanation
[a-weighting]: https://en.wikipedia.org/wiki/A-weighting
[sound-pressure-levels-examples]: https://en.wikipedia.org/wiki/Sound_pressure_level#Examples_of_sound_pressure_and_sound_pressure_levels
[decibel-acoustics]: https://en.wikipedia.org/wiki/Decibel#Acoustics_2
[phon]: https://en.wikipedia.org/wiki/Phon
[equal-loudness-contour]: https://en.wikipedia.org/wiki/Equal-loudness_contour
[psychoacoustics]: https://en.wikipedia.org/wiki/Psychoacoustics
[psychoacoustic-model]: https://en.wikipedia.org/wiki/Psychoacoustics#Software
[deafness]: https://en.wikipedia.org/wiki/Hearing_damage
[pink-noise]: https://en.wikipedia.org/wiki/Colors_of_noise#Pink_noise
[dbfs]: https://en.wikipedia.org/wiki/DBFS
[loudness-war]: https://en.wikipedia.org/wiki/Loudness_war
