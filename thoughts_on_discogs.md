# I'll figure out a title at some moment

I've been gathering data.... Finding the joy of in playing with data I made and combining it with the world out there to see what insights I can gain. 

 I have been using a wonderful music player Guayadeque for years (come to think of it: I really, really need to give this guy a coffee at a minimum). It is wonderful because it gives me the opportunity to sort my collection on practically anything (data acquired, year of release, artist, title... too much to state here). And that in itself is not amazing, but I can do that AND switch between the record a song appears on, or jump to the artist. This allows me to 'stumble' onto gems. But I am an absolute asshole and I think I can do better. And that is the start of a project to find a way to play my music _and_ be inspired by my own song library.

How do I plan to achieve this? By using my collection data (digital and physical) to tell me what I'll probably think is cool by combining playback behaviour and artist collaboration. Using last.fm data to see what I play often and what I play together. Using discogs data to see the whole field of collaborations of artists in my collection and outisde my collection. What do I hope to get out of combining the data? I hope to get better playlist suggestions (not only automated, but also wiki-like suggestions), and inspiration for collecting (@discogs: I might be convinced into being your new data marketeer).

 The first step wading into this was a repository that I haven't made public yet. Not out of code shame (I have no shame), but because I put user credential stuff in the code. I recently learned how to fix that _somewhat_ gracefully. I'll put the code public, when I get around to fixing this. But then again: why should I put it public? The project probably won't help you, because you're probably not using Guayadeque. So I am put that objection aside, because this is a totally egotistical project anyway. It is something I wanted, and probably I won't be able to implement it in such a way that you would want to use it anyway. That is the long version of me saying to you: don't hold your breath.

But what is sticking.... Getting playback data with last.fm. I've been using last.fm for music listening tracking for some time now, and I've been pretty observant in connecting it to for whatever playback device I can: mobile, PC and hifi (see my other project: pi-jukebox). I frivolously chose to let every secret service agency and criminal organization look at my music consumption. (And I'm so gullible, that I will pay said organisations for a nice solution that integrate Shazam/Soundhoud-like services with last.fm... How else can _I_ broadcast _me_ using _my_ vinyl collection to the rest of humanity?)

Last.fm is nice, but I found it's information somewhat lacking. Who is collaborating with who and on what records? Last.fm won't tell. But I found a perfect alternative by accident: Discogs. I subscribed there because I will never get tired of letting the world I exist. So I put my physical music collection on there for everyone to see. And I also started buying from the platform: seriously recommended if you're a collector like me. You can buy the most obscure music there and if you're a collector. For me it's dangerously easy. I've bought additions to my collection without any trouble: it is a community of seriously dedicated people. Don't expect all next day deliveries, but I find mostly that the deliveries are as promised and well packaged. You'll find some funny packaging there that just adds to the charm. Love it. And that dedication also shows in the data quality and diversity. It can tell you about band compositions, releases of whatever kind, artist aliases and more images than I'll ever need for my purposes. In short: they have more extensive information than last.fm, minus the play tracking. So I created another project: discogs_dashboard. Why? Because, again, I thought I can do better in inspiring myself with my music collection (and probably also facilitating my collectors itch).

 The end goal is to merge the two projects: using my play tracking with my music collection to be inspired and play more. I hope to add playback from several back-ends: my own digital music collection, Spotify and Deezer. The result: a player in which all inspiration should immediately be gratified and all prosals are immidiately clear: continue on said path, or go back.

# 2021-08-11

I want to suggest of sorting for the records which is based on artist collaboration. I also want to discern 'genre' indicators by choosing the most 'representative' (need to define) performer. The The more specific I am in selecting performers the more specific 'represenative' performers will show, while less relevant performers will dissapear.

Artist will sort according to related releases. The relative more collection items there are in a 'block' (subgraph), the more 'representative' artists are shown. If the user selects a representative performer, the community of which the performer is part of is displayed. The corresponding releases will be shown (filtering on in collection or not)

Is representative authorative? Artists having been involved of the most releases, not matter the role are the most representative. Number of edges

# 2021-08-23

* How do I stop the navigation through clusters?
* When do do I show releases?
* Add a cluster for all artists that belong to an exclusive cluster

# 2021-08-29

* Replace releases that connect performers with direct links between performers
* Create search patterns
  * id_search_pattern
  * id_node
  * is_in_scope
  * id_cluster
* Create path
  * id_search_pattern_from
  * id_search_pattern_to
  * qty_steps
* Show path, navigational options:
  * To top
  * Drill down
  * One back: 
    * id_time  
    * id_search_pattern_from
    * id_search_pattern_to
    