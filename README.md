# Sick Filing

Filing things on a hard drive is not affectively easy. 100% of the time I've seen people use heavily nested directories, no organisation at all and a sickening combination of the two.

Recently there has been talk of tagging filing systems, which sound awesome but are non trivial to use. Filing should be trivial.

## Installation

Create a symlink of sick-filing.bin in a $PATH dir. Make sure you call the symlink sick-filing.bin.

Add the following to your ```~/.bashrc```

```bash
function filing() {
    TFILE="/tmp/sick-filing.$$.tmp"
    sick-filing.bin $TFILE && cd `cat $TFILE`
}
```

Then you can call ```$ filing```. I have made an alias to sfi, it's shorter.


## Things I'd like to say

Continuing, our options are:

### Option 1 Deep directory nesting

Imagine you have a set of ebooks and dvds. Do you file them under ```Educational/ebooks``` as ```Educational/DVDs```. Or maybe ```Media/Educational/ebooks``` and ```Media/Educational/DVDS```.

Consider non educational dvds and ebooks.

```Media/Non-Educational/DVDS``` and ```Media/Educational/DVDS```, or is it ```Media/DVDS/Educational``` and ```Media/DVDS/Non-Educational```.

What about ```Media/DVDS/Movies```. Is a David Attenborough movie educational or entertainment???

Category hell ensues and no one can ever reliably go to someone else's pc (Much less their own) and find something simply without having to whip out a search tool. And then the capabilities of the search tool often lack luster.

SO I wrote... another search tool... but this one is nice, it's like IDO-mode in emacs (kinda, it's still a baby).


### Option 2 Scattered files

Let's not even consider this insanity.


### Option 3 A mix of nesting and scattering.

Let's not even consider this insanity.


### Option 4 Sane alphabetical filing with a few helpers

```
Filing/Passport--John/[passport_files]
Filing/Ruby--Std-Lib-1.9.3-Manual/
Filing/Resume--John/
Filing/Resume--Jane/
Filing/One-Flew-Over-The-Cuckoos-Nest--Movie--Awesome/
Filing/Practical-Common-Lisp--Ebook--Programming/
```

And so on. It's sane, things are in one spot and you can always place large files on another hard disk, secure files on an encrypted drive, etc, then symlink a directory into your Filing directory.


### Efficiently finding things in your filing system

We usually put things in there once, then look for them many, many times. So we can put some effort into creating useful directory names, such as the ones in the examples above.

Then we use the _sfi_ program in this repo to find things.

I'm using movie names to illustrate my point because my own Filing dir has all sorts of words in it I don't want on the net. DISCLAIMER - there is nothing in these directories, I do not have these movies on my hdd.


#### The directory
![Image of directories]
(https://raw.githubusercontent.com/quazimodo/sick-filing/master/images/ls.png)


#### Load up sfi
![Image of sfi init screen]
(https://raw.githubusercontent.com/quazimodo/sick-filing/master/images/sfi-1.png)


#### Begin typing the stuff you want
![Image of sfi finding things]
(https://raw.githubusercontent.com/quazimodo/sick-filing/master/images/sfi-2.png)


#### Observe that results are narrowed awesomely using fuzzy matching

You don't need to type One-Flew-Over-The-Cuckoos-Nest, oncnes  does it :D.

You could find Passport--john like pasjoh, etc.

![Image of sfi narrowing results]
(https://raw.githubusercontent.com/quazimodo/sick-filing/master/images/sfi-3.png)


#### Keep going till you spot what you want

![Image of sfi finding your match]
(https://raw.githubusercontent.com/quazimodo/sick-filing/master/images/sfi-4.png)


#### Select the match and go to that directory

So you can do ```Control-s``` to go to the next match in the list, ```Control-r``` to go to the previous. (Can you tell I'm an emacs dork?)

Then press ```Enter/Return``` to go to that thing's directory.

![Image of sfi finishing up]
(https://raw.githubusercontent.com/quazimodo/sick-filing/master/images/finished.png)


### Other interesting things

This thing tries to match each word you type, separated by spaces, and return the most relevant stuff. Observe how it matches the following:


#### Matching Star-Trek

![Image of star-trek matches]
(https://raw.githubusercontent.com/quazimodo/sick-filing/master/images/sfi-star-trek.png)


#### Matching Star Trek

![Image of star trek matches]
(https://raw.githubusercontent.com/quazimodo/sick-filing/master/images/sfi-star_trek.png)


### Is it useful

Yes, for me it has been VERY much since I live on the command line.

### Does it work perfectly

No;
- I'd like it to be able to recurse directories and match to files within dirs.
- It should be able to show you some results, and match more in a worker thread in the background so that you can use the ui comfortably. At the moment huge directory counts and big query strings hurt and lag.


### Just to make things more arcane

It's written in Common Lisp, compiled with SBCL. It's not intended to be SBCL specific code, portability is güd. I don't know if this version is portable. Yet.

# License

MIT I guess :D