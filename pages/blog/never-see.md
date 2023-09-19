title: George Carlin's Things You Never See, AI edition
date: 2023-09-19
description: AI-generated images of George Carlin jokes.
publish: y
tags: ai

The comedian George Carlin had a bit known as [Things You Never See](https://www.youtube.com/watch?v=9X0F1Qjn0Ac), where he listed off a series of things you never see or hear (or want to hear) in day-to-day life. Things like a wheelchair with a roll cage, or someone telling their dad that he should drink more. It occurred to me that these would serve as amusing prompts for AI image generation, while giving me an opportunity to play around with the latest trendy AI tools. Continue reading to see the results!

**WARNING:** The prompts and resulting images are NOT SUITABLE FOR WORK. After all, George Carlin's comedy was so controversial that it sparked a court case about the US government's right to censorship.

### The setup
I first looked into DALL-E and Bing Image Creator, both of which offer web interfaces for inputting prompts and getting back AI-generated images. DALL-E turned out to be paywalled, however, while Bing Image Creator censors any "indecent material" in the prompts. So, defeated by The Man, I turned to Stable Diffusion (SD), an open source image-generating AI that can be run by anyone with a GPU and patience.

I had to jump between various guides to get SD to work. I'm not going to link a particular guide, because none of them contained all the information I needed. The steps can be summarised as follows:

* Download the weights/parameters for the SD neural network. The weights file for the latest version (1.4) is about 4GB in size.
* Download the [SD repository](https://github.com/CompVis/stable-diffusion) from GitHub.
* Download Anaconda, which is used to build an environment in which you can run SD.
* Set up the environment using Anaconda. Normally, this is a 1-line command, but I had to figure out how to save the dependencies to a different hard drive because my main one didn't have enough free space.
* Run SD using the script `scripts/txt2img.py` from the repository.

Another roadblock I hit was that my GPU (GeForce GTX 1050 Ti) has only 4GB of memory, while 6GB is normally required to run SD. Thankfully, a [fork exists](https://github.com/basujindal/stable-diffusion/tree/main) to run SD with lower memory requirements. It provides a version of `txt2img.py` that breaks the GPU computations into multiple stages, allowing you to get away with around 2.5GB of GPU memory. In exchange, you have to wait longer for results, and it took my computer over 1 minute to generate each image.

### The results
Carlin's jokes and the corresponding images are shown below. The jokes are in italics, while my comments are not. I edited the text of each joke to make it more prompt-like before passing it to SD. For "things you never hear", I generally passed the dialogue by itself, quote marks included. All the images were originally generated at a resolution of 512x512, since the output was terrible when I used 256x256. Finally, a suggestion: the images may look better if you squint.

*You never see a Rolls-Royce with a bumper sticker that says "shit happens".* It looks like a fancy car, but no bumper sticker.

<img src="{{ url_for('static', filename='img/never-see/sd/rolls-royce.jpg') }}"
     alt="What might be a cyan-coloured Rolls-Royce, but no bumper sticker."
     class="centered">

*You never see a really big tall fat Chinese guy with red hair.* Those symbols resemble Chinese characters, right? But he doesn't have red hair.

<img src="{{ url_for('static', filename='img/never-see/sd/chinese-guy.jpg') }}"
     alt="A large nipple-less man who looks vaguely Chinese, beside what might pass as Chinese characters. He doesn't have red hair, though."
     class="centered">

*You never see a wheelchair with a roll bar.* No roll bar!

<img src="{{ url_for('static', filename='img/never-see/sd/wheelchair.jpg') }}"
     alt="A woman in a normal wheelchair."
     class="centered">

*You never see someone taking a shit while running at full speed.* I used a more explicit prompt to generate the second image, since I thought the first one didn't live up to its full comedic potential. Note how the floor got turned into poop. I kindly censored the naked runner's weird smooth crotch.

<img src="{{ url_for('static', filename='img/never-see/sd/shit-while-running.jpg') }}"
     alt="A runner on tarmac, who doesn't appear to be defecating."
     class="centered">

<img src="{{ url_for('static', filename='img/never-see/sd/shit-while-running-2.jpg') }}"
     alt="A naked runner, whose head is out of frame, running on a floor of poop."
     class="centered">

*You never see a picture of Margaret Thatcher strapping on a dildo.* I generated a second one because I thought it was funny. Hopefully it's okay to do this, given that she's dead? He told the joke while she was alive, so...

<img src="{{ url_for('static', filename='img/never-see/sd/thatcher-1.jpg') }}"
     alt="Margaret Thatcher holding what appears to be a cross between a dildo and a microphone."
     class="centered">

<img src="{{ url_for('static', filename='img/never-see/sd/thatcher-2.jpg') }}"
     alt="Margaret Thatcher holding a vaguely penis-like object."
     class="centered">

*You never hear someone say "Dad, you really ought to drink more".* The first of the lame "you never hear..." prompts, although it's funny to imagine the baby whispering that.

<img src="{{ url_for('static', filename='img/never-see/sd/dad.jpg') }}"
     alt="A man holding his baby. A yelloe-coloured caption contains nonsense letters that vaguely resemble the prompt."
     class="centered">

*You never hear someone say "Do what you want to the girl, but leave me alone".* It seems that SD didn't understand the context of this prompt.

<img src="{{ url_for('static', filename='img/never-see/sd/do-what-you-want.jpg') }}"
     alt="Nonsense text in Impact font, vaguely resembling the prompt. And the back of someone's head."
     class="centered">

*You never hear someone say "As soon as I put this hot poker in my ass I'm going to chop my dick off".* The guy looks appropriately serious.

<img src="{{ url_for('static', filename='img/never-see/sd/hot-poker.jpg') }}"
     alt="A man wearing a green bandana, a shirt, and a gold bracelet that looks obviously Photoshopped. He has stubble, his face is a bit flushed, and he has an intense expression on his face. He's holding long object that might be a poker, it weirdly disappears into his right nostril. He might also be holding a knife in the same hand."
     class="centered">

*You never hear someone say "Honey, let's sell the children, move to Zanzibar, and begin taking opium rectally".* Hm, is this what Zanzibar looks like?

<img src="{{ url_for('static', filename='img/never-see/sd/honey-lets-sell.jpg') }}"
     alt="A Black family sitting on a beach under some palm trees, their faces are quite messed up and their bodies are disconnected / intersecting with the trees."
     class="centered">

*You never hear someone say "Mom, I've got a big date tonight, can I borrow a French tickler from you?".* The disembodied hand is quite funny.

<img src="{{ url_for('static', filename='img/never-see/sd/french-tickler.jpg') }}"
     alt="A woman in a skimpy pink dress sitting on a bed, she's holding a disembodied hand and kissing the thumb, also holding a knife-like object and a metal rod."
     class="centered">

*You don't want to come home and hear "Honey, remember how we told the children never to play on the railroad tracks?".* These children are creepy as hell.

<img src="{{ url_for('static', filename='img/never-see/sd/railroad-tracks.jpg') }}"
     alt="Demon children looking happy and sitting on railroad tracks."
     class="centered">

*You don't want to be sitting in your doctor's office and hear "Well Jim, there's no reason why you shouldn't live another 20 to 30 years. However, you will be bleeding constantly from both eyes".* I could probably pick a better prompt to show someone bleeding from their eyes, but I don't particularly feel like doing that.

<img src="{{ url_for('static', filename='img/never-see/sd/eyebleed.jpg') }}"
     alt="A serious-looking balding man, lots of nonsensical meme letters."
     class="centered">

*You don't want to hear "I'M PREGNANT, YOU'RE THE FATHER, AND I'M GOING TO KILL ALL 3 OF US!"* Liking the intensity with this one.

<img src="{{ url_for('static', filename='img/never-see/sd/pregnant.jpg') }}"
     alt="An intense, angry-looking woman whose mouth is wide open. There are weird black holes in her eyes, nostrils and mouth. Also, once again, there's nonsensical meme text."
     class="centered">

*You don't want to hear "Honey, it's the police. They have a search warrant and the 300 kilos of cocaine are still sitting out in the living room."* This is quite good, actually.

<img src="{{ url_for('static', filename='img/never-see/sd/honey-police.jpg') }}"
     alt="Police officers kneeling down and inspecting a bunch of bags of white powder, scattered across a wooden floor. A TV with a cracked screen is in the background. The officers are missing their eyes and their faces are all blurred / contorted."
     class="centered">

*You don't want to hear your fiancé say "I'll be right back, I've gotta take a dump" while the two of you are having dinner together with your parents for the first time.* I don't know what the hell this is.

<img src="{{ url_for('static', filename='img/never-see/sd/dump.jpg') }}"
     alt="That Willy Wonka meme where he's leaning on his elbow, except someone else is badly superimposed over him. Again, there's meme text."
     class="centered">

### Conclusions
Playing around with SD has made me appreciate the abilities and limitations of current image AIs. As far as I can tell, it takes a lot of thought and a lot of hardware power to get satisfactory results. Regarding the George Carlin prompts, I don't think they were explicit enough to generate the images I was hoping for, particularly the ones in the "things you don't hear" category. SD doesn't seem to be smart enough to pick up on the context of dialogue, and requires a direct description. Also, for some reason, putting the prompt in quotes tends to result in the addition of meme text.

That's all for now! If I were to continue experimenting with SD, then I would try out the many tools and interfaces that people have built around it to help generate better images. There's also the option to base the output on a source image.
