# blizzard-html

## Introduction

*blizzard-html* is a library for Haskell that can be used to dynamically compose HTML documents natively in Haskell, without relying on
templating engines or other techniques that can be error-prone and difficult to maintain. It is best suited to be used in multipage web
applications and has first-class support for internationalization. It aims to provide an ideal means of developing and scaling front-end
applications via Locality of Behavior (LoB).

## Structure

*blizzard-html* consists of the following modules:

1. `Html` - Provides a set of types, classes and functions for generating HTML elements.
2. `Html.Attributes` - Provides a set of functions for generating HTML attributes.
3. `Html.Events` - Provides a set of functions for generating HTML event handlers.
4. `Html.Intl` - Provides a set of functions for internationalization in HTML.

## Internationalization

The `Html` module exposes the type class `Translatable` upon which the `Html` data type depends. If a document is intended to be
monolingual, it is sufficient to simply pass in text as in the first three examples. However, if a document is intended to be multilingual,
then a data type that is an instance of `Translatable` must first be declared as demonstrated in the fourth example. This enables the user
to explicitly specify which languages the document supports and ensures via type-checking that there are no missing translations.
Furthermore, `defaultLanguage` ensures that the HTML can always be generated even without the `translate` function.

## Examples

*Note: All HTML examples are formatted neatly for readability but are condensed in practice.*

### Html

*Note: This example assumes the following import.*

```haskell
import qualified Html
```

Input:
```haskell
Html.doctype
    [ Html.html []
        [ Html.head []
            [ Html.title []
                [ "My Website" ]
            ]
        , Html.body []
            [ Html.h1 []
                [ "Welcome to My Website" ]
            , Html.p []
                [ "This is a simple example of an HTML website." ]
            , Html.ul []
                [ Html.li []
                    [ "Item 1" ]
                , Html.li []
                    [ "Item 2" ]
                , Html.li []
                    [ "Item 3" ]
                ]
            , Html.p []
                [ "Thank you for visiting!" ]
            ]
        ]
    ]
```

Output:
```html
<!DOCTYPE html>
<html>
    <head>
        <title>My Website</title>
    </head>
    <body>
        <h1>Welcome to My Website</h1>
        <p>This is a simple example of an HTML website.</p>
        <ul>
            <li>Item 1</li>
            <li>Item 2</li>
            <li>Item 3</li>
        </ul>
        <p>Thank you for visiting!</p>
    </body>
</html>
```

### Html.Attributes

*Note: This example assumes the following imports.*

```haskell
import qualified Html
import qualified Html.Attributes as Attr
```

Input:
```haskell
Html.doctype
    [ Html.html []
        [ Html.head []
            [ Html.title []
                [ "My Website" ]
            ]
        , Html.body []
            [ Html.h1 []
                [ "Welcome to My Website" ]
            , Html.p []
                [ "This is a simple example of an HTML website." ]
            , Html.a
                [ Attr.href "https://www.example.com"
                , Attr.target "_blank"
                ]
                [ "Visit example.com" ]
            , Html.img
                [ Attr.alt "Example Image"
                , Attr.src "example.jpg"
                ]
            , Html.input
                [ Attr.placeholder "Example Input"
                , Attr.type_ "text"
                ]
            , Html.button
                [ Attr.type_ "submit" ]
                [ "Submit" ]
            ]
        ]
    ]
```

Output:
```html
<!DOCTYPE html>
<html>
    <head>
        <title>My Website</title>
    </head>
    <body>
        <h1>Welcome to My Website</h1>
        <p>This is a simple example of an HTML website.</p>
        <a href="https://www.example.com" target="_blank">Visit example.com</a>
        <img src="example.jpg" alt="Example Image">
        <input type="text" placeholder="Enter your name">
        <button type="submit">Submit</button>
    </body>
</html>
```

### Html.Events

*Note: This example assumes the following imports.*

```haskell
import qualified Html
import qualified Html.Events as Evnt
```

Input:
```haskell
Html.doctype
    [ Html.html []
        [ Html.head []
            [ Html.title []
                [ "My Website" ]
            , Html.script []
                [ "function showMessage() { alert('Button clicked!'); }" ]
            ]
        , Html.body []
            [ Html.h1 []
                [ "Welcome to My Website" ]
            , Html.p []
                [ "Click the button below to trigger an event:" ]
            , Html.button
                [ Evnt.onclick "showMessage()" ]
                [ "Click Me" ]
            ]
        ]
    ]
```

Output:
```html
<!DOCTYPE html>
<html>
    <head>
        <title>My Website</title>
        <script>
            function showMessage() {
                alert('Button clicked!');
            }
        </script>
    </head>
    <body>
        <h1>Welcome to My Website</h1>
        <p>Click the button below to trigger an event:</p>
        <button onclick="showMessage()">Click Me</button>
    </body>
</html>
```

### Html.Intl

*Note: This example assumes the following imports and declarations.*

```haskell
import Data.Text.Lazy.Builder (Builder)
import Html (Translatable)
import Html.Intl (translate)

import qualified Html
import qualified Html.Intl as Html


data Intl = Intl
    { de :: Builder
    , en :: Builder
    }


instance Translatable Intl where
    defaultLanguage = en
```

Input:
```haskell
translate de $ Html.doctype
    [ Html.html []
        [ Html.head []
            [ Html.title []
                [ Html.intl Intl
                    { de = "Meine Webseite"
                    , en = "My Website"
                    }
                ]
            ]
        , Html.body []
            [ Html.h1 []
                [ Html.intl Intl
                    { de = "Willkommen auf meiner Webseite"
                    , en = "Welcome to My Website"
                    }
                ]
            , Html.p []
                [ Html.intl Intl
                    { de = "Das ist ein einfaches Beispiel einer HTML-Webseite."
                    , en = "This is a simple example of an HTML website."
                    }
                ]
            , Html.ul []
                [ Html.li []
                    [ "Item 1" ]
                , Html.li []
                    [ "Item 2" ]
                , Html.li []
                    [ "Item 3" ]
                ]
            , Html.p []
                [ Html.intl Intl
                    { de = "Vielen Dank für Ihren Besuch!"
                    , en = "Thank you for visiting!"
                    }
                ]
            ]
        ]
    ]
```

Output:
```html
<!DOCTYPE html>
<html>
    <head>
        <title>Meine Webseite</title>
    </head>
    <body>
        <h1>Willkommen auf meiner Webseite</h1>
        <p>Das ist ein einfaches Beispiel einer HTML-Webseite.</p>
        <ul>
            <li>Item 1</li>
            <li>Item 2</li>
            <li>Item 3</li>
        </ul>
        <p>Vielen Dank für Ihren Besuch!</p>
    </body>
</html>
```

## FAQ

### Why did you create this monstrosity?

Actually, I've spent quite a bit of time over the past few years, sort of on and off, researching different ways to structure a front-end
application in a scalable way. I've tried many different things both professionally and in my free time from making simple game engines in C
to developing e-learning applications in vanilla JavaScript, React and Elm to writing business software in Dart and Flutter. What, in my
opinion, tends to make scaling front-end applications difficult is the way in which people deal with separation of concerns. This is a very
deep topic, but to keep myself from turning this README into a book, let's just say I like to keep related things together. This includes
markup, styling, internationalization, behavior etc. Let me give you a simple example.

Let's say we want to make our website multilingual. What a lot of people do is use something like i18n where all the text is stored in some
central file. You then end up naming each piece of text. If the text is short and simple, then you can have something like this:

```
en:
    submit: 'Submit'

de:
    submit: 'Bestätigen'
```

And for simple, frequently used text, this is mostly fine. One issue with this kind of global text, however, is that sometimes you want
different translations for a particular language. Perhaps for a particular button, we don't want the English version to be 'Submit', but
rather 'Confirm'. We want the German version to be the same in both cases. So now we end up with this:

```
en:
    submit_button_on_form: 'Confirm'
    global_submit_button: 'Submit'

de:
    submit_button_on_form: 'Bestätigen'
    global_submit_button: 'Bestätigen'
```

As the application grows and becomes more complicated, the translations become more and more fragmented and it becomes difficult to know
which components in the app will be affected if I change this line. Usually you end up having to do a search and then manually confirm each
change. There was actually a post on Hacker News not long ago about how Microsoft mistranslated "Zip" for "Postal Code" in the context of
zipping a file. This stuff is hard.

Another issue is that you often end up with longer text that is much more difficult to name like this:

```
en:
    warning2: 'Please note that you are legally required to take a break for at least 15 minutes every 4 hours of work.'

de:
    warning2: 'Bitte beachten Sie, dass Sie verpflichtet sind, alle 4 Stunden eine Pause von mindestens 15 Minuten zu machen.'
```

*warning2* is pretty vague here and if poorly named, can also be very misleading. It says it's a warning, but in the app I see red text and
an error symbol. This isn't a warning at all. Damn junior developers. Well, it can be a misunderstanding from a junior developer, but naming
stuff is a language and communication problem in general. And again, these differences in naming, understanding and communicating often
result in fragmentations that can go unnoticed and make their way into production.

What helps, although it's certainly not a solution to the limitations of language and communication, is putting stuff where it belongs. If
you have a component or some markup, put the translation on that component. If you make any changes, then you'll immediately see exactly
which component it will affect. If that component is being used in multiple places, then you can do a direct search without first having to
map the translation to the key to the component or components.

I feel the same thing happens with CSS and that's why people are moving toward something like Tailwind CSS.

### Why not use blaze-html? This is practically a carbon copy! You just changed blaze to blizzard you damn thief!

First of all, let me just say that *blaze-html* is great. I doubt my Haskell skills will ever be good enough to write something like that.
The problem I have with it though is not related to HTML at all really, but rather CSS. Let me explain...

So if you try putting stuff where it belongs as I've been saying one should do, then you end up with something like this:

```html
<my-component class="border-bottom-10 padding-top-15 clickable some-really-long-class-name-that-a-junior-dev-named-omfg"></my-component>
```

This scales horizontally in a very awkward way. I find this very difficult to parse visually. I've come to really appreciate how this is
handled in *elm-css* which would look more like this (not actual Elm code):

```haskell
Html.myComponent
    [ Attr.css
        [ borderBottom <| px 10
        , paddingTop <| px 10
        , clickable
        , someReallyLongClassNameThatAJuniorDevNamedOmfg
        ]
    ]
    []
```

*blaze-html* wasn't really designed to work this way with CSS. There are ways of doing it, of course, and originally *blizzard-html* was
just a wrapper around *blaze-markup* and *clay* that kind of enabled you to do that, but I wasn't really happy with the result. So I started
experimenting and came up with this.

The *blizzard* part of the name just comes from the fact that winter has always been my favorite season.

### So if you love Elm so much, why don't you marry her?

Two reasons, well three actually: 1) I'm already married 2) Internationalization and 3) I'm not a fan of SPAs anymore. Don't get me wrong, I
really do like Elm. However, I don't like the way that internationalization is encouraged in Elm. The only way to really do it is to
propagate the language down a bunch of function calls to markup. It's one teeny tiny piece of state that you have to pass around everywhere.
And I mean f-ing everywhere. I understand why, but come on, there are better ways to do it, like in *blizzard-html*.

I guess this brings me to the third point. Yeah, SPAs can certainly be useful. But for most of the stuff I make, it's just not necessary.
Most people just want a mostly static site with some forms. That's it. Elm is overkill for that sort of thing. Haskell probably is too.
*insert reason for using Haskell later*

### Builder? I hardly know her!

So, why am I using *Builder* everywhere? Probably because I'm an idiot. Almost certainly. In fact, if that's not the case then I'll be
utterly shocked. Anyway, my reasoning is this...

I don't have much experience with lazy evaluation, but from what I've understood, *Builder* works by calculating the length of all the
strings and doing a single *malloc* call (under the hood) when concatenating them. In my experience with C, *malloc* calls are expensive so
ideally you want to reduce the number of them (batching) as much as possible to optimize performance. I don't know the internals of GHC but
I heard strict *Text* doesn't batch the *malloc*s. If you're using *blizzard-html* for static site generation, I doubt performance would
really matter unless your site has millions of lines of code. If you're using it dynamically with some backend like IHP or Scotty, then
batching sounds more efficient to me, especially if you're fetching stuff and injecting it into the HTML/CSS in which case it would be
impossible for the compiler to know the size ahead of time. If I'm wrong about that, please let me know and also slap me when you see me.

On a more serious note, if anyone wants to do some benchmarking, I'd be very interested in the results. My priority right now though is
ergonomics, not performance.

### *blizzard-html* only depends on *base* and *text*? Are you some kinda minimalist?

Absolutely. In fact, I think one of the biggest reasons people have for avoiding Haskell is that they hear about how Haskell is constantly
introducing breaking changes. I'm not saying this is good or bad, but by keeping the number of dependencies as small as possible, that
decreases the likelihood that future breaking changes will impact *blizzard-html*.

### What's next? *blizzard-markup*?

I'm currently working on *blizzard-css*. I think I've finished most of the challenging stuff. I just need to finish the tedious stuff now.
It's a lot of work and probably won't be done until next year though. After that, I'll make *blizzard-htmx*. That should go significantly
faster. Then I plan on making *blizzard-static* which will be a kind of static site generator. Then I plan on rebuilding one of my client's
sites with it. That should give me a good idea as to how everything fits together and if I should make any changes. After that, I'll look
into some kind of JavaScript solution. I honestly have no idea what I'm gonna do there. I could use Haskell templates, Haskell to JavaScript
or Hyperscript just to name a few options. I'm gonna have to do some research to find out what works best. Could take me a day or years.

### I thought you like keeping related stuff together. Why did you put everything into separate packages, you hypocrite?

I'd like people to be able to use the packages as they see fit. If you wanna use *Tailwind CSS*, then just import *blizzard-html*. There's
no point in importing *blizzard-css* if you're not gonna use it.

### Why so hard on yourself with these questions?

I think there is so much hate and anger online (and in the world in general). By being highly critical of myself, I make myself immune to
any hatred or anger coming from others.
