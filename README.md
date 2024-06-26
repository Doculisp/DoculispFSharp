<!-- Generated Document do not edit! -->

# Doculisp #

1. [Section: Basic Structure](#basic_structure)
2. [Section: Doculisp Master Block](#doculisp_master_block)
3. [Section: Section Meta Block](#section_meta_block)
4. [Section: Content Block](#content_block)
5. [Section: Dynamic Headings](#dynamic_headings)
6. [Section: Comment Block](#comment_block)
7. [Section: Key Atoms by Depth](#key_atoms_by_depth)

## Basic Structure ##

The basic structure of Doculisp is all code is contained within blocks. A block is constructed within an HTML comment region. It starts with an open parentheses `(` followed by a sting of non-whitespace characters. This is called an atom. It then has 1 of three possibilities. It can have a parameter, a new block, or nothing. All blocks must close with a close parentheses `)`.

Even the Doculisp main block follows this.

Example

```markdown
<!--
(dl
    (section-meta
        (title Basic Structure)
    )
)
-->
```

The first block is the `dl` block. In it `dl` is the atom. It contains the `section-meta` sub-block.  That block has the atom `section-meta` followed by a further sub block. The last sub block is the `title` sub block. In it `title` is the atom and `Basic Structure` is the parameter.

### Parameter ###

A parameter is a string of characters that contains no line advancement (`\r` or `\n`) character and no parentheses (unless escaped). A parameter has a max length of 255 characters.

### Visual Explanation ###

```doculisp
(atom)
(atom parameter)
(atom (atom2))
(atom (atom2 second parameter))
```

## Doculisp Master Block ##

All doculisp is contained in an outer doculisp block. That block starts with `(dl` followed by a white space. This doculisp block must be contained in an html comment.

Example

```markdown
<!--
(dl
    (section-meta
        (title Doculisp)
        (external
            (section ./doculisp.md)
        )
    )

    (content (toc numbered-labled))
)
-->
```

You will notice that in this example the doculisp contains 2 main blocks, however it is valid for the Doculisp block to contain more or less. Even zero other blocks.

## Section Meta Block ##

This block is contained directly within the Doculisp main block. It contains information used to build the current secion. This includes things like title, subtitle, and external.

Example

```doculisp
(section-meta
    (title Doculisp a short description)
    (external
        (section ./doculisp.md)
    )
)
```

### Title (required) ###

This is the only sub block required for the `section-meta` block. The title is followed by a title that ends at a `)`. Every thing following the white space after the word title and until a new line or a close parenthesis is the title.

### Link ###

This is the first optional sub block for the `section-meta` block. The link allows you to take over the link to use in the table of contents. Its main purpose is to handle characters in the title that markdown does not include in its section headers. This does not change the section link, but lets you specify a different link to use instead.

Example

```doculisp
(section-meta
    (title Doculisp is awesome ✨)
    (link doculisp_is_awesome_)
)
```

### Subtitle ###

Subtitle creates a heading that is two levels of heading less then the title directly beneath the title.

### External ###

This allows you to break each section up into sub-sections that are composed in seperate files. This allows you to limit the scope of work in each file making it easier to find where you need to edit and focus on a single idea.

Example

```doculisp
(section-meta
    (title Doculisp a short description)
    (external
        (section ./doculisp.md)
        (section ./section-meta.md)
    )
)
```

#### Sub-Sections ####

The `external` block is composed of sub-section blocks. These blocks are different then other doculisp blocks. They are custom named blocks. Which means the name of each block is decided by the programmer the same way a varible name is. The format of these blocks is `(` followed by a name followed by whitespace. After the white space is the file path that leads to the document containing the information on how to build the sub-section. Followed again by an optional new line and whitespace. Ending in `)`.

You can add a space (` `) to a name by adding a `-` to the name.

Example

```doculisp
(external
    (chapter
        ./information/one.md
    )
)
```

This will create a sub-section called `chapter` that is built using the file `./information/one.md`.

Example

```doculisp
(external
    (sub-section ./one.md)
)
```

This will create a subsection called `sub section` that is built using the file `./one.md`.

```doculisp
(external (section ./two.md))
```

This will create a subsection called `section` that is built using the file `./two.md`.

### Exception to the Rule ###

Comment block breaks this rule slightly. The astrict character is a special character that cause all atoms that start with to be treated as a comment, and all parameters and sub blocks to be ignored.

## Content Block ##

The content block signifies where to build the document from the external documents. This block has only one optional subblock.

### Table of Contents ###

The only subblock to the content block is the table of contents. This will cause a linked table of contents to appear for the section at its location.

The structure of the table of contents is `(toc)` there is an optional parameter that can have one of the following values:

* no-table
* unlabeled
* labeled
* numbered
* numbered-labeled
* bulleted
* bulleted-labeled

Examples

```doculisp
(contents (toc))
```

```doculisp
(contents (toc bulleted-labeled))
```

Any of the options with `labled` on it will use the name of the subsection. 

```doculisp
(dl
    (file-meta
        (title Some Document)
        (external
            (chapter ./first) (*The title of this document is "An introduction")
        )
    )

    (content (toc bulleted-labeled))
)
```

The table of contents would contain a line that looked like:

```markdown
* Chapter: [An Introduction](#an-introduction)
```

## Dynamic Headings ##

Sometimes you want to create structure aware headings such that only heading of lesser importance are under your title. this is accomplished by the `(#` block, or dynamic headinlg block.

The dynamic heading block works differently then other blocks. The number of `#` signs determins how far it is beneith the current sub/section heading.

Example

```markdown
<!--
(dl
    (section-meta
        (title Maths an intro)
        (external
            (section ./add.md)
            (section ./subtract.md)
            (section ./muliply.md)
            (section ./divide.md)
        )
    )
)
-->

<!-- (# Summary) -->

A cool summary of maths.

<!-- (## Resons For Document) -->

An explination why to have the document.


<!-- (dl (content (toc unlabled))) -->
```

If this example was the top level document, then the title of the document, Heading 1, would be "Maths an intro". "Summray" would then be created as a Heading 2, and  "Resons For Document" as Heading 3.

However if this document reprended a subsection directly under the Title, then "Maths an intro" would be Heading 2, "Summary" heading 3, and "Reasons For Document" would be Heading 4.

Currently, the maximum heading depth recognized by Markdown is H6. However Doculisp will not restrict you to that depth. If the total depth is more then H6 you may get unexpected results.

## Comment Block ##

The comment block is the only block that can be present at all levels within the Doculisp Main Block. The comment is created by adding an astrics `*` just after an open parenthisis and end when the block and all its subblocks are closed.

Example:

```markdown
<!--
(dl
    (*section-meta
        (title Doculisp)
        (external
            (section ./doculisp.md)
            (section ./section-meta.md)
            (section ./content.md)
            (section ./comment.md)
        )
    )
)
-->
```

In this example the `section-meta` block and all of its subblocks are commented out. Comments can also be nested. This allows you to uncomment in peices.

xample:

```markdown
<!--
(dl
    (*section-meta
        (title Doculisp)
        (*external
            (section ./doculisp.md)
            (section ./section-meta.md)
            (section ./content.md)
            (*section ./comment.md)
        )
    )
)
-->
```

### Nested Comments ###

In this example the `section-meta` and all its subblocks are commented out. However when you uncomment `section-meta` then the `external` block will be commented out. When you uncomment that block, then the `section ./comment.md` block will be commented out.

## Key Atoms by Depth ##

Here is a list of all the key atoms by depth:

* markdown
* `dl`
  * `section-meta`
    * `title`
      * `*`
    * `subtitle`
      * `*`
    * `link`
      * `*`
    * `external`
      * `*`
    * `*`
  * `content`
    * `toc`
  * `#`
  * `*`
* `*`

<!-- Generated Document do not edit! -->
