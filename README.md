# jamesm21-portfolio
In this repository are my 6008CEM code portfolio and logs of LLM usage.
Generative AI (ChatGPT's free version) was used to help me develop my understanding of concepts in Haskell (functional programming) Prolog (logic programming) and Z3 using its Python API (constraint programming),
as well as to generate code, ideas, implementation details, and documentation. I reviewed the output from the LLM to ensure I understood it to the best of my ability, and sometimes used its output verbatim. 
The tasks selected were respectively:
Convert a Markup Language to HTML
Expert System
University Modules Allocation

Known issues for Convert a Markup Language to HTML:
- The scanner does not generate tokens in a way that currently allows for well formed HTML to be generated: in general but especially for <p> tags the place the </p> tag should be inserted is not recorded.
- Instead the HTML renderer relies upon the insertion of a "TEXT" token to indicate no HTML tags should be inserted and simply does not close the <p> tag.
- While this approach works for the spec given to ensure a closing </p> tag is not inserted in the wrong place resulting in a newline being rendered by a browser in the wrong place, I am aware this is not well formed HTML.
- I have been unable to implement the functionality to insert a space " " when a newline character is inserted in the paragraph. Leaving a trailing space before a newline would also not work as a workaround for the user as whitespace removal functions would not include this in the string attached to the TEXT in the sum Token type. Instead of this a user could keep the text within a paragraph tag on one line.
- Similar issues would be encountered on header tags, however the spec provided does not include a newline in a header tag.
- The HTML output (not the rendered output in a browser) is also all on one line and unindented.
- A workaround for including bold text in the middle of the line only works for <p> tags. While the spec only requires bold text inside a <p> tag I would consider it a limitation of the approach taken. 

Next steps:
- Change the way in which paragraph and header tags are found in order to generate a token for the end of a paragraph or header.
- Change the way in which bold text is rendered by renderHTML() to handle the general case of inline bolding instead of just handling it inside paragraph tags. Perhaps store and send some state when recursively calling this function such as a list of variables for tags simliar to bold to match the closing characters (this would not be necessary for bolding text, but for example if a user wanted a list within a list, the renderer would have to keep track of how many lists deep the user is, in a similar way to how compilers for programming languages like C or Python match parentheses using a stack).
- Adding support to escape characters would be nice to have.

Known issues for Expert System:
- The rules and data for students are stored in source code Prolog facts instead of a compiled format, wasting space and processing time.
- When users enter data using the SWI Prolog REPL, the data entered does not persist when the REPL is exited.

Next steps:
- Convert both existing data and data entered through the REPL into a .qpl format binary using qcompile() predicate or similar, or for scalability store it in a relational database.
