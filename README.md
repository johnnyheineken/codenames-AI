# codenames AI
Do you know codenames?
It is a board game, where you are trying to connect words with a hint. Great fun for the party!

However, I decided to invent AI which could provide such hints, using Word2vec.

First, I had downloaded wikipedia articles, and I also tried reddit comments.
I have processed them using the word2vec algorithm.
Then, I used cosine distance to calculate the most similar words.
In the end, I simulated several games and evaluated, whether the hints have any good use.


And I found out that it works! For more detailed description, see the pdf.
