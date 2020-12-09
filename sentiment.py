from textblob import TextBlob
from textblob.sentiments import NaiveBayesAnalyzer
import pandas as pd
import re
import pycld2 as cld2


def Average(lst):
    return sum(lst) / len(lst)


def main():
    df = pd.read_csv('file-name.csv', sep=',')
    text = df['Tweet']

    sentiment = []
    result = []
    counter = 0

    for elem in text:
        sentiment_temp = []
        # exclude non-english languages
        tuple = cld2.detect(elem[:100])
        if tuple[2][0][0] != 'ENGLISH':
            print("invalid language: " + tuple[2][0][0])
            sentiment_temp.append(0)
            counter += 1
        # toker.tokenize(elem)
        # blob = TextBlob(elem, analyzer=NaiveBayesAnalyzer())
        blob = TextBlob(elem)
        for sentence in blob.sentences:
            elem = re.sub('http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+#]|[!*\(\),]|'
                          '(?:%[0-9a-fA-F][0-9a-fA-F]))+', '', elem)
            elem = re.sub("(@[A-Za-z0-9_]+)", "", elem)
            print(elem)
            # polarity only works without naivebayes
            sentiment_temp.append(sentence.sentiment.polarity)
        # calc avg sentiment per elem/tweet
        sentiment = Average(sentiment_temp)

        result.append(sentiment)

    print(result)
    print(str(counter) + ' zero values')

    df['Sentiment'] = result
    df.to_csv("./your-file-name.csv", sep=',', index=False)


if __name__ == "__main__":
    main()
