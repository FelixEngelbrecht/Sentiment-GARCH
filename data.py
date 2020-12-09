import tweepy
import pandas as pd


def main():
    consumer_key = ""
    consumer_secret = ""
    access_token = ""
    access_token_secret = ""

    auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)

    api = tweepy.API(auth)
    fullText = []
    createdAt = []
    screenName = []
    for status in tweepy.Cursor(api.user_timeline, screen_name='@ecb', tweet_mode="extended").items(5):
        fullText.append(status.full_text)
        createdAt.append(status.created_at)
        screenName.append('ecb')

    df = pd.DataFrame(
        data={"Tweet": fullText, "Date": createdAt, "User": screenName})
    df.to_csv("./file-name.csv", sep=',', index=False)


if __name__ == "__main__":
    main()
