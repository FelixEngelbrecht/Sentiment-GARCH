import pandas as pd


def main():
    df = pd.read_csv('your-file-name.csv', sep=',')
    df['Date'] = pd.to_datetime(df['Date'])
    df['Date'] = df['Date'].dt.date

    daily_avg_ecb = df.groupby('Date')['Sentiment'].mean()
    print(daily_avg_ecb)
    daily_avg_ecb.to_csv("./another-file-name.csv", sep=',', index=True)


if __name__ == "__main__":
    main()
