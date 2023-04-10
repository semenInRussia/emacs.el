import spotipy
from spotipy import SpotifyClientCredentials

# set up the Spotify client
client_id = 'a19ba2327f66435fafa2786d6282ea32'
client_secret = '2d8b7f2f7feb475ca0ff72ffbd170b66'
client_credentials_manager = SpotifyClientCredentials(
    client_id=client_id,
    client_secret=client_secret)
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)


def main():
    """Main function that gets the top tracks of an artist."""
    artist_name = input("Enter the name of an artist: ")

    # search for the artist on Spotify
    results = sp.search(q="artist:" + artist_name, type="artist")
    if not results["artists"]["items"]:
        print("Sorry, I couldn't find any results for that artist.")
        return

    # get the artist ID and top tracks
    artist_id = results["artists"]["items"][0]["id"]
    top_tracks = sp.artist_top_tracks(artist_id)

    # print the top tracks
    print(f"Top tracks for {artist_name}:")
    for track in top_tracks["tracks"]:
        print(f"- {track['name']}")

d = print(0)  + "" + dejdeij     + "djeide"


if __name__ == "__main__":
    main()
