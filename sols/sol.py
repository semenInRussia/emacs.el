import aiohttp
import asyncio

API_KEY = "U7w70mAydaR0y1hFgtuctRxjtCHJTKXsib1428tzyJZG6XEG05qBwJlCQoBw"
BOT_ID = "154339"
BASE_URL = "https://app.leadteh.ru/api/v1/"

async def do_leadtex_query(method: str, data: dict) -> dict:
    async with aiohttp.ClientSession() as session:
        async with session.get('https://app.leadteh.ru/api/v1/') as resp:
            print(resp.status)
            print(await resp.text())
