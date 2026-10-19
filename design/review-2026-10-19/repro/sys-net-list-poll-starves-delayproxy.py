import asyncio, sys, time
LISTEN, TARGET, DELAY = int(sys.argv[1]), int(sys.argv[2]), float(sys.argv[3])
async def pipe(r, w):
    q = asyncio.Queue()
    async def feed():
        while True:
            d = await r.read(65536)
            await q.put((time.monotonic() + DELAY, d))
            if not d:
                return
    async def drain():
        while True:
            due, d = await q.get()
            now = time.monotonic()
            if due > now:
                await asyncio.sleep(due - now)
            if not d:
                w.close(); return
            w.write(d); await w.drain()
    await asyncio.gather(feed(), drain())
async def handle(cr, cw):
    try:
        tr, tw = await asyncio.open_connection('127.0.0.1', TARGET)
        await asyncio.gather(pipe(cr, tw), pipe(tr, cw))
    except Exception:
        pass
async def main():
    s = await asyncio.start_server(handle, '127.0.0.1', LISTEN)
    async with s:
        await s.serve_forever()
asyncio.run(main())
