# ids::mix (graphix-types/src/ids.rs) leaves an aligned address's low bits zero.
# A multiply only carries entropy upward: (16*a * K) mod 16 == 0 for any K, so
# AddrHasher's hash of a 16-byte-aligned heap address always has its low 4 bits
# clear, and hashbrown's bucket index (hash & bucket_mask) only ever starts a
# probe on every 16th slot. The doc on `mix` says the multiply spreads the
# addresses' bits over the tag AND the bucket index; the test
# `address_keys_spread_over_the_tag` checks only the tag.
#
# Command: python3 -I t-expr-core-mix-low-bits.py
# Expected (per the doc): ~1024 distinct bucket starts for 1024 keys, mask 1023.
# Observed: distinct low-4-bit values [0]; 64 distinct bucket starts.
#   Folding the high half down (m ^ (m >> 32)) gives 628 starts, same 128 tags.
K = 0x9E3779B97F4A7C15
M = (1 << 64) - 1
addrs = [0x5555_5555_0000 + i * 48 for i in range(1024)]
mix = lambda n: (n * K) & M
print("distinct low-4-bit values:", sorted({mix(a) & 15 for a in addrs}))
print("distinct bucket starts (mask 1023):", len({mix(a) & 1023 for a in addrs}))
fold = lambda n: (lambda m: m ^ (m >> 32))(mix(n))
print("folded: bucket starts", len({fold(a) & 1023 for a in addrs}),
      "tags", len({fold(a) >> 57 for a in addrs}), "vs", len({mix(a) >> 57 for a in addrs}))
