# sys::dirs

The `sys::dirs` module provides platform-aware paths to standard
directories (home, config, data, etc.). Functions return `null` on
platforms where the directory does not apply.

```graphix
val home_dir: fn() -> [string, null];
val cache_dir: fn() -> [string, null];
val config_dir: fn() -> [string, null];
val config_local_dir: fn() -> [string, null];
val data_dir: fn() -> [string, null];
val data_local_dir: fn() -> [string, null];
val executable_dir: fn() -> [string, null];
val preference_dir: fn() -> [string, null];
val runtime_dir: fn() -> [string, null];
val state_dir: fn() -> [string, null];
val audio_dir: fn() -> [string, null];
val desktop_dir: fn() -> [string, null];
val document_dir: fn() -> [string, null];
val download_dir: fn() -> [string, null];
val font_dir: fn() -> [string, null];
val picture_dir: fn() -> [string, null];
val public_dir: fn() -> [string, null];
val template_dir: fn() -> [string, null];
val video_dir: fn() -> [string, null];
```

`executable_dir`, `runtime_dir`, and `state_dir` are Linux-only and
return `null` on other platforms.
