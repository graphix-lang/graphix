use anyhow::{Context, Result};
use arcstr::format;
use graphix_compiler::{
    CFlag, Event, NoUserEvent, Scope, Tag, TagValue, compile,
    expr::{ModPath, parser::parse_one},
};
use graphix_package_core::testing::init_with_flags_and_setup;
use netidx_value::{ValArray, Value};
use tokio::sync::mpsc;

async fn strict_bottom(fusion_disabled: bool) -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let flags =
        if fusion_disabled { CFlag::FusionDisabled.into() } else { Default::default() };
    let ctx = init_with_flags_and_setup(tx, crate::TEST_REGISTER, vec![], flags, |_| {})
        .await?;
    let result = ctx
        .rt
        .with_ctx(move |ctx| -> Result<()> {
            let scope = Scope::root().append("strict_bottom");
            let mut input = compile(
                ctx,
                flags,
                &scope,
                parse_one("let input: (i64, i64) = never()")?,
            )?;
            let id = ctx
                .env
                .lookup_bind(&scope.lexical, &ModPath::from(["input"]))?
                .context("input binding")?
                .1
                .id;
            let value = Value::Array(ValArray::from([Value::I64(17), Value::I64(3)]));
            for (code, expected) in [
                ("input.0", Value::I64(17)),
                ("({x: input.0}).x", Value::I64(17)),
                ("(input.0 + 1) * 2", Value::I64(36)),
                ("input.0 > 0", Value::Bool(true)),
                ("input.0 == 17 && input.1 != 0", Value::Bool(true)),
                ("-input.0", Value::I64(-17)),
                ("!(input.0 == 0)", Value::Bool(true)),
                ("cast<i64>(input.0)?", Value::I64(17)),
                (r#""[input.0]""#, Value::from("17")),
                ("[input.0, input.1][0]?", Value::I64(17)),
                ("{17 => 17}{input.0}?", Value::I64(17)),
            ] {
                let spec = if fusion_disabled {
                    parse_one(code)?
                } else {
                    parse_one(&format!("#[native]\n{code}"))?
                };
                let mut node = compile(ctx, flags, &scope, spec).with_context(|| code)?;
                for sleep in [false, true] {
                    for (i, tag) in [
                        Tag::FIRED,
                        Tag::STALE_BOTTOM,
                        Tag::STALE_BOTTOM,
                        Tag::STALE,
                        Tag::STALE,
                        Tag::FRESH_BOTTOM,
                        Tag::STALE_BOTTOM,
                        Tag::FIRED,
                    ]
                    .into_iter()
                    .enumerate()
                    {
                        if sleep && i == 1 {
                            node.sleep(ctx);
                        }
                        let mut event = Event::new(NoUserEvent);
                        event.init = i == 0;
                        let payload =
                            if tag.is_bottom() { Value::Null } else { value.clone() };
                        event.variables.insert(id, TagValue::tagged(payload, tag));
                        let actual = node.update(ctx, &mut event);
                        assert_eq!(
                            actual.tag(),
                            tag,
                            "{code}, sleep={sleep}, step={i}: {actual:?}"
                        );
                        if !tag.is_bottom() {
                            assert_eq!(
                                actual.value_cloned(),
                                expected,
                                "{code}, sleep={sleep}, step={i}"
                            );
                        }
                    }
                }
                node.delete(ctx);
            }
            let mut unused =
                compile(ctx, flags, &scope, parse_one("let unused: i64 = never()")?)?;
            let unused_id = ctx
                .env
                .lookup_bind(&scope.lexical, &ModPath::from(["unused"]))?
                .context("unused binding")?
                .1
                .id;
            let code = "select input.1 { 3 => input.0, _ => unused }";
            let spec = if fusion_disabled {
                parse_one(code)?
            } else {
                parse_one(&format!("#[native]\n{code}"))?
            };
            let mut node = compile(ctx, flags, &scope, spec)?;
            for (i, tag) in [
                Tag::FIRED,
                Tag::STALE_BOTTOM,
                Tag::FRESH_BOTTOM,
                Tag::STALE_BOTTOM,
                Tag::STALE,
            ]
            .into_iter()
            .enumerate()
            {
                let mut event = Event::new(NoUserEvent);
                event.init = i == 0;
                let expected = if i == 0 { Tag::FIRED } else { Tag::STALE };
                event.variables.insert(id, TagValue::tagged(value.clone(), expected));
                let payload = if tag.is_bottom() { Value::Null } else { Value::I64(9) };
                event.variables.insert(unused_id, TagValue::tagged(payload, tag));
                let actual = node.update(ctx, &mut event);
                assert_eq!(actual.tag(), expected, "unused bottom, step={i}: {actual:?}");
                assert_eq!(actual.value_cloned(), Value::I64(17));
            }
            node.delete(ctx);
            unused.delete(ctx);
            input.delete(ctx);
            Ok(())
        })
        .await?;
    ctx.shutdown().await;
    result
}

#[tokio::test]
async fn strict_bottom_interp() -> Result<()> {
    strict_bottom(true).await
}

#[tokio::test]
async fn strict_bottom_jit() -> Result<()> {
    strict_bottom(false).await
}

async fn strict_sample(fusion_disabled: bool) -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let flags =
        if fusion_disabled { CFlag::FusionDisabled.into() } else { Default::default() };
    let ctx = init_with_flags_and_setup(tx, crate::TEST_REGISTER, vec![], flags, |_| {})
        .await?;
    let result = ctx
        .rt
        .with_ctx(move |ctx| -> Result<()> {
            let scope = Scope::root().append("strict_sample");
            let mut bindings = Vec::new();
            for name in ["clock", "input", "sampled"] {
                let node = compile(
                    ctx,
                    flags,
                    &scope,
                    parse_one(&format!("let {name}: i64 = never()"))?,
                )?;
                let id = ctx
                    .env
                    .lookup_bind(&scope.lexical, &ModPath::from([name]))?
                    .context("test binding")?
                    .1
                    .id;
                bindings.push((node, id));
            }
            let mut sample = compile(ctx, flags, &scope, parse_one("clock ~! input")?)?;
            let consumer =
                if fusion_disabled { "sampled + 1" } else { "#[native]\nsampled + 1" };
            let mut consumer = compile(ctx, flags, &scope, parse_one(consumer)?)?;
            for (i, (clock_tag, input_tag, value, expected_tag, expected_value)) in [
                (Tag::FIRED, Tag::STALE_BOTTOM, 0, Tag::FRESH_BOTTOM, 0),
                (Tag::STALE, Tag::FIRED, 7, Tag::STALE_BOTTOM, 0),
                (Tag::FIRED, Tag::STALE, 7, Tag::FIRED, 7),
                (Tag::STALE, Tag::STALE_BOTTOM, 0, Tag::STALE, 7),
                (Tag::FIRED, Tag::STALE_BOTTOM, 0, Tag::FRESH_BOTTOM, 0),
                (Tag::STALE, Tag::FIRED, 11, Tag::STALE_BOTTOM, 0),
                (Tag::STALE, Tag::STALE, 11, Tag::STALE_BOTTOM, 0),
                (Tag::FIRED, Tag::STALE, 11, Tag::FIRED, 11),
                (Tag::FRESH_BOTTOM, Tag::FIRED, 13, Tag::STALE, 11),
                (Tag::FIRED, Tag::STALE, 13, Tag::FIRED, 13),
            ]
            .into_iter()
            .enumerate()
            {
                let mut event = Event::new(NoUserEvent);
                event.init = i == 0;
                event.variables.insert(
                    bindings[0].1,
                    TagValue::tagged(Value::I64(i as i64), clock_tag),
                );
                event.variables.insert(
                    bindings[1].1,
                    TagValue::tagged(Value::I64(value), input_tag),
                );
                let actual = sample.update(ctx, &mut event).clone();
                assert_eq!(actual.tag(), expected_tag, "sample step {i}: {actual:?}");
                if !expected_tag.is_bottom() {
                    assert_eq!(actual.value_cloned(), Value::I64(expected_value));
                }
                event.variables.insert(bindings[2].1, actual);
                let actual = consumer.update(ctx, &mut event);
                assert_eq!(actual.tag(), expected_tag, "consumer step {i}: {actual:?}");
                if !expected_tag.is_bottom() {
                    assert_eq!(actual.value_cloned(), Value::I64(expected_value + 1));
                }
            }
            consumer.delete(ctx);
            sample.delete(ctx);
            for (mut node, _) in bindings {
                node.delete(ctx);
            }
            Ok(())
        })
        .await?;
    ctx.shutdown().await;
    result
}

#[tokio::test]
async fn strict_sample_interp() -> Result<()> {
    strict_sample(true).await
}

#[tokio::test]
async fn strict_sample_jit() -> Result<()> {
    strict_sample(false).await
}
