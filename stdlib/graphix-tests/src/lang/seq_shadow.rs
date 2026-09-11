use super::dense_deltas::{as_i64s, run_delta};
use anyhow::{Context, Result};
use arcstr::format;

async fn check(body: &str, expected: &[i64], fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        let requests =
            if form == "seq" { "1 => 1, 25 => 2, 50 => 3" } else { "1 | 2 | 3 => step" };
        let code = format!(
            r#"{{
                let step = 0;
                step <- select step {{ n if n < 75 => n + 1, _ => never() }};
                let request = select step {{ {requests}, _ => never() }};
                {form} request {{ {body} }}
            }}"#
        );
        let (values, _) =
            run_delta(&code, fusion_disabled).await.with_context(|| code.clone())?;
        assert_eq!(as_i64s(&values), expected, "{code}");
    }
    Ok(())
}

async fn references(fusion_disabled: bool) -> Result<()> {
    for body in [
        "let x = request; let a = &x; let x = 99; *a",
        "let x = request; let a = &x; { let x = 99; *a }",
        "{ let x = request; let a = &x; let x = 99; *a }",
        "let x = request; let a = &x; { let x = 99; x }; *a",
    ] {
        check(body, &[1, 2, 3], fusion_disabled).await?;
    }
    Ok(())
}

async fn type_changes(fusion_disabled: bool) -> Result<()> {
    for body in [
        r#"let x = request; let a = &x; let x = "later"; *a"#,
        r#"{ let x = request; let a = &x; let x = "later"; *a }"#,
    ] {
        check(body, &[1, 2, 3], fusion_disabled).await?;
    }
    Ok(())
}

async fn closures(fusion_disabled: bool) -> Result<()> {
    for body in [
        "let x = request; let f = |v| v + x; let x = 99; f(0)",
        "{ let x = request; let f = |v| v + x; let x = 99; f(0) }",
    ] {
        check(body, &[1, 2, 3], fusion_disabled).await?;
    }
    Ok(())
}

async fn initializers_and_patterns(fusion_disabled: bool) -> Result<()> {
    for body in [
        "let x = request; let a = &x; let x = x + 10; x - *a",
        "{ let x = request; let a = &x; let x = x + 10; x - *a }",
        "let (x, y) = (request, 10); let a = &x; let (x, y) = (y, x); x + y - *a",
        "{ let {x, y} = {x: request, y: 10}; let a = &x; \
         let {x, y} = {x: y, y: x}; x + y - *a }",
    ] {
        check(body, &[10, 10, 10], fusion_disabled).await?;
    }
    Ok(())
}

async fn writes(fusion_disabled: bool) -> Result<()> {
    for body in ["let x = request; let a = &x; let x = 99; \
         *a <- *a + 10; x <- x + 1; *a"]
    {
        check(body, &[11, 12, 13], fusion_disabled).await?;
    }
    Ok(())
}

async fn trigger_shadowing(fusion_disabled: bool) -> Result<()> {
    for body in [
        "let initial = request; let request = request + 10; request - initial",
        "{ let initial = request; let request = request + 10; request - initial }",
    ] {
        check(body, &[10, 10, 10], fusion_disabled).await?;
    }
    Ok(())
}

async fn sampled_closure(fusion_disabled: bool) -> Result<()> {
    check("let x = request; let f = |v| v ~ x; f(request)", &[1, 2, 3], fusion_disabled)
        .await
}

macro_rules! modes {
    ($($test:ident),+ $(,)?) => {$ (
        mod $test {
            use super::*;

            #[tokio::test(flavor = "current_thread")]
            async fn interp() -> Result<()> { super::$test(true).await }

            #[tokio::test(flavor = "current_thread")]
            async fn jit() -> Result<()> { super::$test(false).await }
        }
    )+};
}

modes!(
    references,
    type_changes,
    closures,
    initializers_and_patterns,
    writes,
    trigger_shadowing,
    sampled_closure
);
