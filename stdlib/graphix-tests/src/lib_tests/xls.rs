use graphix_package_core::run_with_tempdir;

run_with_tempdir! {
    name: xls_sheets,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::sheets(data)$ }}",
    setup: |td| {
        let path = td.path().join("test.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        wb.add_worksheet().set_name("Alpha").unwrap();
        wb.add_worksheet().set_name("Beta").unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        assert_eq!(v.to_string(), r#"["Alpha", "Beta"]"#);
        Ok(())
    }
}

run_with_tempdir! {
    name: xls_read_numbers,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::read(data, \"Sheet1\")$ }}",
    setup: |td| {
        let path = td.path().join("numbers.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        let ws = wb.add_worksheet();
        ws.write(0, 0, 42).unwrap();
        ws.write(0, 1, 3.14).unwrap();
        ws.write(1, 0, -7).unwrap();
        ws.write(1, 1, 0.0).unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        assert_eq!(v.to_string(), r#"[[f64:42., f64:3.14], [f64:-7., f64:0.]]"#);
        Ok(())
    }
}

run_with_tempdir! {
    name: xls_read_strings,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::read(data, \"Sheet1\")$ }}",
    setup: |td| {
        let path = td.path().join("strings.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        let ws = wb.add_worksheet();
        ws.write(0, 0, "hello").unwrap();
        ws.write(0, 1, "world").unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        assert_eq!(v.to_string(), r#"[["hello", "world"]]"#);
        Ok(())
    }
}

run_with_tempdir! {
    name: xls_read_bools,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::read(data, \"Sheet1\")$ }}",
    setup: |td| {
        let path = td.path().join("bools.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        let ws = wb.add_worksheet();
        ws.write(0, 0, true).unwrap();
        ws.write(0, 1, false).unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        assert_eq!(v.to_string(), r#"[[true, false]]"#);
        Ok(())
    }
}

run_with_tempdir! {
    name: xls_read_mixed,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::read(data, \"Sheet1\")$ }}",
    setup: |td| {
        let path = td.path().join("mixed.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        let ws = wb.add_worksheet();
        ws.write(0, 0, 42).unwrap();
        ws.write(0, 1, "text").unwrap();
        ws.write(0, 2, true).unwrap();
        ws.write(0, 3, 2.718).unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        assert_eq!(v.to_string(), r#"[[f64:42., "text", true, f64:2.718]]"#);
        Ok(())
    }
}

run_with_tempdir! {
    name: xls_read_empty_cells,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::read(data, \"Sheet1\")$ }}",
    setup: |td| {
        let path = td.path().join("empty.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        let ws = wb.add_worksheet();
        ws.write(0, 0, "a").unwrap();
        ws.write(0, 2, "c").unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        assert_eq!(v.to_string(), r#"[["a", null, "c"]]"#);
        Ok(())
    }
}

run_with_tempdir! {
    name: xls_read_missing_sheet,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::read(data, \"NoSuchSheet\") }}",
    setup: |td| {
        let path = td.path().join("test.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        wb.add_worksheet();
        wb.save(&path).unwrap();
        path
    },
    expect_error
}

run_with_tempdir! {
    name: xls_invalid,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::sheets(data) }}",
    setup: |td| {
        let path = td.path().join("garbage.xlsx");
        std::fs::write(&path, b"this is not a spreadsheet").unwrap();
        path
    },
    expect_error
}

// A sheet reads as its grid from A1, a cell not given null.
run_with_tempdir! {
    name: xls_read_pads_to_a1,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::read(data, \"Sheet1\")$ }}",
    setup: |td| {
        let path = td.path().join("offset.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        wb.add_worksheet().write(2, 2, 1).unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        assert_eq!(v.to_string(), "[[null, null, null], [null, null, null], [null, null, f64:1.]]");
        Ok(())
    }
}

// One far cell in a tiny file is a grid past the cap: refused, never built.
run_with_tempdir! {
    name: xls_read_refuses_a_far_cell,
    code: "{{ let data = sys::fs::read_all_bin(\"{}\")$; xls::read(data, \"Sheet1\") }}",
    setup: |td| {
        let path = td.path().join("far.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        wb.add_worksheet().write(99_999, 16_383, 1).unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        assert!(v.to_string().contains("XlsErr") && v.to_string().contains("cells"), "{v}");
        Ok(())
    }
}

// A select over the seven kinds a cell can be is exhaustive.
run_with_tempdir! {
    name: xls_cell_kinds_cover_a_cell,
    code: "{{
        let data = sys::fs::read_all_bin(\"{}\")$;
        array::map(xls::read(data, \"Sheet1\")$, |row| array::map(row, |cell| select cell {{
            i64 as _ => \"int\",
            f64 as _ => \"float\",
            string as s => s,
            bool as _ => \"bool\",
            datetime as _ => \"date\",
            duration as _ => \"dur\",
            null as _ => \"empty\"
        }}))
    }}",
    setup: |td| {
        let path = td.path().join("kinds.xlsx");
        let mut wb = rust_xlsxwriter::Workbook::new();
        let ws = wb.add_worksheet();
        ws.write(0, 0, 1.5).unwrap();
        ws.write(0, 1, "text").unwrap();
        ws.write(0, 2, true).unwrap();
        ws.write(0, 4, "end").unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        assert_eq!(v.to_string(), r#"[["float", "text", "bool", "empty", "end"]]"#);
        Ok(())
    }
}
