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
        if let ::netidx::subscriber::Value::Array(arr) = &v {
            assert_eq!(arr.len(), 2);
            assert!(matches!(&arr[0], ::netidx::subscriber::Value::String(s) if &**s == "Alpha"));
            assert!(matches!(&arr[1], ::netidx::subscriber::Value::String(s) if &**s == "Beta"));
            Ok(())
        } else {
            panic!("expected Array, got: {v:?}")
        }
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
        let arr = match &v {
            ::netidx::subscriber::Value::Array(a) => a,
            _ => panic!("expected Array, got: {v:?}"),
        };
        assert_eq!(arr.len(), 2);
        // row 0
        let row0 = match &arr[0] {
            ::netidx::subscriber::Value::Array(r) => r,
            v => panic!("expected row Array, got: {v:?}"),
        };
        assert_eq!(row0.len(), 2);
        // 42 may come back as Int or Float
        match &row0[0] {
            ::netidx::subscriber::Value::I64(42) => (),
            ::netidx::subscriber::Value::F64(f) if (*f - 42.0).abs() < 1e-10 => (),
            v => panic!("expected 42, got: {v:?}"),
        }
        match &row0[1] {
            ::netidx::subscriber::Value::F64(f) if (*f - 3.14).abs() < 1e-10 => (),
            v => panic!("expected 3.14, got: {v:?}"),
        }
        // row 1
        let row1 = match &arr[1] {
            ::netidx::subscriber::Value::Array(r) => r,
            v => panic!("expected row Array, got: {v:?}"),
        };
        match &row1[0] {
            ::netidx::subscriber::Value::I64(-7) => (),
            ::netidx::subscriber::Value::F64(f) if (*f + 7.0).abs() < 1e-10 => (),
            v => panic!("expected -7, got: {v:?}"),
        }
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
        let arr = match &v {
            ::netidx::subscriber::Value::Array(a) => a,
            _ => panic!("expected Array, got: {v:?}"),
        };
        assert_eq!(arr.len(), 1);
        let row = match &arr[0] {
            ::netidx::subscriber::Value::Array(r) => r,
            v => panic!("expected row Array, got: {v:?}"),
        };
        assert_eq!(row.len(), 2);
        assert!(matches!(&row[0], ::netidx::subscriber::Value::String(s) if &**s == "hello"));
        assert!(matches!(&row[1], ::netidx::subscriber::Value::String(s) if &**s == "world"));
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
        let arr = match &v {
            ::netidx::subscriber::Value::Array(a) => a,
            _ => panic!("expected Array, got: {v:?}"),
        };
        let row = match &arr[0] {
            ::netidx::subscriber::Value::Array(r) => r,
            v => panic!("expected row Array, got: {v:?}"),
        };
        assert!(matches!(&row[0], ::netidx::subscriber::Value::Bool(true)));
        assert!(matches!(&row[1], ::netidx::subscriber::Value::Bool(false)));
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
        let arr = match &v {
            ::netidx::subscriber::Value::Array(a) => a,
            _ => panic!("expected Array, got: {v:?}"),
        };
        assert_eq!(arr.len(), 1);
        let row = match &arr[0] {
            ::netidx::subscriber::Value::Array(r) => r,
            v => panic!("expected row Array, got: {v:?}"),
        };
        assert_eq!(row.len(), 4);
        assert!(matches!(&row[1], ::netidx::subscriber::Value::String(s) if &**s == "text"));
        assert!(matches!(&row[2], ::netidx::subscriber::Value::Bool(true)));
        match &row[3] {
            ::netidx::subscriber::Value::F64(f) if (*f - 2.718).abs() < 1e-10 => (),
            v => panic!("expected 2.718, got: {v:?}"),
        }
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
        // skip (0,1) — leave it empty
        ws.write(0, 2, "c").unwrap();
        wb.save(&path).unwrap();
        path
    },
    expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
        let arr = match &v {
            ::netidx::subscriber::Value::Array(a) => a,
            _ => panic!("expected Array, got: {v:?}"),
        };
        let row = match &arr[0] {
            ::netidx::subscriber::Value::Array(r) => r,
            v => panic!("expected row Array, got: {v:?}"),
        };
        assert_eq!(row.len(), 3);
        assert!(matches!(&row[0], ::netidx::subscriber::Value::String(s) if &**s == "a"));
        assert!(matches!(&row[1], ::netidx::subscriber::Value::Null));
        assert!(matches!(&row[2], ::netidx::subscriber::Value::String(s) if &**s == "c"));
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
