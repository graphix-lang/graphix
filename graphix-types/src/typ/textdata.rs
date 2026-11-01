//! Which types JSON and TOML read back as they were written
//! (`'a: JsonData`, `'a: TomlData`): a type whose every union tells its
//! members apart by a tag. A member other than `null` is a variant, by its
//! tag, or an abstract type, which the formats write inside an object
//! keyed by its unqualified name (`{"Port": 80}`); no two members share a
//! tag, or the union is one member and `null`. Anything else is refused:
//! `[i64, string]`, `[Port, u8]`, `` [Transparent<Port>, `A] ``, an open
//! member beside another. A `Transparent<T>`, written bare, has no tag, and
//! is refused over a type `T` that is not abstract, where it would change
//! nothing. A typedef whose parameters grow as it recurses is refused: its
//! expansions never end.

use crate::{
    env::Env,
    expr::ModPath,
    typ::{AbstractId, Typ, Type, cast::grows_on, params_size},
};
use ahash::AHashSet;
use arcstr::ArcStr;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

/// Why JSON or TOML may not read back what they write of a type
/// ([`Type::text_refusal`]).
#[derive(Debug, Clone)]
pub enum TextRefusal {
    /// A member with no tag in a union holding another member but `null`.
    Untagged { union: Type, member: Type },
    /// Two members of a union under one tag.
    SharedTag { union: Type, tag: ArcStr, a: Type, b: Type },
    /// `Transparent` over a type that is not abstract.
    Transparent(Type),
    /// A typedef whose parameters grow as it recurses.
    Grows(ModPath),
}

/// A named type a walk expands: a typedef by its definition, or an
/// abstract type's representation by its identity.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Named {
    Def(usize),
    Abstract(AbstractId),
}

struct Walk<'a> {
    env: &'a Env,
    /// Each named type expanded, by what it names and its arguments.
    seen: LPooled<AHashSet<(Named, Arc<[Type]>)>>,
    /// The named types the walk is inside, each with its arguments' size.
    path: LPooled<Vec<(Named, usize)>>,
}

impl<'a> Walk<'a> {
    /// `f` over the expansion of the named type `named` at `params`,
    /// unless the walk expanded it already.
    fn named(
        &mut self,
        named: Named,
        name: impl FnOnce() -> ModPath,
        params: &Arc<[Type]>,
        body: Type,
    ) -> Result<(), TextRefusal> {
        if !self.seen.insert((named, params.clone())) {
            return Ok(());
        }
        let size = params_size(params);
        if grows_on(&self.path, &named, size) {
            return Err(TextRefusal::Grows(name()));
        }
        self.path.push((named, size));
        let r = self.walk(&body);
        self.path.pop();
        r
    }

    /// The typedef reference `t`'s body at its arguments; `None` when it
    /// names nothing known. Fills no cell and adds no constraint.
    fn expand(&self, t: &Type) -> Option<(usize, Type)> {
        let Type::Ref(tr) = t else { return None };
        let def = tr.resolve_peek(self.env)?;
        let known = def.bindings(&tr.params)?;
        Some((def.def_key(), def.typ.replace_tvars(&known)))
    }

    /// `t` through its variables and names to its constructor; `None` for
    /// an open variable or a name that names nothing known.
    fn head(&self, t: &Type) -> Option<Type> {
        let mut t = t.clone();
        loop {
            t = match &t {
                Type::TVar(tv) => tv.binding()?,
                Type::Ref(_) => self.expand(&t)?.1,
                _ => return Some(t),
            }
        }
    }

    fn walk(&mut self, t: &Type) -> Result<(), TextRefusal> {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.walk(&b),
                None => Ok(()),
            },
            Type::Ref(tr) => match self.expand(t) {
                Some((def, body)) => {
                    self.named(Named::Def(def), || tr.name.clone(), &tr.params, body)
                }
                None => Ok(()),
            },
            Type::Set(_) => self.union(t),
            Type::Primitive(p) if (*p & !(Typ::Null | Typ::Error)).len() > 1 => {
                self.union(t)
            }
            Type::Array(e) | Type::List(e) => self.walk(e),
            Type::Tuple(ts) | Type::Variant(_, ts, _) => {
                ts.iter().try_for_each(|t| self.walk(t))
            }
            Type::Struct(fs) => fs.iter().try_for_each(|(_, t, _)| self.walk(t)),
            Type::Map { key, value } => {
                self.walk(key)?;
                self.walk(value)
            }
            Type::Abstract { id, params } => self.abstract_type(t, *id, params),
            Type::Primitive(_)
            | Type::Any
            | Type::Bottom
            | Type::Error(_)
            | Type::Fn(_)
            | Type::ByRef(..)
            | Type::App(..)
            | Type::Hole
            | Type::Bound(_) => Ok(()),
        })
    }

    // XCR claude for claude: [bug/medium] An abstract type written as its
    // representation takes that representation's shape here, but a union's
    // reader tries it only after every member of the value's own class and never
    // keeps the data as it is, so any primitive member that converts the written
    // data wins. With type N = Abstract<i64> and type S = Abstract<string> (each
    // with impl Cast), [N, f64] writes N(2) as 2 and json::read gives back f64
    // 2.0; [S, bool] reads S("true") back as true; [S, i64] reads S("1") back as
    // 1, a map key alike ({"1":1}). JsonData accepts all three. Either model such
    // a member as overlapping every member the reader converts its data into, or
    // have the union reader try it in its representation's class. probes:
    // ~/tmp/cr29/scratch/laneTA/idnum.gx, idkey.gx (textdata-abstract-01)
    // 2026-10-30 claude: Fixed with the round-2 textdata model
    // (holds-exactly-class-01): an abstract type written as its representation is
    // Shape::Foreign, its readings and its own reading outside the data's class,
    // so the union's pick tries it after every member that reads the data in its
    // class, in the union's order. [S, bool] and Map<[Id, i64], i64> are refused;
    // [N, f64] is admitted and reads back (the lane's Choice lets N's kept
    // reading replace f64's crossing one). Pins, the first two failing without
    // Shape::Foreign (main and 5fa76d1e2 admit them):
    // lib_tests::json::{json_target_abstract_string_beside_a_bool_refused,
    // json_target_abstract_key_beside_an_integer_refused,
    // json_abstract_number_beside_a_float_reads_back}.
    // 2026-10-30 claude: Round 5: an abstract written as its representation that
    // holds itself (type N = Abstract<[N, `L(i64)]>), or two that hold each
    // other, made a cycle of Shape::Foreign nodes, which textdata's member walks
    // followed without end (a stack overflow in --check, json::read into N, [N,
    // `B(string)] or Map<N, i64>). The walks now fold over Shapes::reach, each
    // node met once. Verdicts are main's: refused, since N(N(x)) writes what N(x)
    // does. Pins: lib_tests::json::{json_target_abstract_holding_itself_refused,
    // json_target_key_abstract_holding_itself_refused,
    // json_target_abstracts_holding_each_other_refused} (each overflows the stack
    // on 1ab657e8c).
    // 2026-10-31 claude: Round 6 (review7 B r4): an abstract written as its
    // representation whose Cast implementation writes from_value was modelled as
    // reading data as its representation, but the cast calls from_value, which
    // may read anything: with a from_value reading a string s as Id(str::len(s)),
    // [Id, `A] read `A back as Id(1) (main too). Shape::Foreign now carries
    // whether from_value is written (Env::read_as_representation); such a reader
    // reads every value it is tried on, outside the class. An impl writing both
    // methods is still Shape::Unknown, trusted: textdata-cast-trust-01 asks Eric
    // which rule holds. Pins:
    // lib_tests::json::json_target_abstract_reading_by_from_value_beside_a_tag_refused
    // (admitted without the change),
    // json_abstract_reading_by_from_value_beside_a_payload_variant_reads_back.
    // 2026-10-31 claude: Round 7 (brief r4B5): an abstract whose Cast implementation
    // writes to_value and leaves from_value out was Shape::Unknown, reading nothing,
    // while the cast reads it as its representation: [S, `A] read "A" back as S("A")
    // and [N, decimal] read 3 back as N(3) (main too). Shape::Foreign now keeps the
    // two sides apart (writes: to_value left out; reads_rep: from_value left out), so
    // such an abstract writes nothing known here and reads as its representation. Pins
    // (admitted at 3cb1fc3d2):
    // lib_tests::json::{json_target_abstract_reading_as_its_representation_beside_a_tag_refused,
    // json_target_abstract_reading_as_its_representation_beside_a_decimal_refused};
    // json_target_abstract_beside_a_decimal_refused (impl Cast {}) stays refused.
    // 2026-10-31 claude: Round 8 (review7 B r5): the writer's map_shape took an abstract key
    // type as one that never writes a string, while textdata took one whose representation holds
    // a string (or whose implementation writes to_value) as writing its keys as pairs: Map<Id,
    // string> over type Id = Abstract<[i64, string]> was admitted and wrote {Id(3) => "a"} as
    // {"3":"a"}, read back as Id("3") (main too). The writer now asks Type::may_write_string,
    // textdata's rule, so such a map is written as pairs. Pins (fail at af00eb549):
    // lib_tests::json::{json_abstract_key_that_may_write_a_string_reads_back,
    // json_abstract_key_writer_follows_recursive_representations},
    // lib_tests::toml::toml_abstract_key_that_may_write_a_string_reads_back.
    // 2026-10-31 claude: Round 9 (brief r4B6): round 8's writer rule (Type::may_write_string: an
    // abstract whose impl writes to_value may write a string) wrote Map<Id, string> over type Id
    // = Abstract<i64>, impl Cast { to_value = |x| x.0 }, as pairs, which TOML refuses at a
    // document's root where main writes 3 = "a"
    // (~/tmp/cr29/scratch/review7/B/r5/r2/hand/wr1_toml.gx, wr3_toml.gx), and JSON as pairs
    // where main wrote an object. An object's keys reach the reader as text, so the rule now
    // asks the reader: Type::may_read_text_as_string, asked by the writer's map_shape and
    // textdata's key_forms alike, so the two agree by construction. Id reads a key's text as its
    // representation i64, so the map is an object again. Pins (fail at e94d78407):
    // lib_tests::toml::toml_abstract_key_read_as_its_representation_writes_a_table,
    // lib_tests::json::json_abstract_key_read_as_its_representation_writes_an_object; round 8's
    // pins (json_abstract_key_that_may_write_a_string_reads_back, toml_ alike,
    // json_abstract_key_writer_follows_recursive_representations) still pass.
    // 2026-10-31 claude: Eric ruled (round 5 D6, 10-31): JSON and TOML write an abstract value
    // wrapped in an object keyed by its type's unqualified name, the inside being its to_value
    // or its representation; a core wrapper type (working name Transparent<T>) writes T's inside
    // bare, per use; every union a json/toml read or write holds is tagged members (variants,
    // wrapped abstracts, no two sharing a tag) or one member and null, else refused, writers
    // included; [Transparent<T>, null] is trusted and documented. This replaces the
    // member-by-member textdata analysis; lane B builds it.
    // 2026-10-31 claude: Round 6 (brief r6B): built D6. JSON and TOML write an abstract value
    // inside an object of one entry keyed by its type's unqualified name and read it only so
    // (core::Abstracts::Wrapped, CastInput::Text, Type::cast_abstract), and every union a
    // json/toml read or write holds is variants and abstract types under distinct tags or one
    // member beside null (Type::text_refusal, which replaces Shapes): [N, f64], [S, bool], [S,
    // i64] and Map<[Id, i64], i64> are refused for their untagged member, and an abstract beside
    // a variant reads back by the tag. Pins: lib_tests::json::{json_forms_read_back (false on
    // the lane base: ~/ptmp/cr29/scratch/r6-B/forms_nontransparent.gx),
    // json_abstract_reads_only_its_object, json_written_untagged_union_refused,
    // json_members_sharing_a_tag_refused, json_abstract_holding_itself_reads_back},
    // lib_tests::toml::toml_forms_read_back; the round 2-9 pins named above are deleted with the
    // model they pinned.
    // XCR claude for claude: [design/low] textdata trusts an abstract type whose Cast
    // implementation writes to_value (Shape::Unknown shares a form with nothing),
    // whatever its from_value does, while one that writes only from_value is judged
    // to read every value it is tried on (Shape::Foreign(_, true)). With a
    // from_value that reads a string s as Id(str::len(s)), [Id, `A] is refused when
    // the impl leaves to_value out and admitted when it writes one, and in both the
    // cast reads `A back as Id(1). Rule needed: (a) a written from_value reads any
    // value wherever it stands, so the second is refused too; or (b) what a written
    // method reads is the impl's duty, as from_value(to_value(x)) == x is
    // (design/abstract_cast.md), so neither is judged and Shape::Foreign's flag
    // goes. Recommend (a): JsonData then holds whatever the impl writes; a
    // from_value is tried after every member keeping the data's class, so only
    // members outside it are refused beside one. Probes:
    // ~/tmp/cr29/scratch/laneB/r7/fv.txt, fv1 and fv3 (run with hand.py there)
    // (textdata-cast-trust-01)
    // 2026-10-31 claude: The to_value-only case is now judged: an implementation
    // that writes to_value and leaves from_value out reads as its representation
    // (Shape::Foreign { writes: false, reads_rep: true }), so [S, `A] over type S =
    // Abstract<string> is refused (textdata-abstract-01's round 7 note). This CR's
    // question is now only about an implementation that writes both methods, still
    // Shape::Unknown.
    // 2026-10-31 claude: Correction to the note above (review7 B r5): the question
    // also covers the write side of every implementation that writes to_value,
    // from_value written or not. Such an implementation writes Shape::Foreign {
    // writes: false, .. }, which writes nothing known here and so is trusted, and
    // (b) does not cover it: the implementation may keep from_value(to_value(x)) ==
    // x while another member of the union takes what to_value writes. With type Id
    // = Abstract<i64> and to_value = |x| x.0 (from_value left out),
    // Map<[Id, i64], string> is admitted and reads Id(3) back as 3; with to_value =
    // |x| cast<string>(x.0)$, [Id, string] reads Id(3) back as "3" (JSON and TOML;
    // main admits both; ~/tmp/cr29/scratch/laneB/r9/ak_json.txt, ak_toml.txt,
    // ~/tmp/cr29/scratch/review7/B/r5/hand/h1.gx, h1t.gx). Its read side is judged
    // unless from_value is written. The write side's options: to_value may write
    // any value, so such an abstract is refused beside every member that reads
    // data; or it writes what its method's body returns, judged as that type's
    // data; or it stays trusted, as now.
    // 2026-10-31 claude: Round 9 (brief r4B6): a map's key forms now follow the
    // read side, Type::may_read_text_as_string, which the writer's map_shape and
    // textdata's key_forms both ask: an abstract read as its representation
    // (from_value left out) reads a key's text as the representation does, whatever
    // to_value writes, and one whose from_value is written is taken, as this CR's
    // conservative answer, to read it as a string. A case the ruling decides: type
    // Id = Abstract<i64> whose impl writes to_value = |x| x.0 and a from_value
    // reading cast<i64>(v): Map<Id, string> is now written as pairs, which TOML
    // refuses at a document's root ("unsupported array type") where main writes 3 =
    // "a" and reads it back (~/tmp/cr29/scratch/review7/B/r5/r2/hand/wr2_toml.gx);
    // JSON writes [[3,"a"]] and reads it back
    // (lib_tests::json::json_abstract_key_read_as_its_representation_writes_an_object).
    // Under (b) such a from_value is trusted to read a key's text and the map is a
    // table again. With from_value left out (wr1_toml.gx) the map is a table and
    // reads back.
    // 2026-10-31 claude: Round 6 review (review7 B r6): more of the write-side class
    // the correction note above names. type Ito = Abstract<i64> with to_value = |x| x.0
    // and from_value left out: Map<[Ito, u8], i64>, Map<[Ito, datetime], i64> and
    // Map<[Ito, duration], i64> are admitted in JSON and TOML and read the key Ito(3)
    // back as u8:3, as datetime 1970-01-01T00:00:03Z and as 3.s; main admits all three
    // and misreads them alike. The map's key forms are judged right (Ito reads a key's
    // text as its representation does); what to_value writes is trusted. Probes:
    // ~/tmp/cr29/scratch/review7/B/r6/rt/ito_u8_json.gx, ito_u8_toml.gx,
    // ito_dt_json.gx, ito_dt_toml.gx, ito_dur_json.gx, ito_dur_toml.gx; results
    // ~/tmp/cr29/scratch/review7/B/r6/ito.txt. The ruling's write-side options decide
    // them.
    // 2026-10-31 claude: Eric ruled (round 5 D6, 10-31): neither half is judged. JSON
    // and TOML write an abstract value wrapped in an object keyed by its type's
    // unqualified name (the inside its to_value or representation); a core wrapper type
    // (working name Transparent<T>) writes T's inside bare, per use; every union a
    // json/toml read or write holds is tagged members (variants, wrapped abstracts, no
    // two sharing a tag) or one member and null, else refused, writers included;
    // [Transparent<T>, null] is trusted and documented. Lane B's next round replaces
    // the member-by-member analysis with this.
    // 2026-10-31 claude: Built Eric's D6 answer: neither half is judged. An abstract
    // value is written as {"Name": data} (its to_value or its representation) and read
    // only from that object, so what a written to_value or from_value does reaches no
    // other member: a union holds the abstract only beside variants and abstract types
    // of other names, or alone beside null; core's Transparent<T> writes T's data bare
    // and is untagged ([Transparent<T>, null] is trusted, documented in json.md and the
    // json gxi: a T that writes null reads back as the outer null). Map keys follow the
    // key type alone (an object only under string), so no key's text reaches
    // from_value. Pins: lib_tests::json::{json_forms_read_back,
    // json_abstract_reads_only_its_object,
    // json_transparent_null_reads_back_as_the_option,
    // json_written_transparent_beside_a_tag_refused,
    // json_transparent_over_a_type_not_abstract_refused},
    // lib_tests::toml::toml_forms_read_back; on the lane base the forms do not read
    // back (~/ptmp/cr29/scratch/r6-B/forms_nontransparent.gx) and the refused writers
    // are admitted (untagged_writer.gx, shared_tag.gx).
    /// An abstract type: its representation where its `Cast`
    /// implementation reads or writes it as that, a `Transparent`'s
    /// parameter, which must be an abstract type.
    fn abstract_type(
        &mut self,
        t: &Type,
        id: AbstractId,
        params: &Arc<[Type]>,
    ) -> Result<(), TextRefusal> {
        let Some(rep) = self.env.abstract_reps.get(&id) else { return Ok(()) };
        let body = rep.instantiate_with(params);
        if id == AbstractId::transparent() {
            match self.head(&body) {
                None | Some(Type::Abstract { .. }) => (),
                Some(_) => return Err(TextRefusal::Transparent(t.clone())),
            }
        } else if !self.env.written_as_representation(id)
            && !self.env.read_as_representation(id)
        {
            return Ok(());
        }
        let name = || ModPath::from([rep.name.as_str()]);
        self.named(Named::Abstract(id), name, params, body)
    }

    /// The members of the union `t` past every union and name in them,
    /// each its constructor beside the type written; an open variable is
    /// its own constructor. An error is no member: no data reads into one.
    fn members(
        &self,
        t: &Type,
        out: &mut SmallVec<[(Type, Type); 8]>,
        seen: &mut Vec<Type>,
    ) {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.members(&b, out, seen),
                None => out.push((t.clone(), t.clone())),
            },
            Type::Set(ms) => ms.iter().for_each(|m| self.members(m, out, seen)),
            Type::Primitive(p) => (*p & !Typ::Error)
                .iter()
                .for_each(|b| out.push((Type::Primitive(b.into()), t.clone()))),
            Type::Bottom | Type::Error(_) => (),
            Type::Ref(_) if seen.contains(t) => (),
            Type::Ref(_) => match self.head(t) {
                Some(h @ (Type::Set(_) | Type::Primitive(_))) => {
                    seen.push(t.clone());
                    self.members(&h, out, seen);
                    seen.pop();
                }
                Some(h) => out.push((h, t.clone())),
                None => out.push((t.clone(), t.clone())),
            },
            t => out.push((t.clone(), t.clone())),
        })
    }

    /// The tag a member writes under: a variant's, or an abstract type's
    /// name; `None` for a member with none.
    fn tag(&self, head: &Type) -> Option<ArcStr> {
        match head {
            Type::Variant(tag, _, _) => Some(tag.clone()),
            Type::Abstract { id, .. } if *id != AbstractId::transparent() => {
                match self.env.abstract_reps.get(id) {
                    Some(r) => Some(r.name.clone()),
                    None => id.name(),
                }
            }
            _ => None,
        }
    }

    fn union(&mut self, t: &Type) -> Result<(), TextRefusal> {
        let mut members: SmallVec<[(Type, Type); 8]> = SmallVec::new();
        self.members(t, &mut members, &mut vec![]);
        let null = Type::Primitive(Typ::Null.into());
        members.retain(|(h, _)| *h != null);
        let mut tags: SmallVec<[(ArcStr, &Type); 8]> = SmallVec::new();
        for (head, _) in members.iter() {
            let Some(tag) = self.tag(head) else {
                if members.iter().any(|(h, _)| h != head) {
                    let member = head.clone();
                    return Err(TextRefusal::Untagged { union: t.clone(), member });
                }
                continue;
            };
            match tags.iter().find(|(t, _)| *t == tag) {
                Some((_, a)) if *a != head => {
                    let (a, b) = ((*a).clone(), head.clone());
                    return Err(TextRefusal::SharedTag { union: t.clone(), tag, a, b });
                }
                Some(_) => (),
                None => tags.push((tag, head)),
            }
        }
        members.iter().try_for_each(|(_, written)| self.walk(written))
    }
}

impl Type {
    /// Why JSON or TOML may not read back what they write of this type:
    /// a union whose members a tag does not tell apart, a `Transparent`
    /// over a type that is not abstract, a typedef growing its
    /// parameters; `None` when they read back every value of it.
    pub fn text_refusal(&self, env: &Env) -> Option<TextRefusal> {
        let mut walk = Walk { env, seen: LPooled::take(), path: LPooled::take() };
        walk.walk(self).err()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{expr::parser::parse_type, typ::cast::tests::typed};

    fn refused(t: &str) -> bool {
        let env = Env::default();
        parse_type(t).unwrap().text_refusal(&env).is_some()
    }

    /// [`refused`] over the typedefs `defs`; graphix-package-json's
    /// round trip reads these calls from this file.
    fn named(defs: &[(&str, &str)], t: &str) -> bool {
        let (env, t) = typed(defs, t);
        t.text_refusal(&env).is_some()
    }

    #[test]
    fn unions_tell_their_members_apart_by_a_tag() {
        assert!(!refused("[`A, `B(i64), `C(string, f64), null]"));
        assert!(!refused("[i64, null]"));
        assert!(!refused("[{a: i64}, null]"));
        assert!(!refused("Array<[`A(Array<i64>), `B({x: [f32, null]})]>"));
        assert!(!refused("[[`A, `B], `C]"));
        assert!(!refused("[[`A, null], `A]"));
        assert!(refused("[u8, i64]"));
        assert!(refused("[i64, f64]"));
        assert!(refused("[i64, string]"));
        assert!(refused("[`A, string]"));
        assert!(refused("[`A, null, string]"));
        assert!(refused("[`A(i64), `A(string)]"));
        assert!(refused("[`A, `A(i64)]"));
        assert!(refused("[{a: i64}, {b: i64}]"));
        assert!(refused("[Array<i64>, null, Map<string, i64>]"));
        assert!(refused("Array<[i64, string]>"));
        assert!(refused("{a: [`A, bool]}"));
        assert!(refused("Map<[i64, string], i64>"));
        assert!(!refused("{a: [i64, Error<string>]}"));
        assert!(!refused("[i64, error]"));
        assert!(!refused("[`A, error]"));
        assert!(!refused("Map<[`A, `B], [`C(i64), null]>"));
    }

    #[test]
    fn named_members_are_judged_by_their_constructors() {
        assert!(!named(&[("AB", "[`A, `B]")], "[AB, `C]"));
        assert!(!named(&[("AB", "[`A, `B]")], "[AB, `B]"));
        assert!(!named(&[("O<'a>", "['a, null]")], "O<O<[`A, `B]>>"));
        assert!(named(&[("O<'a>", "['a, null]")], "[O<i64>, `B]"));
        assert!(named(&[("AB", "[`A, `B(i64)]")], "[AB, `B(string)]"));
        assert!(named(&[("S", "{a: i64}")], "[S, `A]"));
        assert!(!named(&[("S", "{a: [S, null]}")], "[S, null]"));
        assert!(!named(
            &[("R<'a>", "[`Nil, `Cons('a, R<'a>)]")],
            "{k: R<i64>, l: R<[`A, `B]>}"
        ));
        assert!(named(&[("R<'a>", "[`Nil, `Cons('a, R<'a>)]")], "[R<i64>, R<string>]"));
        assert!(named(&[("R<'a>", "[`Nil, `Cons('a, R<'a>)]")], "R<[i64, string]>"));
        assert!(!named(
            &[("P<'a, 'b>", "[`N, `C('a, P<'b, 'a>)]")],
            "{k: P<i64, string>}"
        ));
    }

    // A typedef expands once for each list of arguments it is applied to,
    // so a recursion that passes its parameters on is a cycle; one that
    // grows them at every level never ends, and is refused.
    #[test]
    fn a_typedef_growing_its_parameters_is_refused() {
        assert!(!named(&[("A<'a>", "[`Here('a), `Next(A<Array<i64>>)]")], "A<i64>"));
        let (env, t) = typed(&[("N<'a>", "[`Nil, `Cons('a, N<Array<'a>>)]")], "N<i64>");
        assert!(matches!(
            t.text_refusal(&env),
            Some(TextRefusal::Grows(n)) if n.to_string() == "N"
        ));
    }
}
