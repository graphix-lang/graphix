//! The image of what a scheme's cells keep ([`Known`]): a registration a
//! check by bounds made is restored with its schemes whole. Written
//! inside a cell's definition, so the cells these hold are references.

use super::{
    Known, Left, Use,
    select::{Arm, Pat, Template},
};
use crate::{
    expr::ModPath,
    typ::{TVar, Type, tvar::Level},
};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use smallvec::SmallVec;
use triomphe::Arc;

fn put_len(n: usize, buf: &mut impl BufMut) {
    encode_varint(n as u64, buf)
}

fn get_len(buf: &mut impl Buf) -> Result<usize, PackError> {
    Ok(decode_varint(buf)? as usize)
}

fn put_all<T: Pack>(xs: &[T], buf: &mut impl BufMut) -> Result<(), PackError> {
    put_len(xs.len(), buf);
    xs.iter().try_for_each(|x| x.encode(buf))
}

fn get_all<T: Pack, A: smallvec::Array<Item = T>>(
    buf: &mut impl Buf,
) -> Result<SmallVec<A>, PackError> {
    (0..get_len(buf)?).map(|_| T::decode(buf)).collect()
}

fn put_cell(v: &Option<TVar>, buf: &mut impl BufMut) -> Result<(), PackError> {
    match v {
        None => Ok(buf.put_u8(0)),
        Some(v) => {
            buf.put_u8(1);
            v.encode(buf)
        }
    }
}

fn get_cell(buf: &mut impl Buf) -> Result<Option<TVar>, PackError> {
    match u8::decode(buf)? {
        0 => Ok(None),
        1 => Ok(Some(TVar::decode(buf)?)),
        _ => Err(PackError::InvalidFormat),
    }
}

impl Pat {
    fn image_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let all = |ps: &[Pat], buf: &mut _| -> Result<(), PackError> {
            put_len(ps.len(), buf);
            ps.iter().try_for_each(|p| p.image_encode(buf))
        };
        crate::stack::ensure_sufficient(|| match self {
            Pat::Any(v) => {
                buf.put_u8(0);
                put_cell(v, buf)
            }
            Pat::Test(t, p) => {
                buf.put_u8(1);
                t.encode(buf)?;
                p.image_encode(buf)
            }
            Pat::Lit(t, whole) => {
                buf.put_u8(2);
                t.encode(buf)?;
                whole.encode(buf)
            }
            Pat::Tuple(v, ps) => {
                buf.put_u8(3);
                put_cell(v, buf)?;
                all(ps, buf)
            }
            Pat::Variant(v, tag, ps) => {
                buf.put_u8(4);
                put_cell(v, buf)?;
                tag.encode(buf)?;
                all(ps, buf)
            }
            Pat::Struct(v, fields, exhaustive) => {
                buf.put_u8(5);
                put_cell(v, buf)?;
                put_len(fields.len(), buf);
                for (name, p) in fields.iter() {
                    name.encode(buf)?;
                    p.image_encode(buf)?
                }
                exhaustive.encode(buf)
            }
            Pat::Elems { all: v, list, elems, open } => {
                buf.put_u8(6);
                put_cell(v, buf)?;
                list.encode(buf)?;
                all(elems, buf)?;
                match open {
                    None => Ok(buf.put_u8(0)),
                    Some(run) => {
                        buf.put_u8(1);
                        put_cell(run, buf)
                    }
                }
            }
            Pat::Or(ps) => {
                buf.put_u8(7);
                all(ps, buf)
            }
            Pat::Abs(v, id, p) => {
                buf.put_u8(8);
                put_cell(v, buf)?;
                Type::Abstract { id: *id, params: Arc::from_iter([]) }.encode(buf)?;
                p.image_encode(buf)
            }
            Pat::Whole(v, p) => {
                buf.put_u8(9);
                v.encode(buf)?;
                p.image_encode(buf)
            }
        })
    }

    fn image_decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let all = |buf: &mut _| -> Result<Vec<Pat>, PackError> {
            (0..get_len(buf)?).map(|_| Pat::image_decode(buf)).collect()
        };
        crate::stack::ensure_sufficient(|| {
            Ok(match u8::decode(buf)? {
                0 => Pat::Any(get_cell(buf)?),
                1 => Pat::Test(Type::decode(buf)?, Box::new(Pat::image_decode(buf)?)),
                2 => Pat::Lit(Type::decode(buf)?, bool::decode(buf)?),
                3 => Pat::Tuple(get_cell(buf)?, all(buf)?),
                4 => Pat::Variant(get_cell(buf)?, ArcStr::decode(buf)?, all(buf)?),
                5 => {
                    let v = get_cell(buf)?;
                    let fields = (0..get_len(buf)?)
                        .map(|_| Ok((ArcStr::decode(buf)?, Pat::image_decode(buf)?)))
                        .collect::<Result<Vec<_>, PackError>>()?;
                    Pat::Struct(v, fields, bool::decode(buf)?)
                }
                6 => {
                    let (v, list, elems) =
                        (get_cell(buf)?, bool::decode(buf)?, all(buf)?);
                    let open = match u8::decode(buf)? {
                        0 => None,
                        1 => Some(get_cell(buf)?),
                        _ => return Err(PackError::InvalidFormat),
                    };
                    Pat::Elems { all: v, list, elems, open }
                }
                7 => Pat::Or(all(buf)?),
                8 => {
                    let v = get_cell(buf)?;
                    let Type::Abstract { id, .. } = Type::decode(buf)? else {
                        return Err(PackError::InvalidFormat);
                    };
                    Pat::Abs(v, id, Box::new(Pat::image_decode(buf)?))
                }
                9 => Pat::Whole(TVar::decode(buf)?, Box::new(Pat::image_decode(buf)?)),
                _ => return Err(PackError::InvalidFormat),
            })
        })
    }
}

impl Template {
    fn image_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let (arms, at) = self.parts();
        put_len(arms.len(), buf);
        for arm in arms.iter() {
            arm.pat.image_encode(buf)?;
            arm.guarded.encode(buf)?
        }
        at.encode(buf)
    }

    fn image_decode(buf: &mut impl Buf) -> Result<Arc<Self>, PackError> {
        let arms = (0..get_len(buf)?)
            .map(|_| {
                Ok(Arm { pat: Pat::image_decode(buf)?, guarded: bool::decode(buf)? })
            })
            .collect::<Result<Vec<_>, PackError>>()?;
        Ok(Arc::new(Self::of_parts(arms, ArcStr::decode(buf)?)))
    }
}

fn put_args(
    args: &[(Option<ArcStr>, Type)],
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    put_len(args.len(), buf);
    for (label, t) in args.iter() {
        label.encode(buf)?;
        t.encode(buf)?
    }
    Ok(())
}

fn get_args(
    buf: &mut impl Buf,
) -> Result<SmallVec<[(Option<ArcStr>, Type); 4]>, PackError> {
    (0..get_len(buf)?).map(|_| Ok((Pack::decode(buf)?, Type::decode(buf)?))).collect()
}

impl Use {
    fn image_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        match self {
            Use::Field { field, out, write } => {
                buf.put_u8(0);
                field.encode(buf)?;
                out.encode(buf)?;
                write.encode(buf)
            }
            Use::Index { i, out, write, from } => {
                buf.put_u8(1);
                put_len(*i, buf);
                out.encode(buf)?;
                write.encode(buf)?;
                from.encode(buf)
            }
            Use::Element { out } => {
                buf.put_u8(2);
                out.encode(buf)
            }
            Use::Call { args, out } => {
                buf.put_u8(3);
                put_args(args, buf)?;
                out.encode(buf)
            }
            Use::With { fields, out } => {
                buf.put_u8(4);
                put_len(fields.len(), buf);
                for (name, t) in fields.iter() {
                    name.encode(buf)?;
                    t.encode(buf)?
                }
                out.encode(buf)
            }
            Use::Write { value } => {
                buf.put_u8(7);
                value.encode(buf)
            }
            Use::Defaults { method, name, labels, level } => {
                buf.put_u8(8);
                method.encode(buf)?;
                name.encode(buf)?;
                put_len(labels.len(), buf);
                for (label, t) in labels.iter() {
                    label.encode(buf)?;
                    t.encode(buf)?
                }
                level.encode(buf)
            }
            // a cell keeps a select as its template: the select itself is
            // its check's
            Use::Select(_) => Err(PackError::InvalidFormat),
            Use::Routed(_, of) => {
                buf.put_u8(5);
                of.image_encode(buf)
            }
            Use::Method { name, typings, receiver, at, args, out, throws, level } => {
                buf.put_u8(6);
                name.encode(buf)?;
                put_all(typings, buf)?;
                put_len(*receiver, buf);
                put_len(*at, buf);
                put_args(args, buf)?;
                out.encode(buf)?;
                throws.encode(buf)?;
                level.encode(buf)
            }
        }
    }

    fn image_decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(match u8::decode(buf)? {
            0 => Use::Field {
                field: ArcStr::decode(buf)?,
                out: Type::decode(buf)?,
                write: bool::decode(buf)?,
            },
            1 => Use::Index {
                i: get_len(buf)?,
                out: Type::decode(buf)?,
                write: bool::decode(buf)?,
                from: ModPath::decode(buf)?,
            },
            2 => Use::Element { out: Type::decode(buf)? },
            3 => Use::Call { args: get_args(buf)?, out: Type::decode(buf)? },
            4 => {
                let fields = (0..get_len(buf)?)
                    .map(|_| Ok((ArcStr::decode(buf)?, Type::decode(buf)?)))
                    .collect::<Result<_, PackError>>()?;
                Use::With { fields, out: Type::decode(buf)? }
            }
            // the select a restored template names is made by the check
            // that copies its cell
            5 => Use::Routed(usize::MAX, Template::image_decode(buf)?),
            6 => {
                let name = ArcStr::decode(buf)?;
                let typings: SmallVec<[Type; 4]> = get_all(buf)?;
                Use::Method {
                    name,
                    typings: Arc::from_iter(typings),
                    receiver: get_len(buf)?,
                    at: get_len(buf)?,
                    args: get_args(buf)?,
                    out: Type::decode(buf)?,
                    throws: Type::decode(buf)?,
                    level: Level::decode(buf)?,
                }
            }
            7 => Use::Write { value: Type::decode(buf)? },
            8 => {
                let method = crate::env::TraitMethodRef::decode(buf)?;
                let name = ArcStr::decode(buf)?;
                let labels = (0..get_len(buf)?)
                    .map(|_| Ok((ArcStr::decode(buf)?, Type::decode(buf)?)))
                    .collect::<Result<_, PackError>>()?;
                Use::Defaults { method, name, labels, level: Level::decode(buf)? }
            }
            _ => return Err(PackError::InvalidFormat),
        })
    }
}

impl Left {
    fn image_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        match self {
            Left::Held(pair) => {
                buf.put_u8(0);
                pair.0.encode(buf)?;
                pair.1.encode(buf)
            }
            Left::Single(v) => {
                buf.put_u8(1);
                v.encode(buf)
            }
            Left::Waiting(of, t) => {
                buf.put_u8(2);
                of.image_encode(buf)?;
                t.encode(buf)
            }
            Left::Compared(pair) => {
                buf.put_u8(3);
                pair.0.encode(buf)?;
                pair.1.encode(buf)?;
                pair.2.encode(buf)
            }
            Left::Asked(asked) => {
                buf.put_u8(4);
                asked.0.encode(buf)?;
                put_all(&asked.1, buf)?;
                asked.2.encode(buf)
            }
        }
    }

    fn image_decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(match u8::decode(buf)? {
            0 => Left::Held(Arc::new((Type::decode(buf)?, Type::decode(buf)?))),
            1 => Left::Single(TVar::decode(buf)?),
            2 => Left::Waiting(Template::image_decode(buf)?, Type::decode(buf)?),
            3 => Left::Compared(Arc::new((
                Type::decode(buf)?,
                Type::decode(buf)?,
                bool::decode(buf)?,
            ))),
            4 => Left::Asked(Arc::new((
                Type::decode(buf)?,
                get_all(buf)?,
                ArcStr::decode(buf)?,
            ))),
            _ => return Err(PackError::InvalidFormat),
        })
    }
}

impl Known {
    /// What a restored cell needs of this: everything but its place in
    /// the list of the check that made it.
    pub(crate) fn image_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let Known {
            feeders,
            sinks,
            quantified,
            demanded,
            made,
            listed: _,
            uses,
            left,
            defaults,
        } = self;
        put_all(feeders, buf)?;
        put_all(sinks, buf)?;
        quantified.encode(buf)?;
        demanded.encode(buf)?;
        made.encode(buf)?;
        put_len(uses.len(), buf);
        for (u, met) in uses.iter() {
            u.image_encode(buf)?;
            met.encode(buf)?
        }
        put_len(left.len(), buf);
        left.iter().try_for_each(|l| l.image_encode(buf))?;
        put_len(defaults.len(), buf);
        for (label, default, param) in defaults.iter() {
            label.encode(buf)?;
            default.encode(buf)?;
            param.encode(buf)?
        }
        Ok(())
    }

    pub(crate) fn image_decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let (feeders, sinks) = (get_all(buf)?, get_all(buf)?);
        let (quantified, demanded, made) =
            (bool::decode(buf)?, bool::decode(buf)?, bool::decode(buf)?);
        let uses = (0..get_len(buf)?)
            .map(|_| Ok((Use::image_decode(buf)?, bool::decode(buf)?)))
            .collect::<Result<_, PackError>>()?;
        let left = (0..get_len(buf)?)
            .map(|_| Left::image_decode(buf))
            .collect::<Result<_, PackError>>()?;
        let defaults = (0..get_len(buf)?)
            .map(|_| Ok((ArcStr::decode(buf)?, Type::decode(buf)?, Type::decode(buf)?)))
            .collect::<Result<_, PackError>>()?;
        Ok(Known {
            feeders,
            sinks,
            quantified,
            demanded,
            made,
            listed: false,
            uses,
            left,
            defaults,
        })
    }
}
