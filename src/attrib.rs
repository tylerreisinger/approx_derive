use proc_macro2::Span;
use syn::{self, parenthesized, Token};
use syn::parse::Parse;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MemberAttribKind {
    ExactEq
}

impl MemberAttribKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            MemberAttribKind::ExactEq => "exact_eq",
        }
    }

    pub fn from_str(name: &str, span: proc_macro2::Span) -> Result<MemberAttribKind, syn::Error> {
        match name {
            "exact_eq" => Ok(MemberAttribKind::ExactEq),
            _ => Err(syn::Error::new(span, format!("Unknown member attribute {}", name))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FieldMember {
    pub member: syn::Member,
    pub parsed_attribs: Vec<MemberAttribute>,
    pub field: syn::Field,
}

#[derive(Debug, Clone)]
pub struct MemberAttribute {
    pub ident: syn::Ident,
    pub kind: MemberAttribKind,
    pub value: Option<syn::Expr>,
}

impl FieldMember {
    pub fn new(member: syn::Member, field: syn::Field) -> Result<FieldMember, syn::Error> {
        let mut parsed_attribs = Vec::new();
        for attrib in &field.attrs {
            if attrib.path.segments[0].ident.to_string() == "approx" {
                println!("{:?}", attrib);
                let parsed_attrib = syn::parse2::<MemberAttribute>(attrib.tts.clone())?;
                parsed_attribs.push(parsed_attrib);
            }
        }
        Ok(FieldMember {
            member,
            field,
            parsed_attribs,
        })
    }
    pub fn ty(&self) -> &syn::Type {
        &self.field.ty
    }
    pub fn has_attrib(&self, kind: MemberAttribKind) -> bool {
        for attrib in &self.parsed_attribs {
            if attrib.kind == kind {
                return true;
            }
        }
        false
    }
}

impl Parse for MemberAttribute {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let content;
        parenthesized!(content in input);
        let name = content.parse::<syn::Ident>()?;
        let name_str = name.to_string();
        let attrib_kind = MemberAttribKind::from_str(&name_str[..], name.span())?;

        let value =
            if let Ok(_) = content.parse::<Token![=]>() {
                Some(content.parse::<syn::Expr>()?)
            } else {
                None
            };

        Ok(MemberAttribute {
            kind: attrib_kind,
            ident: name,
            value,
        })
    }
}
