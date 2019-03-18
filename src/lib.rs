extern crate proc_macro;

mod attrib;
mod config;

use proc_macro::TokenStream;
use proc_macro2::Span;
use syn::{parse_quote, Token};
use quote::quote;

use attrib::{FieldMember, MemberAttribKind};


#[proc_macro_derive(AbsDiffEq, attributes(approx))]
pub fn derive_abs_diff_eq(tokens: TokenStream) -> TokenStream {
    match derive_abs_diff_eq_impl(tokens) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error().into()
    }
}

fn derive_abs_diff_eq_impl(tokens: TokenStream) -> Result<TokenStream, syn::Error> {
    let input: syn::DeriveInput = syn::parse(tokens)?;
    let ident = input.ident;
    let config = config::parse_attributes(&input.attrs)?;

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    match input.data {
        syn::Data::Struct(ref obj) => {
            let fields = &obj.fields;
            let epsilon_ty =
                if let Some(ty) = config.epsilon_ty { ty } else { get_epsilon_type(fields.iter()) };
            let members = get_members_from_fields(fields)?;

            let compare_expr = build_approx_eq_body(&members, &epsilon_ty, &build_abs_diff_comparison);

            let default_epsilon =
                if let Some(val) = config.default_epsilon {
                    val
                } else {
                    parse_quote! { Self::Epsilon::default_epsilon() }
                };

            let out = quote! {
                #[automatically_derived]
                impl #impl_generics approx::AbsDiffEq for #ident #type_generics
                    #where_clause
                {
                    type Epsilon = <#epsilon_ty as approx::AbsDiffEq>::Epsilon;

                    #[inline]
                    fn default_epsilon() -> Self::Epsilon {
                        #default_epsilon
                    }

                    fn abs_diff_eq(&self, other: &Self, epsilon: Self::Epsilon) -> bool {
                        #compare_expr
                    }
                }
            };
            Ok(out.into())
        }
        syn::Data::Enum(ref obj) => {
            unimplemented!()
        }
        syn::Data::Union(r#union) => {
            Err(syn::Error::new(r#union.union_token.span, "Unions are not supported by approx_derive"))
        }
    }
}

fn build_exact_comparison(field_member: &FieldMember) -> syn::Expr {
    let member = &field_member.member;
    parse_quote! {
        self.#member == other.#member
    }
}

fn build_abs_diff_comparison(field_member: &FieldMember) -> syn::Expr {
    if field_member.has_attrib(MemberAttribKind::ExactEq) {
        build_exact_comparison(field_member)
    } else {
        let member = &field_member.member;
        let ty = field_member.ty();
        parse_quote! {
            approx::AbsDiffEq::abs_diff_eq(&self.#member, &other.#member, epsilon.clone() as <#ty as approx::AbsDiffEq>::Epsilon)
        }
    }
}

fn build_approx_eq_body<F>(members: &Vec<FieldMember>, epsilon_ty: &syn::Type, expr_builder: F) -> syn::Expr
    where F: Fn(&FieldMember) -> syn::Expr
{
    let mut compare_exprs = syn::punctuated::Punctuated::<syn::Expr, Token![&&]>::new();
    for field_member in members {
        let expr = expr_builder(field_member);
        compare_exprs.push(expr);
    }
    parse_quote! { #compare_exprs }
}

fn get_field_span(field: &syn::Field) -> Span {
    if let Some(ref ident) = field.ident {
        ident.span()
    } else {
        if let syn::Type::Path(path) = &field.ty {
            path.path.segments[0].ident.span()
        } else {
            Span::call_site()
        }
    }
}

fn get_members_from_fields(fields: &syn::Fields) -> Result<Vec<FieldMember>, syn::Error> {
    let mut members = Vec::new();
    match fields {
        syn::Fields::Named(fields) => {
            for field in fields.named.iter().cloned() {
                let member = syn::Member::Named(field.ident.clone().unwrap());
                let field_members = FieldMember::new(member, field)?;
                members.push(field_members);
            }
        },
        syn::Fields::Unnamed(fields) => {
            for (i, field) in fields.unnamed.iter().cloned().enumerate() {
                let field_span = get_field_span(&field);
                let member = syn::Member::Unnamed(syn::Index { index: i as u32, span: field_span });
                let field_members = FieldMember::new(member, field)?;
                members.push(field_members);
            }
        },
        syn::Fields::Unit => {},
    }
    Ok(members)
}

fn get_epsilon_type(fields: syn::punctuated::Iter<syn::Field>) -> syn::Type {
    let f32_ty: syn::Type = parse_quote! {f32};
    let f64_ty: syn::Type = parse_quote! {f64};
    let mut out_type: syn::Type = f32_ty.clone();
    for field in fields {
        let ty = &field.ty;
        if *ty == f64_ty && out_type == f32_ty {
            out_type = ty.clone();
        }
    }
    out_type
}

