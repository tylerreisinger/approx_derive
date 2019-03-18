use proc_macro;
use syn::{self, parse_macro_input, Token, parenthesized};
use syn::parse::Parse;

pub struct DeriveConfig {
    /// A type specified with `#[approx(epsilon_ty = ...)]` to be used for `AbsDiffEq::Epsilon`
    /// If omitted, try to infer a valid type, or fail if we cannot.
    pub epsilon_ty: Option<syn::Type>,
}

#[derive(Debug, Clone)]
pub enum ConfigAttrib {
    EpsilonTy(syn::Type),
}

pub fn parse_attributes(attributes: &Vec<syn::Attribute>) -> Result<DeriveConfig, syn::Error> {
    println!("{:?}", attributes);
    let mut config = DeriveConfig {
        epsilon_ty: None,
    };
    for attrib in attributes {
        let inner_tts = attrib.tts.clone().into();
        let config_attrib: ConfigAttrib = syn::parse2(inner_tts)?;
        match config_attrib {
            ConfigAttrib::EpsilonTy(ty) => {
                config.epsilon_ty = Some(ty);
            }
        }
    }

    Ok(config)
}

impl Parse for ConfigAttrib {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let content;
        parenthesized!(content in input);
        let name = content.parse::<syn::Ident>()?;
        let name_str = name.clone().to_string();

        match &name_str[..] {
            "epsilon_ty" => {
                content.parse::<Token![=]>()?;
                let ty_lit: syn::Lit = content.parse()?;
                if let syn::Lit::Str(lit) = ty_lit {
                    let ty = syn::parse_str(lit.value().as_ref())?;
                    Ok(ConfigAttrib::EpsilonTy(ty))
                } else {
                    Err(syn::parse::Error::new(name.span(), "epsilon_ty must be provided as a quoted string"))
                }
            },
            _ => {
                Err(syn::parse::Error::new(name.span(), format!("Invalid approx attribute {}", name)))
            }
        }
    }
}
