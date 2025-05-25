use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DeriveInput, Fields, parse_macro_input};

#[proc_macro_derive(From, attributes(from))]
pub fn derive_from(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let Data::Enum(data_enum) = input.data else {
        return syn::Error::new_spanned(name, "From can only be derived for enums")
            .to_compile_error()
            .into();
    };

    let variants = data_enum.variants;

    let mut impls = vec![];

    for variant in variants {
        let Fields::Unnamed(fields) = &variant.fields else {
            continue;
        };

        if fields.unnamed.len() != 1 {
            continue;
        }

        let field = &fields.unnamed[0];
        let from_attr = field.attrs.iter().any(is_from_attr);
        if !from_attr {
            continue;
        }

        let var_ident = &variant.ident;
        let ty = &field.ty;
        impls.push(quote! {
            impl From<#ty> for #name {
                fn from(value: #ty) -> Self {
                    Self::#var_ident(value)
                }
            }
        });
    }

    TokenStream::from(quote! {
        #(#impls)*
    })
}

fn is_from_attr(attr: &Attribute) -> bool {
    attr.path().is_ident("from")
}
