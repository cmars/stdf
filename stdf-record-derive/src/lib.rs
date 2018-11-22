extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

fn attr_name(a: &syn::Path) -> String {
    a.segments
        .iter()
        .map(|x| x.ident.to_string())
        .collect::<Vec<String>>()
        .join("::")
}

fn default_attr(f: &syn::Field) -> Option<proc_macro2::TokenStream> {
    for attr in &f.attrs {
        if attr_name(&attr.path) == "default" {
            return Some(attr.tts.clone());
        }
    }
    None
}

fn array_length_attr(f: &syn::Field) -> Option<proc_macro2::TokenStream> {
    for attr in &f.attrs {
        if attr_name(&attr.path) == "array_length" {
            return Some(attr.tts.clone());
        }
    }
    None
}

fn array_type_attr(f: &syn::Field) -> Option<proc_macro2::TokenStream> {
    for attr in &f.attrs {
        if attr_name(&attr.path) == "array_type" {
            return Some(attr.tts.clone());
        }
    }
    None
}

#[proc_macro_derive(
    STDFRecord,
    attributes(default, array_length, nibble_array_length, array_type)
)]
pub fn stdf_record(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    let name = derive_input.ident;
    let generics = derive_input.generics;
    let record_struct = if let syn::Data::Struct(ref record_struct) = derive_input.data {
        record_struct
    } else {
        return TokenStream::from(quote!{});
    };

    let try_read_vars = record_struct.fields.iter().map(|ref x| {
        let name = x.ident.as_ref().unwrap();
        let ty = &x.ty;
        let missing = match default_attr(x) {
            Some(ts) => ts,
            None => quote! { return Err(byte::Error::Incomplete) },
        };
        match (array_length_attr(x), array_type_attr(x)) {
            (Some(ref index_name), Some(ref index_type)) => quote! {
                let mut #name: Vec<#index_type> = vec![];
                for i in 0..#index_name as usize {
                    let v = match bytes.read_with::<#index_type>(offset, endian) {
                        Ok(v) => v,
                        Err(byte::Error::Incomplete) => break,
                        Err(byte::Error::BadOffset(_)) => break,
                        Err(e) => return Err(e),
                    };
                    #name.push(v);
                }
            },
            (None, None) => quote! {
                let #name = match bytes.read_with::<#ty>(offset, endian) {
                    Ok(v) => v,
                    Err(byte::Error::Incomplete) => #missing,
                    Err(byte::Error::BadOffset(_)) => #missing,
                    Err(e) => return Err(e),
                };
            },
            _ => panic!("invalid array attributes"),
        }
    });
    let try_read_fields = record_struct.fields.iter().map(|ref x| {
        let name = x.ident.as_ref().unwrap();
        quote! {
            #name: #name
        }
    });
    let try_write_fields = record_struct.fields.iter().map(|ref x| {
        let name = x.ident.as_ref().unwrap();
        let ty = &x.ty;
        match array_type_attr(x) {
            Some(ref index_type) => quote! {
                for v in self.#name {
                    bytes.write_with::<#index_type>(offset, v.clone(), endian)?;
                }
            },
            None => quote! {
                bytes.write_with::<#ty>(offset, self.#name, endian)?;
            },
        }
    });
    let default_impl_generics: syn::Generics = parse_quote! { <'a> };
    let (record_impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    // TryRead needs to declare an 'a lifetime for the byte slice. The record type we're
    // implementing for might already declare an 'a lifetime if it contains variable-length field
    // types, which should use the same lifetime as the read buffer. However if it doesn't, an 'a
    // lifetime needs to be declared.
    let (impl_generics, record_ty_lifetimes) = if generics.lifetimes().count() == 0 {
        let (ig, _, _) = default_impl_generics.split_for_impl();
        (ig, default_impl_generics.lifetimes())
    } else {
        (record_impl_generics, generics.lifetimes())
    };
    let try_read = quote!{
        impl #impl_generics TryRead<#(#record_ty_lifetimes,)* ctx::Endian> for #name #ty_generics #where_clause {
            fn try_read(bytes: &'a [u8], endian: ctx::Endian) -> byte::Result<(Self, usize)> {
                let offset = &mut 0;
                #(#try_read_vars)*
                Ok((
                    #name {
                        #(#try_read_fields),*
                    },
                    *offset,
                ))
            }
        }
    };
    let try_write = quote!{
        impl #impl_generics TryWrite<ctx::Endian> for #name #ty_generics #where_clause {
            fn try_write(self, bytes: &mut [u8], endian: ctx::Endian) -> byte::Result<usize> {
                let offset = &mut 0;
                #(#try_write_fields);*
                Ok(*offset)
            }
        }
    };
    TokenStream::from(quote!{
        #try_read
        #try_write
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
