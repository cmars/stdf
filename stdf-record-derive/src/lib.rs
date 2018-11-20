extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(STDFRecord, attributes(STDFField))]
pub fn stdf_record(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    let name = derive_input.ident;
    let generics = derive_input.generics;
    let record_struct = if let syn::Data::Struct(ref record_struct) = derive_input.data {
        record_struct
    } else {
        return TokenStream::from(quote!{});
    };

    let try_read_fields = record_struct
        .fields
        .iter()
        .filter(|x| x.ident.as_ref().unwrap().to_string() != "header")
        .map(|ref x| {
            let name = x.ident.as_ref().unwrap();
            let ty = &x.ty;
            quote! {
                #name: bytes.read_with::<#ty>(offset, endian)?
            }
        });
    let try_write_fields = record_struct
        .fields
        .iter()
        .filter(|x| x.ident.as_ref().unwrap().to_string() != "header")
        .map(|ref x| {
            let name = x.ident.as_ref().unwrap();
            let ty = &x.ty;
            quote! {
                bytes.write_with::<#ty>(offset, self.#name, endian)?;
            }
        });
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let try_read = quote!{
        impl #impl_generics TryRead<'a, ctx::Endian> for #name #ty_generics #where_clause {
            fn try_read(bytes: &'a [u8], endian: ctx::Endian) -> byte::Result<(Self, usize)> {
                let offset = &mut 0;
                Ok((
                    #name {
                        header: bytes.read_with::<Header>(offset, endian)?,
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
                bytes.write_with::<Header>(offset, self.header, endian)?;
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
