use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;
#[proc_macro_derive(Expr)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let fields = if let DeriveInput {
        data:
            syn::Data::Struct(syn::DataStruct {
                fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
                ..
            }),
        ..
    } = input
    {
        named
    } else {
        panic!("Expected the macro #[derive(Expr)] to be an attribute for structs");
    };

    let trait_methods = fields.iter().map(|f| {
        let field_name = f.ident.as_ref().expect(
            format!(
                "Expected field_name to from struct {}  to have an ident",
                name.to_string()
            )
            .as_str(),
        );
        if field_name == "operator" {
            quote! {
                fn #field_name(&self) -> Option<&Token> {
                    Some(&self.#field_name)
                }
            }
        } else {
            quote! {
                fn #field_name(&self) -> Option<&dyn Expr> {
                    Some(self.#field_name.as_ref())
                }
            }
        }
    });

    let output = quote! {
        impl Expr for #name {
            #(#trait_methods)*
        }

    };
    TokenStream::from(output)
}

#[proc_macro_derive(Impl)]
pub fn derive_impl(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    let fields = if let DeriveInput {
        data:
            syn::Data::Struct(syn::DataStruct {
                fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
                ..
            }),
        ..
    } = input
    {
        named
    } else {
        panic!("Expected the macro #[derive(Impl)] to be an attribute for structs");
    };

    let fields_types_ziped = fields.iter().map(|f| {
        let field_name = f.ident.clone().unwrap();
        let field_type = f.ty.clone();
        quote! {
            #field_name: #field_type
        }
    });
    let fields = fields.iter().map(|f| {
        let field_name = f.ident.clone().unwrap();
        quote! {
            #field_name
        }
    });

    let name = input.ident;

    let output = quote! {
        impl #name {
            pub fn new(#(#fields_types_ziped,)*) -> Self {
                Self {
                    #(#fields,)*
                }
            }
        }
    };
    TokenStream::from(output)
}
