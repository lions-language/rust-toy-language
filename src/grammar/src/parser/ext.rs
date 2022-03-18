pub trait ArrayFinder<T>
where
    T: Unique,
{
    fn key_contains<Key>(&self, key: &Key) -> bool
    where
        Key: PartialEq<<T as Unique>::T> + ?Sized;
    fn find<Key>(&self, key: &Key) -> Option<&T>
    where
        Key: PartialEq<<T as Unique>::T> + ?Sized;
    fn find_clone<Key>(&self, key: &Key) -> Option<T>
    where
        Key: PartialEq<<T as Unique>::T> + ?Sized,
        T: Clone;
}

pub trait Unique {
    type T;

    fn unique(&self) -> &Self::T;
}

impl<T> ArrayFinder<T> for Vec<T>
where
    T: Unique,
{
    fn key_contains<Key>(&self, key: &Key) -> bool
    where
        Key: PartialEq<<T as Unique>::T> + ?Sized,
    {
        self.find(key).is_some()
    }

    fn find<Key>(&self, key: &Key) -> Option<&T>
    where
        Key: PartialEq<<T as Unique>::T> + ?Sized,
    {
        self.iter().find(|&item| key == item.unique())
    }

    fn find_clone<Key>(&self, key: &Key) -> Option<T>
    where
        Key: PartialEq<<T as Unique>::T> + ?Sized,
        T: Clone,
    {
        match self.find(key) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    impl Unique for u32 {
        type T = u32;

        fn unique(&self) -> &u32 {
            self
        }
    }

    struct Student {
        id: String,
        name: String,
    }

    impl Unique for Student {
        type T = String;

        fn unique(&self) -> &String {
            &self.id
        }
    }

    #[test]
    fn test_vec_impl_arrayfinder() {
        let v = vec![1, 2, 3, 4];
        let r = v.find(&4);
        assert!(r.is_some());

        let v = vec![
            Student {
                id: "001".into(),
                name: "jake".into(),
            },
            Student {
                id: "002".into(),
                name: "mike".into(),
            },
        ];
        let r = v.find("001");
        assert!(r.is_some());
    }
}
