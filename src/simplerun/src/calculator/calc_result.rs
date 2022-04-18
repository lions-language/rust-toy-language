use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use educe::Educe;

use grammar::StructDef;
use utils::{Shared, SharedCell, Weaked};

#[derive(Educe, Clone, derive_new::new)]
#[educe(Debug)]
pub struct StructValue {
    fields: HashMap<String, Box<CalcResult>>,
    #[educe(Debug(ignore))]
    pub struct_def: Weaked<StructDef>,
}

impl Default for StructValue {
    fn default() -> Self {
        Self {
            fields: HashMap::new(),
            struct_def: Weaked::default(),
        }
    }
}

impl StructValue {
    pub fn insert(&mut self, name: String, value: impl Into<CalcResult>) {
        self.fields.insert(name, Box::new(value.into()));
    }
}

impl StructValue {
    pub fn find_member<N>(&self, name: N) -> Box<CalcResult>
    where
        N: AsRef<str> + std::fmt::Display,
    {
        match self.fields.get(name.as_ref()) {
            Some(result) => result.clone(),
            None => panic!("the {} field could not be found in the structure", name),
        }
    }

    pub fn find<'a, Item, T: ?Sized>(&self, t: &'a mut T) -> Box<CalcResult>
    where
        Item: AsRef<str> + std::fmt::Display,
        T: Iterator<Item = Item>,
    {
        let mut calc_result = match t.next() {
            Some(name) => match self.fields.get(name.as_ref()) {
                Some(result) => result.clone(),
                None => panic!("the {} field could not be found in the structure", name),
            },
            None => panic!("no element exists in the given iterator"),
        };

        while let Some(field) = t.next() {
            match calc_result.deref() {
                CalcResult::Value(CalcValue::Struct(struct_value)) => {
                    calc_result = match struct_value.fields.get(field.as_ref()) {
                        Some(v) => v.clone(),
                        None => panic!("the {} field could not be found in the structure", field),
                    };
                }
                other => panic!(
                    "want to access a field, but the previous calculation result type: {:?} does not contain any fields", 
                    other),
            }
        }

        calc_result
    }
}

impl ToString for StructValue {
    fn to_string(&self) -> String {
        format!("{:?}", self.fields)
    }
}

impl From<StructValue> for CalcResult {
    fn from(v: StructValue) -> Self {
        CalcResult::Value(v.into())
    }
}

impl From<String> for CalcResult {
    fn from(v: String) -> Self {
        CalcResult::Value(v.into())
    }
}

impl From<u32> for CalcResult {
    fn from(v: u32) -> Self {
        CalcResult::Value(v.into())
    }
}

#[derive(Clone, Debug)]
pub enum CalcValue {
    U32(u32),
    U64(u64),
    Bool(bool),
    String(String),
    Struct(StructValue),
}

impl From<StructValue> for CalcValue {
    fn from(v: StructValue) -> Self {
        CalcValue::Struct(v)
    }
}

impl From<String> for CalcValue {
    fn from(v: String) -> Self {
        CalcValue::String(v)
    }
}

impl From<u32> for CalcValue {
    fn from(v: u32) -> Self {
        CalcValue::U32(v)
    }
}

impl std::ops::Add for CalcValue {
    type Output = CalcValue;

    fn add(self, rhs: CalcValue) -> Self::Output {
        match &self {
            CalcValue::U32(v1) => match &rhs {
                CalcValue::U32(v2) => CalcValue::U32(v1 + v2),
                other => {
                    unimplemented!("{:?} + {:?}", self, rhs);
                }
            },
            CalcValue::U64(v1) => match &rhs {
                CalcValue::U64(v2) => CalcValue::U64(v1 + v2),
                other => {
                    unimplemented!("{:?} + {:?}", self, rhs);
                }
            },
            other => {
                unimplemented!("{:?} + ...", other);
            }
        }
    }
}

impl std::cmp::PartialEq<Self> for CalcValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            CalcValue::U64(v1) => match other {
                CalcValue::U64(v2) => v1 == v2,
                other => {
                    unimplemented!("{:?} == {:?}", self, other);
                }
            },
            CalcValue::U32(v1) => match other {
                CalcValue::U32(v2) => v1 == v2,
                other => {
                    unimplemented!("{:?} == {:?}", self, other);
                }
            },
            other => {
                unimplemented!("{:?} == ...", other);
            }
        }
    }
}

impl std::cmp::PartialOrd<Self> for CalcValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            CalcValue::U32(v1) => match other {
                CalcValue::U32(v2) => {
                    if v1 < v2 {
                        Some(std::cmp::Ordering::Less)
                    } else if v1 > v2 {
                        Some(std::cmp::Ordering::Greater)
                    } else {
                        Some(std::cmp::Ordering::Equal)
                    }
                }
                other => {
                    unimplemented!("{:?} < {:?}", self, other);
                }
            },
            CalcValue::U64(v1) => match other {
                CalcValue::U64(v2) => {
                    if v1 < v2 {
                        Some(std::cmp::Ordering::Less)
                    } else if v1 > v2 {
                        Some(std::cmp::Ordering::Greater)
                    } else {
                        Some(std::cmp::Ordering::Equal)
                    }
                }
                other => {
                    unimplemented!("{:?} < {:?}", self, other);
                }
            },
            other => {
                unimplemented!("{:?} == ...", other);
            }
        }
    }
}

impl ToString for CalcValue {
    fn to_string(&self) -> String {
        match self {
            CalcValue::U32(v) => v.to_string(),
            CalcValue::U64(v) => v.to_string(),
            CalcValue::Bool(v) => v.to_string(),
            CalcValue::String(v) => v.clone(),
            CalcValue::Struct(v) => v.to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum CalcResult {
    Value(CalcValue),
    Return(Box<CalcResult>),
    Break,
    Continue,
    Unit,
    Unknown,
}

impl CalcResult {
    pub fn is_true(&self) -> bool {
        if let CalcResult::Value(CalcValue::Bool(true)) = self {
            true
        } else {
            false
        }
    }
}

impl std::ops::Add for CalcResult {
    type Output = CalcResult;

    fn add(self, rhs: CalcResult) -> Self::Output {
        match self {
            CalcResult::Value(v1) => match rhs {
                CalcResult::Value(v2) => CalcResult::Value(v1 + v2),
                _ => {
                    unimplemented!();
                }
            },
            _ => CalcResult::Unknown,
        }
    }
}

impl std::cmp::PartialEq<Self> for CalcResult {
    fn eq(&self, other: &Self) -> bool {
        match self {
            CalcResult::Value(v1) => match other {
                CalcResult::Value(v2) => v1 == v2,
                _ => {
                    unimplemented!();
                }
            },
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd<Self> for CalcResult {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            CalcResult::Value(v1) => match other {
                CalcResult::Value(v2) => {
                    if v1 < v2 {
                        Some(std::cmp::Ordering::Less)
                    } else if v1 > v2 {
                        Some(std::cmp::Ordering::Greater)
                    } else {
                        Some(std::cmp::Ordering::Equal)
                    }
                }
                other => {
                    unimplemented!("{:?} < {:?}", self, other);
                }
            },
            other => {
                unimplemented!("{:?} == ...", other);
            }
        }
    }
}

impl ToString for CalcResult {
    fn to_string(&self) -> String {
        match self {
            CalcResult::Value(v) => v.to_string(),
            CalcResult::Return(v) => v.to_string(),
            CalcResult::Unit => "unit".to_string(),
            other => {
                unimplemented!("{:?}", other);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn struct_value_test() {
        let mut dog_struct_object = StructValue::default();
        dog_struct_object.insert("name".into(), "Mr Huang".to_string());
        dog_struct_object.insert("age".into(), "5".to_string());

        let mut master_struct_object = StructValue::default();
        master_struct_object.insert("dog".into(), dog_struct_object);

        let mut field_access: Vec<String> = vec!["dog".into(), "name".into()];
        let calc_result = master_struct_object.find(&mut field_access.into_iter());
        println!("{:?}", calc_result);
    }
}
