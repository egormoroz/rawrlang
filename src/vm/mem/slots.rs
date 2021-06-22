use super::object::Object;

pub struct Slots {
    //TODO: use pool allocator or something
    data: Vec<Option<Object>>,
    //TODO: same here
    free_slots: Vec<usize>,
}

impl Slots {
    pub fn new() -> Self {
        Self {
            data: vec![],
            free_slots: vec![],
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            free_slots: Vec::with_capacity(capacity),
        }
    }

    pub fn present(&self, slot: usize) -> bool {
        match self.data.get(slot) {
            Some(Some(_)) => true,
            _ => false,
        }
    }

    pub fn add(&mut self, object: Object) -> usize {
        match self.free_slots.pop() {
            Some(idx) => {
                self.data[idx] = Some(object);
                idx
            },
            None => {
                self.data.push(Some(object));
                self.data.len() - 1
            }
        }
    }

    pub fn duplicate(&mut self, idx: usize) -> Option<usize> {
        match self.data.get(idx) {
            Some(Some(obj)) => Some(self.add(obj.clone())),
            _ => None,
        }
    }

    pub fn get(&self, idx: usize) -> Option<&Object> {
        match self.data.get(idx) {
            Some(Some(obj)) => Some(obj),
            _ => None,
        }
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut Object> {
        match self.data.get_mut(idx) {
            Some(Some(obj)) => Some(obj),
            _ => None,
        }
    }

    pub fn take(&mut self, idx: usize) -> Option<Object> {
        match self.data.get_mut(idx) {
            Some(itm) => match std::mem::take(itm) {
                Some(obj) => {
                    self.free_slots.push(idx);
                    Some(obj)
                },
                None => None,
            },
            None => None,
        }
    }

    pub fn destroy(&mut self, idx: usize) -> Option<()> {
        match self.data.get_mut(idx) {
            Some(obj @ Some(_)) => {
                obj.take();
                self.free_slots.push(idx);
                Some(())
            },
            _ => None,
        }
    }
}
