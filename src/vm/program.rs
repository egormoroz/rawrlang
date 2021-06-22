#[derive(Debug)]
pub struct Function {
    pub name: u8,
    pub anary: u8,
    pub ip: usize,
    pub len: usize,
}

#[derive(Debug)]
pub struct Program {
    //-------header-------
    str_atlas: Vec<(usize, usize)>,
    str_data: String,

    //the first function is always entry
    fn_table: Vec<Function>,

    //-------code---------
    
    code: Vec<u8>,
}

impl Program {
    pub fn from_bytes(_: &[u8]) -> Self {
        todo!()
    }

    pub fn new(str_atlas: Vec<(usize, usize)>, str_data: String, fn_table: Vec<Function>, code: Vec<u8>) -> Self {
        //TODO: make sure that str_atlas and utf8_data are valid
        Self {
            str_atlas,
            str_data,
            fn_table,
            code
        }
    }

    pub fn get_const_str(&self, id: u8) -> Option<&str> {
        match self.str_atlas.get(id as usize) {
            Some((p, q)) => Some(&self.str_data[*p..*q]),
            None => None,
        }
    }

    pub fn num_of_consts(&self) -> usize {
        self.str_atlas.len()
    }

    pub fn get_fn_info(&self, idx: usize) -> Option<&Function> {
        self.fn_table.get(idx)
    }

    pub fn num_of_fns(&self) -> usize{ 
        self.fn_table.len()
    }

    pub fn get_fn_code(&self, idx: usize) -> Option<&[u8]> {
        if let Some(f) = self.fn_table.get(idx) {
            return Some(&self.code[f.ip..f.ip+f.len])
        }
        None
    }
}
