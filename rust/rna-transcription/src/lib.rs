#[derive(Debug, PartialEq)]
enum Dna {
    A,
    C,
    G,
    T,
}

#[derive(Debug, PartialEq)]
enum Rna {
    C,
    G,
    A,
    U,
}

#[derive(Debug, PartialEq)]
pub struct DNA {
    strand: Vec<Dna>,
}

#[derive(Debug, PartialEq)]
pub struct RNA {
    strand: Vec<Rna>,
}

impl Dna {
    fn parse((index, dna): (usize, char)) -> Result<Dna, usize> {
        match dna {
            'A' => Ok(Dna::A),
            'C' => Ok(Dna::C),
            'G' => Ok(Dna::G),
            'T' => Ok(Dna::T),
            _ => Err(index),
        }
    }

    fn into_rna(self) -> Rna {
        match self {
            Dna::G => Rna::C,
            Dna::C => Rna::G,
            Dna::T => Rna::A,
            Dna::A => Rna::U,
        }
    }
}

impl Rna {
    fn parse((index, rna): (usize, char)) -> Result<Rna, usize> {
        match rna {
            'C' => Ok(Rna::C),
            'G' => Ok(Rna::G),
            'A' => Ok(Rna::A),
            'U' => Ok(Rna::U),
            _ => Err(index),
        }
    }
}

impl DNA {
    pub fn new(dna: &str) -> Result<DNA, usize> {
        let strand: Vec<Dna> = generate_strand(dna, Dna::parse)?;
        Ok(DNA { strand })
    }

    pub fn into_rna(self) -> RNA {
        let strand: Vec<Rna> =
            self.strand.into_iter().map(Dna::into_rna).collect();
        RNA { strand }
    }
}

impl RNA {
    pub fn new(rna: &str) -> Result<RNA, usize> {
        let strand: Vec<Rna> = generate_strand(rna, Rna::parse)?;
        Ok(RNA { strand })
    }
}

fn generate_strand<T>(
    string: &str,
    parse: fn((usize, char)) -> Result<T, usize>,
) -> Result<Vec<T>, usize> {
    string.char_indices().map(parse).collect()
}
