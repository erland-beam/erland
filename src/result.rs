use thiserror::Error;

use crate::messaging::{PlaygroundResponse, PlaygroundResponseType};

pub type Result<T> = std::result::Result<T, self::Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexcepted error from filesystem")]
    Filesystem,
    #[error("Unexcepted error while running a shell command")]
    Command,
    #[error("Playground already exists")]
    Exist,
    #[error("Playground doesn't exists")]
    NotExist,
}

impl Error {
    pub fn to_response(&self, id: String) -> PlaygroundResponse {
        PlaygroundResponse {
            id,
            r#type: PlaygroundResponseType::Error,
            data: Some(self.to_string()),
        }
    }
}
