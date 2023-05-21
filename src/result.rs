//! Result types for Erland Server.

use crate::messaging::{PlaygroundResponse, PlaygroundResponseType};

use thiserror::Error;

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
    #[error("Playground name can only contain ascii alphanumeric characters and dot (.)")]
    Format,
}

impl Error {
    /// Convert [`self::Error`] to [`PlaygroundResponse`].
    pub fn to_response(&self, id: String) -> PlaygroundResponse {
        PlaygroundResponse {
            id,
            r#type: PlaygroundResponseType::Error,
            data: Some(self.to_string()),
        }
    }
}
