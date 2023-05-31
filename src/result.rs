//! Result types for Erland Server.

use thiserror::Error;

use crate::messaging::{PlaygroundResponse, PlaygroundResponseType};

pub type Result<T> = std::result::Result<T, self::Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected error from filesystem")]
    Filesystem,
    #[error("Unexpected error while running a shell command")]
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
