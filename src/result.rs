use crate::messaging::PlaygroundResponse;

use axum::{http::StatusCode, Json};
use thiserror::Error;

pub type Result<T> = std::result::Result<T, self::Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexcepted error from filesystem")]
    FsError,
    #[error("Unexcepted error while running a shell command")]
    CmdError,
}

impl Error {
    pub fn as_response(self) -> (StatusCode, Json<PlaygroundResponse>) {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(PlaygroundResponse::Error(self.to_string())),
        )
    }
}
