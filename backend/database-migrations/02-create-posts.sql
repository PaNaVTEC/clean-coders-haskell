CREATE TABLE IF NOT EXISTS posts (
  postId char(36) PRIMARY KEY,
  userId char(36) NOT NULL,
  text text,
  date TIMESTAMP WITH TIME ZONE
)
