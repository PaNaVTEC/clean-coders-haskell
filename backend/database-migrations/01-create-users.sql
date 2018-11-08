CREATE TABLE users (
  userId char(36) PRIMARY KEY,
  userName varchar(100) NOT NULL,
  about text,
  password varchar(500) NOT NULL,

  UNIQUE(userName)
)
