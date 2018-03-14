
ActiveRecord::Base.establish_connection(
    adapter: "postgresql",
    host: "db",
    username: "postgres",
    database: "test",
    port: "5432"
)

