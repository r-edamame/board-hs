
#!/bin/bash

bundle exec rake db:migrate

bundle exec ruby app.rb -o 0.0.0.0 -p 3000
