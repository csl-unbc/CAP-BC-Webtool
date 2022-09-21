git fetch
git checkout  remotes/origin/cap-bc
docker-compose stop
docker-compose up -d --build --remove-orphans