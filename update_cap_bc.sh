git fetch
git checkout remotes/origin/cap-bc

#docker-compose stop
#docker-compose up -d --build --remove-orphans

sudo systemctl restart cap-bc-app.service
sudo systemctl restart cap-bc-homepage.service