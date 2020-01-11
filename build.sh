date | tee build_logs.log
echo "building optimized" | tee build_logs.log
elm make ~/chapliboy/cluegrid/src/Main.elm --optimize --output=/home/samhattangady/chapliboy/cluegrid/dist/cluegrid_clean.js >> build_logs.log
echo "uglifying" | tee build_logs.log
uglifyjs ~/chapliboy/cluegrid/dist/cluegrid_clean.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=/home/samhattangady/chapliboy/cluegrid/dist/cluegrid.js >> build_logs.log
echo "remove prettified" | tee build_logs.log
rm /home/samhattangady/chapliboy/cluegrid/dist/cluegrid_clean.js >> build_logs.log
echo "copy html, css to dist" | tee build_logs.log
cp ~/chapliboy/cluegrid/public/index.html ~/chapliboy/cluegrid/dist/index.html >> build_logs.log
cp ~/chapliboy/cluegrid/public/styles.css ~/chapliboy/cluegrid/dist/styles.css >> build_logs.log
echo "zipping build" | tee build_logs.log
zip -r dist.zip dist >> build_logs.log
echo "transferring build" | tee build_logs.log
scp -r dist.zip owlery_prealpha: >> build_logs.log
echo "preparing machine" | tee build_logs.log
ssh owlery_prealpha rm -rf dist >> build_logs.log
echo "unzipping build" | tee build_logs.log
ssh owlery_prealpha unzip dist.zip >> build_logs.log
echo "cleaning up machung" | tee build_logs.log
ssh owlery_prealpha rm -rf dist.zip >> build_logs.log
ssh owlery_prealpha rm -rf home >> build_logs.log
rm -rf dist.zip >> build_logs.log
echo "done" | tee build_logs.log
date | tee build_logs.log
