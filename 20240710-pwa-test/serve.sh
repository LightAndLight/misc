set -e

rm -rf build
mkdir build

# regular website
cp index.html build/index.html
cp install.html build/install.html
cp page1.html build/page1.html
cp page2.html build/page2.html

mkdir build/nested
cp page3.html build/nested/page3.html

# PWA
mkdir -p build/pwa
cp pwa/manifest.json build/pwa/manifest.json
cp pwa/service-worker.js build/pwa/service-worker.js
cp pwa/installing.html build/pwa/installing.html
cp pwa/icon.png build/pwa/icon.png
cp pwa/install-app.png build/pwa/install-app.png
cp pwa/install-app-confirm.png build/pwa/install-app-confirm.png

cp pwa/index.html build/pwa/index.html
cp pwa/page1.html build/pwa/page1.html
cp pwa/page2.html build/pwa/page2.html

mkdir build/pwa/nested
cp pwa/page3.html build/pwa/nested/page3.html

python3 -m http.server -d build
