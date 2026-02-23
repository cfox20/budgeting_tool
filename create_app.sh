#!/bin/bash

APP_NAME="Household Budgeting"
APP_DIR="${APP_NAME}.app"
ICON_SOURCE="$1"

echo "Building $APP_NAME..."

# 1. Create directory structure
mkdir -p "$APP_DIR/Contents/MacOS"
mkdir -p "$APP_DIR/Contents/Resources"

# 2. Create Info.plist
echo "Creating Info.plist..."
cat > "$APP_DIR/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key>
    <string>launcher</string>
    <key>CFBundleIconFile</key>
    <string>AppIcon</string>
    <key>CFBundleIdentifier</key>
    <string>com.slater.budgeting</string>
    <key>CFBundleName</key>
    <string>${APP_NAME}</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0</string>
    <key>CFBundleVersion</key>
    <string>1</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.10</string>
    <key>LSUIElement</key>
    <false/>
</dict>
</plist>
EOF

# 3. Create Launcher Script
# The launcher will reside in App.app/Contents/MacOS/
# We use the ABSOLUTE path captured at build time to avoid relative path issues
# or App Translocation problems.
PROJECT_ROOT="$(pwd)"

# Run the app via Terminal to ensure permissions and visibility
# Using osascript avoids "Operation not permitted" by leveraging Terminal's access
cat > "$APP_DIR/Contents/MacOS/launcher" <<EOF
#!/bin/bash

# Hardcoded project path
PROJECT_DIR="$PROJECT_ROOT"

# Locate Rscript
if [ -f /usr/local/bin/Rscript ]; then
    HS_RSCRIPT="/usr/local/bin/Rscript"
elif [ -f /opt/homebrew/bin/Rscript ]; then
    HS_RSCRIPT="/opt/homebrew/bin/Rscript"
else
    HS_RSCRIPT="/usr/bin/env Rscript"
fi

# Use AppleScript to tell Terminal to launch the app
# This ensures we inherit Terminal's file permissions
osascript <<EOD
tell application "Terminal"
    do script "cd '\$PROJECT_DIR' && '\$HS_RSCRIPT' run_app.R"
    activate
end tell
EOD
EOF

chmod +x "$APP_DIR/Contents/MacOS/launcher"

# 4. Handle Icon
if [ -n "$ICON_SOURCE" ] && [ -f "$ICON_SOURCE" ]; then
    echo "Processing icon..."
    mkdir -p MyIcon.iconset
    sips -z 16 16     -s format png "$ICON_SOURCE" --out MyIcon.iconset/icon_16x16.png > /dev/null
    sips -z 32 32     -s format png "$ICON_SOURCE" --out MyIcon.iconset/icon_16x16@2x.png > /dev/null
    sips -z 32 32     -s format png "$ICON_SOURCE" --out MyIcon.iconset/icon_32x32.png > /dev/null
    sips -z 64 64     -s format png "$ICON_SOURCE" --out MyIcon.iconset/icon_32x32@2x.png > /dev/null
    sips -z 128 128   -s format png "$ICON_SOURCE" --out MyIcon.iconset/icon_128x128.png > /dev/null
    sips -z 256 256   -s format png "$ICON_SOURCE" --out MyIcon.iconset/icon_128x128@2x.png > /dev/null
    sips -z 512 512   -s format png "$ICON_SOURCE" --out MyIcon.iconset/icon_512x512.png > /dev/null
    sips -z 1024 1024 -s format png "$ICON_SOURCE" --out MyIcon.iconset/icon_512x512@2x.png > /dev/null
    
    iconutil -c icns MyIcon.iconset
    mv MyIcon.icns "$APP_DIR/Contents/Resources/AppIcon.icns"
    rm -rf MyIcon.iconset
    echo "Icon applied."
else
    echo "Warning: Icon source not found or no icon provided."
fi

echo "Done. App bundle created at $(pwd)/$APP_DIR"
