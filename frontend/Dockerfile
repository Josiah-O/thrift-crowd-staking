FROM node:16

WORKDIR /app

# Copy package.json and package-lock.json (if available)
COPY package*.json ./

# Copy tailwind.config.js
COPY tailwind.config.js ./

# Install dependencies
RUN npm install

# Copy the rest of the application code
COPY . .

EXPOSE 3000

# Use a shell script to install new dependencies and start the app
COPY start.sh /start.sh
RUN chmod +x /start.sh

CMD ["/start.sh"]
