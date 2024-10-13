FROM node:16

WORKDIR /app

# Copy package.json and package-lock.json (if available)
COPY package*.json ./

# Copy tailwind.config.js
COPY tailwind.config.js ./

# Clear npm cache and install dependencies
RUN npm cache clean --force
RUN npm install

# Copy the rest of the application code
COPY . .

# Build the application
RUN npm install
RUN npm run build

EXPOSE 3000

CMD ["npm", "start"]
