# Use an official Node runtime as the parent image
FROM node:16

# Set the working directory in the container
WORKDIR /app

# Copy package.json, package-lock.json, and .npmrc
COPY package*.json .npmrc ./

# Install dependencies
RUN npm install

# Copy the rest of the application's source code
COPY . .

# Build the app
RUN npm run build

# Expose the port the app runs on
EXPOSE 3000

# Define the command to run the app
CMD ["npm", "start"]
