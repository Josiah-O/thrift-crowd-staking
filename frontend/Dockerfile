FROM node:16

# Create app directory
WORKDIR /app

# Create a non-root user
RUN groupadd -r appuser && useradd -r -g appuser appuser

# Copy package.json and package-lock.json (if available)
COPY package*.json ./

# Copy tailwind.config.js
COPY tailwind.config.js ./

# Install dependencies as root first
RUN npm install

# Copy the rest of the application code
COPY . .

# Create and set permissions for cache directory
RUN mkdir -p /app/node_modules/.cache && \
    chown -R appuser:appuser /app && \
    chmod -R 755 /app

# Copy and set permissions for start script
COPY start.sh /start.sh
RUN chmod +x /start.sh && chown appuser:appuser /start.sh

# Switch to non-root user
USER appuser

EXPOSE 3000

CMD ["/start.sh"]
