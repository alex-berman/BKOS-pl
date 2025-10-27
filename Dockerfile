# Use the official SWI-Prolog base image
FROM swipl:latest

# Copy your server code into the container
WORKDIR /app
COPY *.pl initial_state.yml .

# Expose the HTTP port
EXPOSE 8080

# Run the Prolog HTTP server
CMD ["swipl", "server.pl"]
