# Boids Simulation in Erlang

Welcome to my Boids Simulation repository! This project's goal was for me to learn Erlang/OTP through a practical example. It showcases the beauty of collective behavior with the flocking of simulated boid entities, inspired by Craig Reynolds' Boids algorithm.

## Getting Started

1. **Prerequisites**: Ensure Erlang and Rebar3 are installed on your system.
2. **Clone the Repository**: `git clone https://github.com/leventekoor/boids_simulation.git`
3. **Compile**: `cd boids_simulation` then `rebar3 compile`
4. **Start the Simulation**: `rebar3 shell`
5. **View the Simulation**: Open your browser and visit [http://localhost/8080](http://localhost/8080)

## Debug Mode

Debug mode prints information about the handling of the child processes during the simulation.

To start the application in **_debug mode_** use the _"debug"_ rebar profile: `rebar3 as debug shell`

## Running Tests

The tests validate the functionality of the simulation, they cover the algorithms _cohesion_, _alignment_, and _separation_. These three fundamental algorithms constitute the Boids flocking behavior.
You can run the tests with `rebar3 eunit`.

## See It In Action

![Boid simulation running.](/docs/simulation_running.gif)

## Architecture

The application initializes a Cowboy web server responsible for serving the static frontend page and managing incoming HTTP requests governing the simulation. A primary supervisor is launched, subsequently initiating a simulation supervisor. This supervisor dynamically spawns boid gen_servers using a simple_one_for_one strategy and manages updating and polling their states.

## Acknowledgments

Special thanks to Craig Reynolds for his groundbreaking work on the Boids algorithm, which inspired this project.
