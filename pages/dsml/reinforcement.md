name: Reinforcement learning for navigation
notebook: reinforcement.html

<figure>
<img src="{{ url_for('static', filename='img/dsml/rl-agent.png') }}"
	 alt="3-by-4 grid with arrows in each cell indicating the direction taken by the optimal policy."
     class="centered">
<figcaption>Visualisation of a navigation task and the optimal policy.</figcaption>
</figure>

An implementation of value iteration to find the optimal policy for reinforcement learning problems. In the scenario depicted above, the agent is placed 3 cells across and 3 cells down in a 3-by-4 grid. The top-right corner is the goal state, giving a reward of +1. Below that is a fail state, giving a reward of -1. Every other state gives -0.1, incentivising the agent to not dilly-dally. The arrows indicate the direction to move, as proposed by the optimal policy. There's a 75% chance of moving in the intended direction, and a 25% chance of veering left or right. This is why, at grid position (2,3), the optimal decision is to go left, avoiding the possibility of accidentally reaching the fail state.
