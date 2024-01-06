import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

def lorentzian_inner_product(v, w):
    """ Compute the Lorentzian inner product of two vectors in 2D Minkowski space. """
    return v[0] * w[0] - v[1] * w[1]

def minkowski_orthogonal(v):
    """ Find a vector that is Minkowski orthogonal to the given vector v in 2D Minkowski space. """
    return np.array([v[1], v[0]])

# Time range for the simulation
t = np.linspace(0, 2, 200)

# Trajectories
a_t = np.array([np.cosh(t), np.sinh(t)])
b_t = np.array([np.cosh(t) + 1, np.sinh(t)])

# Tangent vectors
a_tangent = np.array([np.sinh(t), np.cosh(t)])
b_tangent = np.array([np.sinh(t), np.cosh(t)])

# Minkowski orthogonal vectors
a_orthogonal = np.array([minkowski_orthogonal(v) for v in a_tangent.T]).T
b_orthogonal = np.array([minkowski_orthogonal(v) for v in b_tangent.T]).T

# Setting up the figure
fig, ax = plt.subplots(figsize=(12, 8))
ax.set_xlim(-20, 20)
ax.set_ylim(-20, 20)
ax.grid(True)
ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_title("Trajectories and Tangent Vectors in 2D Special Relativity (t=0 to t=2)")

# Plot trajectories
line_a, = ax.plot([], [], label="Trajectory of Particle A (a(t))", lw=2)
line_b, = ax.plot([], [], label="Trajectory of Particle B (b(t))", lw=2)

# Initialize tangent vectors and orthogonal lines
quiver_a = ax.quiver([], [], [], [], color="blue", angles='xy', scale_units='xy', scale=1, width=0.005)
quiver_b = ax.quiver([], [], [], [], color="red", angles='xy', scale_units='xy', scale=1, width=0.005)
line_a_orthogonal, = ax.plot([], [], color="blue", linestyle='--', linewidth=1)
line_b_orthogonal, = ax.plot([], [], color="red", linestyle='--', linewidth=1)

def init():
    line_a.set_data([], [])
    line_b.set_data([], [])
    quiver_a.set_UVC([], [], [])
    quiver_b.set_UVC([], [], [])
    line_a_orthogonal.set_data([], [])
    line_b_orthogonal.set_data([], [])
    return line_a, line_b, quiver_a, quiver_b, line_a_orthogonal, line_b_orthogonal

def animate(i):
    line_a.set_data(a_t[0, :i+1], a_t[1, :i+1])
    line_b.set_data(b_t[0, :i+1], b_t[1, :i+1])
    quiver_a.set_UVC(a_tangent[0, i], a_tangent[1, i])
    quiver_b.set_UVC(b_tangent[0, i], b_tangent[1, i])

    a_orthogonal_line_end = a_t[:, i] + 10 * a_orthogonal[:, i]
    a_orthogonal_line_start = a_t[:, i] - 10 * a_orthogonal[:, i]
    b_orthogonal_line_end = b_t[:, i] + 10 * b_orthogonal[:, i]
    b_orthogonal_line_start = b_t[:, i] - 10 * b_orthogonal[:, i]

    line_a_orthogonal.set_data([a_orthogonal_line_start[0], a_orthogonal_line_end[0]], 
                               [a_orthogonal_line_start[1], a_orthogonal_line_end[1]])
    line_b_orthogonal.set_data([b_orthogonal_line_start[0], b_orthogonal_line_end[0]], 
                               [b_orthogonal_line_start[1], b_orthogonal_line_end[1]])

    return line_a, line_b, quiver_a, quiver_b, line_a_orthogonal, line_b_orthogonal

ani = animation.FuncAnimation(fig, animate, init_func=init, frames=len(t), interval=50, blit=True)

plt.legend()
plt.show()

