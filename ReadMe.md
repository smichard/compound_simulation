# Overview

The “Portfolio Growth with Monte Carlo Simulation” Shiny app lets you explore how your portfolio could evolve over time under uncertainty. You provide assumptions such as initial capital, monthly savings, expected annual return (μ), and volatility (σ). The app runs Monte Carlo simulations to visualize potential outcomes and uncertainty bands, and contrasts a deterministic expectation path with the simulated median path.

It also supports a stress test (halve μ, double σ), a goal tracker (target value by a target date), and a savings-elasticity view to estimate how increasing monthly savings affects median outcomes.

Note: This is a theoretical model based on your inputs and random simulations. Real markets can deviate materially.

Container image: [Quay link](https://quay.io/repository/michard/compound_simulation)   
App: [Demo link](https://compound-simulation-139537757990.europe-north1.run.app)

## Input Parameters
- Initial Capital [€]: Starting portfolio value.
- Monthly Savings [€]: Recurring monthly contribution.
- Annual Return (μ, p.a.): Expected mean return per year.
- Volatility (σ, p.a.): Annual standard deviation of returns.
- Savings Elasticity (Δ €/month): Increment used to test uplift from higher savings.
- Stress Test: Halves μ and doubles σ across the full horizon.
- Time Horizon [years]: Total investment period.
- Number of Simulations: Monte Carlo paths to generate.
- Target Value [€]: Goal value to reach or exceed.
- Target Date: Date by which to reach the goal.

## Generated Diagrams & Metrics
1. Fan Chart (Portfolio Value):  
Median path, deterministic expectation path, and uncertainty ribbons (25–75% and 5–95%). Target value/date lines are highlighted.

2. End-Value Distribution:  
Density plot of end values at the horizon with markers for median, mean, and target value, plus shaded probability bands.

3. Target Probability:  
Probability of reaching at least the target value on the target date.

4. Savings Elasticity (Horizon Uplift):  
Estimated increase in median end value if monthly savings are raised by the elasticity amount.

## How to Use

Enter initial capital, monthly savings, μ, and σ.  
(Optional) Enable the stress test and/or set a savings elasticity increment.  
Choose the time horizon, number of simulations, target value, and target date.  
Review the charts and metrics to understand expected growth, uncertainty, goal probability, and savings sensitivity.

## Run App Locally

1. Clone this repository:
```bash
git clone https://github.com/smichard/compound_simulation
```

2. Navigate to the project directory:
```bash
cd compound_simulation
```

3. Build the container image (uses the provided Containerfile):
```bash
podman build -t my_app -f Containerfile .
```

4. Run the Shiny app locally:
```bash
podman run --rm -p 3838:3838 my_app
```

5. Open the app in your browser:  
http://localhost:3838/