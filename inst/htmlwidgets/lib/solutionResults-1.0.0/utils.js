function newSolution(manager, x) {
  return new Solution(
    manager,
    x.id,
    x.name,
    x.parameters,
    x.statistics,
    x.theme_results,
    x.weight_results,
    x.solution_color
  );
}

function newStatistics(manager, x) {
  return new Statistics(
    manager,
    x.map((y) => y.name),
    x.map((y) => y.value),
    x.map((y) => y.units),
    x.map((y) => y.proportion)
  );
}

function newParameters(manager, x) {
  return new Parameters(
    manager,
    x.map((y) => y.id),
    x.map((y) => y.name),
    x.map((y) => y.status),
    x.map((y) => y.value),
    x.map((y) => y.min_value),
    x.map((y) => y.max_value),
    x.map((y) => y.step_value),
    x.map((y) => y.hide),
    x.map((y) => y.units)
  );
}

function newWeightResults(manager, x, solution_color) {
  return new WeightResults(
    manager,
    x.id,
    x.name,
    x.status,
    x.factor,
    x.total_amount,
    x.current_held,
    x.solution_held,
    x.units || "units",
    solution_color
  );
}

function newThemeResults(manager, x, solution_color) {
  if (typeof(x.feature_name) === "string") {
    var y = new SingleThemeResults(
      manager,
      x.id,
      x.name,
      x.feature_name,
      x.feature_id,
      x.feature_status,
      x.feature_total_amount,
      x.feature_current_held,
      x.feature_goal,
      x.feature_solution_held,
      x.units || "units",
      x.mandatory,
      x.round,
      x.icon,
      solution_color
    );
  } else {
    var y = new MultiThemeResults(
      manager,
      x.id,
      x.name,
      x.feature_name,
      x.feature_id,
      x.feature_status,
      x.feature_total_amount,
      x.feature_current_held,
      x.feature_goal,
      x.feature_solution_held,
      x.units || "units",
      x.mandatory,
      x.round,
      x.icon,
      solution_color
    );
  }
  return y;
}
