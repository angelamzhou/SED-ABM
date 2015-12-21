###############################
# Parameter Initialization
n = 100
# committed believers
minority_p = 0.16
minority_n = round(0.16*n)
minority_theta_0 = 1 # 0 => always responds with positive belief
minority_ab.x = c(1,1)

# committed disbelievers
minority_2_n = round(0.13*n)
minority_2_theta_0 = 0
minority_2_ab.x = c(0,1)

majority_n = n - minority_n - minority_2_n
majority_theta_0 = 0.5
majority_ab.x = c(0.8669, 1.121)

agents = 1:n

minority = agents[0:minority_n] 
minority_2 = agents[minority_n:(minority_n+minority_2_n)]
majority = agents[(minority_n+minority_2_n):n]

agent_beliefs = data.frame(agents)
agent_beliefs$type <- c(replicate(minority_n, 'minority'), replicate(minority_2_n, 'minority_2'), replicate(majority_n, 'majority'))
agent_beliefs$ab <- c(replicate(minority_n, list(minority_ab.x)), replicate(minority_2_n, list(minority_2_ab.x)), replicate(majority_n, list(majority_ab.x)))
agent_beliefs$theta <- c(replicate(minority_n, minority_theta_0), replicate(minority_2_n, minority_2_theta_0), replicate(majority_n, majority_theta_0))


# Helper function for pairwise belief updating 
# Update the beliefs of agent by the beliefs of agent 2
# Assuming beta-binomial 
update_beliefs <- function(agent, agent2, agent_beliefs, t) {
  if ((agent_beliefs$type[agent] == 'minority') || (agent_beliefs$type[agent] == 'minority_2'))
    return(agent_beliefs)
  # update beliefs depending on type of other agent
  else if(agent_beliefs$type[agent2] == 'minority') { 
    agent_beliefs$ab[agent][[1]][1] = agent_beliefs$ab[agent][[1]][1]+ 1 }
  
  else if (agent_beliefs$type[agent2] == 'minority_2')  {
    agent_beliefs$ab[agent][[1]][2] = agent_beliefs$ab[agent][[1]][2] + 1
  }
  # update: don't update if interacting w majority
  #    else: 
  #         agent_beliefs[agent]['ab'][0] += agent_beliefs[agent2]['theta'] 
  #         agent_beliefs[agent]['ab'][1] += (1- agent_beliefs[agent2]['theta'])
  
  agent_beliefs$theta[agent] = 1.0 * agent_beliefs$ab[agent][[1]][1]/ (agent_beliefs$ab[agent][[1]][1] + agent_beliefs$ab[agent][[1]][2])
  return(agent_beliefs)  
}
update_belief_assignment <- function(agent_dict) {
  minority_n = 0
  minority_2_n = 0 
  majority_n = 0 
  for (agent in dim(agent_dict)[1]) {
    if (agent_dict$theta[agent] > 0.8) {
      agent_dict$type[agent] = 'minority'
      
      minority_n = minority_n + 1}
    if (agent_dict$theta[agent] < 0.2) {
      agent_dict$type[agent] = 'minority_2'
      
      minority_2_n = minority_2_n + 1
    }
    else
      majority_n = majority_n + 1
  }
  return(list(agent_beliefs,c(minority_n, minority_2_n, majority_n)))
}


T = 1000
# number of interactions at every round
t_round = 60
n_agents = 2 # number of agents that interact
group_n = matrix(nrow=T, ncol=3)

# keep track of hyperparameters
mean_maj_beliefs = replicate(T, 0)

# sd of belief? 
std_maj_beliefs = replicate(T, 0)

for (t in 1:T) {
  # randomly select two agents 
  interaction_agents = sample(agents, n_agents)
  # need to generalize this better
  agent = interaction_agents[1]
  agent2 = interaction_agents[2]
  if (agent_beliefs$type[agent] == 'majority' || agent_beliefs$type[agent2] == 'majority') {
    # update each by the other 
    # sample belief probability to get success probability 
    # of an update 
    if (agent_beliefs$type[agent] == 'majority') {
      agent_beliefs = update_beliefs(agent, agent2, agent_beliefs, t)
    }
    if  (agent_beliefs$type[agent2] == 'majority') {
      agent_beliefs = update_beliefs(agent2, agent, agent_beliefs, t)
    }
  }
  # keep track of statistics
  
  mean_maj_beliefs[t] = mean(agent_beliefs$theta[(agent_beliefs$type == 'majority')])
  std_maj_beliefs[t] = sd(agent_beliefs$theta[(agent_beliefs$type == 'majority')])
  # move people into minority if their beliefs have migrated 
  output = update_belief_assignment(agent_beliefs)
  agent_beliefs = output[[1]]
  group_n[t,1] = output[[2]][1]
  group_n[t,2] = output[[2]][2]
  group_n[t,3] = output[[2]][3]
}
