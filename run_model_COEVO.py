#### Co-evolutionary model of dissemination of culture ########
###############################################################
# by José Segovia Martín
# Universidad Autónoma de Barcelona
# ISC-PIF (Centre National de la Recherche Scientifique, France)
###############################################################
#### A model to simulate the emergence of conventions:
#### You can systematically manipulate:
#### Agents' value systems, content bias, compliance bias, conformity bias and confirmation bias
#### Agent's memory size
#### Innovation rate
#### Degree of homogeneirty and hegemony of value systems
#### Institutional power
#### Population size and pool of variants
#### Type of replacement in the initial set of variants
#### Number of samples and simulations
###############################################################
# The model includes:
#
# 1. class Agent: it defines an __init__() method, so that class instantiation automatically invokes __init__().
# Arguments given to the class instantiation operator are passed on to __init__():
# (self, name, signals, sigma, cont, coord, conform, confirm, mut, menLen).
# In each simulation and sample, each agent has an unique name (or ID), an unique value system (named "sigma"),
# but the same content, coordination, conformity ("compliance" in the published work) and confirmation bias.
# They also have same innovation rate (mut) and memory size (menLen).
#
# 1.2. Method recall: function used to store observed and shown variants (or signals) in agents' memories. Each agent has two
# memories: one for shown varaints (self.mem_shown) and another one for observed variants (self.mem_observed).
#
# 1.3. Method with_b: Parametrized mathematical model: The model yields the probability distribution
# of variants (x) for a given agent's history (h) of previous rounds, according to an equation
# that is as a function of content bias, coordination bias, memory and innovarion rate.
#
# 1.4. Method choose (Variant selection): Function that yields the variant choice of the agent being simulated.
#
# 2. class Match: it defines an __init__() method with arguments for each instance of the game.
# It creates the agents. The constructor of those agents (in class Agent) assigns values to the attributes
# of each Agent instance.
# self.agents is a dictionary that is created by iterating parameter "agents", which is the list of names. Here,
# each element will correspond to a key-value pair in the dictionary. Each key-value pair has a key (name of the agent) and
# its value, which is a new Agent object that is created with the Agent(parameters) sintax.
#
# 2.1 Method produce_signals: it yileds a dictionary that contains each agent' variant choice at each round
#
# 2.2 Method play: it uses methods produce_signals and recall to allow agents showing and observing signals and store them in memory.
# It also updates the value system of the institution by averaging agents' value systems.
#
# 3. Function entropy: to calculate diversity of variants
#
# 4. Function group: to set up the network connectivity dynamic (the order in wich agents pair over time)
#
# 5. Functon main:

# declares the initial values of the variables:
# Agents (name and population size)
# Variants (name and number of variants)
# Social network
# Memory lenght
# Value systems
# Content bias
# Coordination bias
# Compliance bias
# Conformity bias
# Number of samples
# Number of simulations
# Note: institutional power is defined in the choose method
#
# creates statistcs,
#
# runs instances of the game (game.play()), for each simulation and sample:
#
# writes the csv file with all the data
#
#############################################################
from __future__ import division
from random import random, sample
import random as rand
from random import choices
from bisect import bisect
from collections import deque, Counter
from itertools import permutations
import csv
import math
import numpy as np
from skbio.diversity.alpha import brillouin_d, margalef, simpson, simpson_e, observed_otus, shannon


###List that appends institution's value system at each round
institution_history=[]

##class Agent, with constructor __init__, which is used to assign values to the attributes of each Agent instance in class Match
class Agent:
    def __init__(self, name, signals, sigma, cont, coord, conform, confirm, mut, menLen):
        self.name = name
        self.signals = signals
        self.mem_shown = {signal: 0 for signal in signals}
        self.mem_observed = {signal: 0 for signal in signals}
        self.__mem_shown = deque(maxlen=menLen)
        self.__mem_observed = deque(maxlen=menLen)
        self.sigma = sigma[:]  # Make a unique copy of sigma for each agent
        self.cont = cont
        self.coord = coord
        self.conform = conform
        self.confirm = confirm
        self.mut = mut

    # Method recall
    def recall(self, v_shown, v_observed):
        self.__mem_shown.append(v_shown)
        self.__mem_observed.append(v_observed)
        v_shown = Counter(self.__mem_shown)
        v_observed = Counter(self.__mem_observed)
        self.mem_shown = {signal: v_shown.get(signal, 0) for signal in self.signals}
        self.mem_observed = {signal: v_observed.get(signal, 0) for signal in self.signals}

    def __str__(self):
        return "Agent_{}".format(self.name)

    #Method with_b: Probabilistic function that yields the probability
    # of production of a variant (or signal) for a given history
    def with_b(self, shown, observed, r, idx):
        if not (shown == observed == 0):
            result = (
                    ((0.98) * (1.0 - self.cont) * (1.0 - self.coord) * shown / r)
                    + ((0.98) * (1.0 - self.cont) * (self.coord) * observed / r)
                    + ((0.98) * self.cont * self.sigma[idx])
                    + ((self.mut / 8))
            )
        else:
            result = (
                    ((0.98) * (1.0 - 0) * (1.0 - self.coord) * shown / r)
                    + ((0.98) * (1.0 - 0) * (self.coord) * observed / r)
                    + ((0.98) * 0 * self.sigma[idx])
                    + ((self.mut / 8))
            )
        return result

    ##  Method choose (Variant selection): Function that yields the variant choice of the agent being simulated
    def choose(self, r, institution):
        probs = [
            self.with_b(
                self.mem_shown[op], self.mem_observed[op], r, indx
            )
            for indx, op in enumerate(self.signals)
        ]
        # Elecc is a list that returns a randomly selected element from the specified sequence:
        # In our case, we weight the possibility of each result with the probabilities yielded by the model (probs)
        elecc = choices(self.signals, probs)[0]
        # Line to transform choice (e.g. "S1") to a vector representing the choice [1,0,0,0]
        aux = [(elecc == signali) + 0 for signali in self.signals]
        #############
        # The following lines update agent's value system(sigma) in the current round, according to
        # compliance, confirmation, and institutional values and institutional power.
        # Note that self.sigma is the value system of the agent in the current run
        i_power = 0 #Epsilon: institutional power (takes values from 0 (null power) to 1 (full power))
        #Kappa: compliance bias is self.conform
        #Gamma: confirmation bias is self.confirm
        non_conformity = (1 - self.conform)
        current_choice_bias = (1 - self.confirm)
        current_choice_weight = [i * current_choice_bias for i in aux]
        value_system_weight = [i * self.confirm for i in self.sigma]
        agent_values_sum = np.add(current_choice_weight, value_system_weight)
        individual_weight = [i * non_conformity for i in agent_values_sum]
        institutional_weight = [i * self.conform * i_power for i in institution]
        self.sigma = np.add(individual_weight, institutional_weight) # This self.sigma will be used in the next round r+1
        # print("new_value_system(new_sigma)={}".format(value_system))
        #print(elecc)
        return elecc

# class Match, constructor of the game
class Match:
    def __init__(self, agents, pairs, signals, sigmas, cont, coord, conform, confirm, mut, menLen):
        self.pairs = pairs
        self.signals = signals
        self.agents = {
            name: Agent(name, signals, sigmas[name], cont, coord, conform, confirm, mut, menLen)
            for name in agents
        }
        self.institution = np.mean(list(sigmas.values()), 0)
        self.memory = list()
        self.entropy = float()
        self.memory_sigmas = list()

    # Method produce_signals: it yileds a dictionary that contains each agent' variant choice at each round
    def produce_signals(self):
        # Agents initialised with an unique variant
        # yield dict(zip(self.agents, self.signals))
        # Agents initialised with a random variant from the pool of variants (without repetition [with replacement]): Agents might have the same initial varaint
        yield dict(zip(self.agents, choices(self.signals, k=10)))
        r = 1
        while True:
            eleccs = {}
            for agent in self.agents.values():
                eleccs[agent.name] = agent.choose(r, self.institution)
            r += 1
            yield eleccs
            #print(eleccs)

    # Method play: it uses methods produce_signals and recall to allow agents showing and observing signals and store them in memory.
    # It also updates the value system of the institution by averaging agents' value systems.
    def play(self):
        gen_sens = self.produce_signals()
        for round in self.pairs:
            signals = next(gen_sens)
            self.memory.append(signals)
            for agent1, agent2 in round:
                self.agents[agent1].recall(v_observed=signals[agent2], v_shown=signals[agent1])
                self.agents[agent2].recall(v_observed=signals[agent1], v_shown=signals[agent2])
            # for name, agent in self.agents.items():
            #     print("{}: sigma={}, content.bias={}".format(name, agent.sigma, agent.cont))
            # Calculate mean of sigmas of the past round. That is, create the institution, which is a mean of agents' value system
            i = [agent.sigma for agent in self.agents.values()]
            self.institution = np.mean(i, 0)
            # Append institution value system (institution) to the history of the institution (institution_history)
            institution_history.append(self.institution)

# Shannon entropy function
def entropy(lista):
    N = sum(lista)
    probs = (freq / N for freq in lista if freq > 0)
    return -sum(x * math.log(x, 2) for x in probs)

# Function to randomize connectivity dynamic (with replacement): n= number of rounds
def group(agents, n=100):
  caso = agents[:]
  result = []
  for _ in range(n):
    rand.shuffle(caso)
    gen = list(zip(*[iter(caso)] * 2))
    result.append(gen)
  return result


# main function (declares variables, generates statistics, runs the gam.play(), writes csv file)
def main():
    # Agents names (and number of agents):
    agents = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    # Variants
    signals = ['S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10']

    # Social network (order in which agents pair over time)
    network = group(agents)
    pairs = [list(elem) for elem in network]

    # Memory length (amount of hisotry (in rounds) that agents are able to recall)
    menLen = 3

    # Condition (type name according to value system structure, sigmas)
    condition = "Homogeneity"

    # Scenario (type name according to value system structure sigmas)
    scenario = "OTA"

    # Institutional power (type number according to value assinged to institutional power in "choose" method
    i_power = 0

    ####SIGMAS: Agents' value system at the initial state. That is, the value that an agent assigns to each signal in the initial state####
    ### Homogeneity and hegemony (OTA)
    s1 = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    s2 = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ### Heterogeneity and pseudo-random (PR)
    #s1 = np.random.uniform(low=0, high=1, size=(10,))
    #s2 = np.random.uniform(low=0, high=1, size=(10,))
    ### Dictionary of agents' value systems
    sigmas = {1: s1, 2: s1, 3: s1, 4: s1, 5: s1, 6: s2, 7: s2, 8: s2, 9:s2, 10:s2}

    # Samples: agents' content bias, coordination bias, conformity bias, confirmation bias, innovation rate.
    # Content bias ('cont'): no content bias=0.0, fully content biased population=1.0
    # Coordination bias ('coord'): fully egocentric=0.0, fully allocentric=1.0, neutral=0.5
    # Compliance bias ('conform'): null compliance=0.0, fully compliant=1.0
    # Conformity bias ('conformity'): null conformity=0.0, full conformity=1.0
    # Innovation rate('mut')
    # Setup for different parameter combinations:
    samples = [
        {'cont': 0.0, 'coord': 0.5, 'conform': 0, 'confirm':0, 'mut': 0.02},
        {'cont': 0.2, 'coord': 0.5, 'conform': 0, 'confirm':0, 'mut': 0.02},
        {'cont': 0.4, 'coord': 0.5, 'conform': 0, 'confirm':0, 'mut': 0.02},
        {'cont': 0.5, 'coord': 0.5, 'conform': 0, 'confirm':0, 'mut': 0.02},
        {'cont': 0.6, 'coord': 0.5, 'conform': 0, 'confirm':0, 'mut': 0.02},
        {'cont': 0.8, 'coord': 0.5, 'conform': 0, 'confirm':0, 'mut': 0.02},
        {'cont': 1.0, 'coord': 0.5, 'conform': 0, 'confirm':0, 'mut': 0.02},
        {'cont': 0.0, 'coord': 0.5, 'conform': 0, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.2, 'coord': 0.5, 'conform': 0, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.4, 'coord': 0.5, 'conform': 0, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.5, 'coord': 0.5, 'conform': 0, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.6, 'coord': 0.5, 'conform': 0, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.8, 'coord': 0.5, 'conform': 0, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 1.0, 'coord': 0.5, 'conform': 0, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.0, 'coord': 0.5, 'conform': 0, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.2, 'coord': 0.5, 'conform': 0, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.4, 'coord': 0.5, 'conform': 0, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.5, 'coord': 0.5, 'conform': 0, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.6, 'coord': 0.5, 'conform': 0, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.8, 'coord': 0.5, 'conform': 0, 'confirm': 1, 'mut': 0.02},
        {'cont': 1.0, 'coord': 0.5, 'conform': 0, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.0, 'coord': 0.5, 'conform': 0.5, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.2, 'coord': 0.5, 'conform': 0.5, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.4, 'coord': 0.5, 'conform': 0.5, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.5, 'coord': 0.5, 'conform': 0.5, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.6, 'coord': 0.5, 'conform': 0.5, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.8, 'coord': 0.5, 'conform': 0.5, 'confirm': 0, 'mut': 0.02},
        {'cont': 1.0, 'coord': 0.5, 'conform': 0.5, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.0, 'coord': 0.5, 'conform': 0.5, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.2, 'coord': 0.5, 'conform': 0.5, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.4, 'coord': 0.5, 'conform': 0.5, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.5, 'coord': 0.5, 'conform': 0.5, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.6, 'coord': 0.5, 'conform': 0.5, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.8, 'coord': 0.5, 'conform': 0.5, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 1.0, 'coord': 0.5, 'conform': 0.5, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.0, 'coord': 0.5, 'conform': 0.5, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.2, 'coord': 0.5, 'conform': 0.5, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.4, 'coord': 0.5, 'conform': 0.5, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.5, 'coord': 0.5, 'conform': 0.5, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.6, 'coord': 0.5, 'conform': 0.5, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.8, 'coord': 0.5, 'conform': 0.5, 'confirm': 1, 'mut': 0.02},
        {'cont': 1.0, 'coord': 0.5, 'conform': 0.5, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.0, 'coord': 0.5, 'conform': 1, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.2, 'coord': 0.5, 'conform': 1, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.4, 'coord': 0.5, 'conform': 1, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.5, 'coord': 0.5, 'conform': 1, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.6, 'coord': 0.5, 'conform': 1, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.8, 'coord': 0.5, 'conform': 1, 'confirm': 0, 'mut': 0.02},
        {'cont': 1.0, 'coord': 0.5, 'conform': 1, 'confirm': 0, 'mut': 0.02},
        {'cont': 0.0, 'coord': 0.5, 'conform': 1, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.2, 'coord': 0.5, 'conform': 1, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.4, 'coord': 0.5, 'conform': 1, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.5, 'coord': 0.5, 'conform': 1, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.6, 'coord': 0.5, 'conform': 1, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.8, 'coord': 0.5, 'conform': 1, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 1.0, 'coord': 0.5, 'conform': 1, 'confirm': 0.5, 'mut': 0.02},
        {'cont': 0.0, 'coord': 0.5, 'conform': 1, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.2, 'coord': 0.5, 'conform': 1, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.4, 'coord': 0.5, 'conform': 1, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.5, 'coord': 0.5, 'conform': 1, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.6, 'coord': 0.5, 'conform': 1, 'confirm': 1, 'mut': 0.02},
        {'cont': 0.8, 'coord': 0.5, 'conform': 1, 'confirm': 1, 'mut': 0.02},
        {'cont': 1.0, 'coord': 0.5, 'conform': 1, 'confirm': 1, 'mut': 0.02}]

    # Number of samples of each parameter combination
    samples = [d for d in samples for _ in range(1)]

    # Number of simulations
    simulations = 1

    # Statistics
    statistics = {
        sim: {
            agent: {
                sample: {
                    signal: [0 for round in range(1, len(pairs) + 1)]
                    for signal in signals
                }
                for sample in range(len(samples))
            }
            for agent in agents
        }
        for sim in range(simulations)
    }

    # Piece of code to run each instance of the game (game.play) for the specified number of samples and simulations
    for sim in range(simulations):
        network = group(agents)
        pairs = [list(elem) for elem in network]
        for mu in range(len(samples)):
            game = Match(
                agents,
                pairs,
                signals,
                sigmas,
                samples[mu]["cont"],
                samples[mu]["coord"],
                samples[mu]["conform"],
                samples[mu]["confirm"],
                samples[mu]["mut"],
                menLen
            )
            game.play()
            for n, round in enumerate(game.memory):
                for agent, signal in round.items():
                    statistics[sim][agent][mu][signal][n] += 1

    # Write csv file
    with open('Test_COEVO_Hom_OTA_R_I00_F.csv', 'w', newline='') as csvfile:
        writer = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(['Simulation', 'Sample', 'Agent', 'Memory', 'Generation', 'Condition', 'Scenario', 'Inst_power','Content bias',
                         'Coordination bias','Conformity bias','Confirmation bias', 'Mutation rate'] + signals +
                        ['Population signals'] + ['Entropy_population'] + ['Entropy_subpopulation_1'] + [
                            'Entropy_subpopulation_2'] + ['Subpopulation_1 signals'] + ['Subpopulation_2 signals']
                        + ['Brillouin_population'] + ['Margalef_population'] + ['Simpson_population'] + [
                            'Simpson_e_population'] + ['Richness'])

        # Creating lists that contain the the production of signals at each round: for the whole population (aux) and each agent (auxn)
        for agent in agents:
            for sim in range(simulations):
                for mu in range(len(samples)):
                    for round in range(1, len(pairs) + 1):
                        aux = [statistics[sim][agent][mu][signal][round - 1] for signal in signals]
                        aux1 = [statistics[sim][1][mu][signal][round - 1] for signal in signals]
                        aux2 = [statistics[sim][2][mu][signal][round - 1] for signal in signals]
                        aux3 = [statistics[sim][3][mu][signal][round - 1] for signal in signals]
                        aux4 = [statistics[sim][4][mu][signal][round - 1] for signal in signals]
                        aux5 = [statistics[sim][5][mu][signal][round - 1] for signal in signals]
                        aux6 = [statistics[sim][6][mu][signal][round - 1] for signal in signals]
                        aux7 = [statistics[sim][7][mu][signal][round - 1] for signal in signals]
                        aux8 = [statistics[sim][8][mu][signal][round - 1] for signal in signals]
                        aux9 = [statistics[sim][9][mu][signal][round - 1] for signal in signals]
                        aux10 = [statistics[sim][10][mu][signal][round - 1] for signal in signals]

                        # List that contains the summation of produced signals at the level of the population
                        summation_pop = []
                        # # List that contains the summation of produced signals at the level of each subpopulation
                        summation_subpop_1 = []
                        summation_subpop_2 = []

                        # Piece of code to append the lists of signals
                        for i in range(len(aux1)):
                            # At the population level
                            summation_pop.append(
                                aux1[i] + aux2[i] + aux3[i] + aux4[i] + aux5[i] + aux6[i] + aux7[i] + aux8[i] + aux9[i] + aux10[i])
                            # At the subpopulation level
                        for i in range(len(aux1)):
                            summation_subpop_1.append(aux1[i] + aux2[i] + aux3[i] + aux4[i])
                            summation_subpop_2.append(aux5[i] + aux6[i] + aux7[i] + aux8[i])

                        # Writing csv file
                        writer.writerow([sim + 1, mu + 1, agent, menLen, round, condition, scenario, i_power, samples[mu]['cont'],
                                         samples[mu]['coord'], samples[mu]['conform'], samples[mu]['confirm'],
                                         samples[mu]['mut']] + aux + [summation_pop] + [shannon(summation_pop)] + [
                                            shannon(summation_subpop_1)] + [shannon(summation_subpop_2)] + [
                                            summation_subpop_1] + [summation_subpop_2]
                                        + [brillouin_d(summation_pop)] + [margalef(summation_pop)] + [
                                            simpson(summation_pop)] + [simpson_e(summation_pop)] + [
                                            observed_otus(summation_pop) / 8])


if __name__ == '__main__':
    main()

#This piece of code adds two new columns: one with institutional value systems, the other with institutional entropy of values
import pandas as pd
df = pd.read_csv('Test_COEVO_Hom_OTA_R_I00_F.csv')
df['Institution'] = pd.Series(institution_history)
df['Entropy_Institution'] = pd.Series(entropy(i) for i in institution_history)
df = df.drop(df[df.Agent > 1].index)
df.to_csv('Test_COEVO_Hom_OTA_R_I00_F.csv')
