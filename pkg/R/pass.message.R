

#takes a well ordered CEG and C-copmatible information I 
#outputs: an uncolored CEG with pi_hat

evidence <- df[1,]

pass.message <- function(df, stage.key, evidence){#what's the most natural way to put the evidence into the system?
  for i in (1:length(stage.key)){
    mutate(stage.key[[i]],pi <- n/dim(df)[1])
    
  }
  
  
  
}


Input: A well-ordered CEG C = (W,E) and a C-compatible information I.
Output: An uncoloured CEG ˆ
Ct = ( ˆ E). W, ˆ 1 Set ˆ
W = ∅, ˆ
E = ∅, and φ = 0. 2 Initialise a vector ˆ π of length #W −1.
3 for i from #W −1 to 0 do 4
Initialise a vector τi = −1 of length Ki
5
for j from 1 to Ki do
6
τij ←τj(wi) from (5.15)
7
if τj(wi) ?= 0 then
8
E ← ˆ
ˆ E ∪ {eij} 9
φ←φ(wi) from (5.16)
10
if φ(wi) ?= 0 then
11
V ← ˆ
ˆ V ∪ {wi} 12 πi ←τ/φ ˆ 13