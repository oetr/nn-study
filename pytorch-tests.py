import torch
torch.set_printoptions(precision=10)


t0 = torch.tensor([1,2,3,4,5]).double(); t0.requires_grad = True
t1 = torch.tensor([1,2,3,4,5]).double(); t1.requires_grad = True
t2 = t0 * t1; t2.retain_grad()
t3 = torch.tanh(t2); t3.retain_grad()
result = sum(t3)
result.backward()
t0
t2
t3
result.item()
