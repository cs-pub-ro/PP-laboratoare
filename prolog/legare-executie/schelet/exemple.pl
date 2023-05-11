

p(a).
p(b).
p(A/B) :- q(A), !, t(A/B).
p(d).

q(a).
q(b).
q(c).

t(a/a).
t(a/b).
t(b/c).
t(b/d).
