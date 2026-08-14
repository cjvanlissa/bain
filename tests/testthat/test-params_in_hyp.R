# params_in_hyp <- function(hyp){
#   hyp <- bain:::rename_function(hyp)
#   out <- unique(regmatches(
#     hyp,
#     gregexpr(
#       "[A-Za-z][A-Za-z0-9_\\.]*(?:\\[[^]]+\\])?",
#       hyp,
#       perl = TRUE
#     )
#   )[[1]])
#   bain:::reverse_rename_function(out)
# }
#
# params_in_hyp <- function(hyp){
#   parts <- regmatches(
#     hyp,
#     gregexpr(
#       "[A-Za-z][A-Za-z0-9_.]*\\[[0-9]+(?:,[0-9]+)*\\]|[^\\s><=+\\-*(),&;]+",
#       hyp,
#       perl = TRUE
#     )
#   )[[1]]
#   unique(parts[grepl("^[A-Za-z\\.]", parts)])
# }

test_that("params_in_hyp works", {


expect_equal(params_in_hyp("M[1,2]>M[2,2]"), c("M[1,2]", "M[2,2]"))
expect_equal(params_in_hyp("(M[1,2],M[2,2])=0"), c("M[1,2]", "M[2,2]"))
expect_equal(params_in_hyp("sex.2>sex:pea.2"), c("sex.2", "sex:pea.2"))
expect_equal(params_in_hyp("(sex.2,sex:pea.2)=0"), c("sex.2", "sex:pea.2"))
expect_equal(params_in_hyp("a-b = 0 & b  > 0"), c("a", "b"))

expect_equal(params_in_hyp("a=0 & b=0 & c=0 & d=0 & e=0 & f=0; a<0 & b=0 & c<0 & d=0 & e<0 & f=0;a<0 & b>0 & c<0 & d>0 & e<0 & f>0"), c("a", "b","c", "d", "e", "f"))
expect_equal(params_in_hyp("a > b & b > c"), c("a", "b","c"))

expect_equal(params_in_hyp(" Ab~Bb = Ab~Bl; Ab~Bb > Ab~Bl"), c("Ab~Bb", "Ab~Bl"))

expect_equal(params_in_hyp(" A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 ; A=~An = .6 & A=~Ar = .6 & A=~Ac=.6 ;
B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 ; B=~Bn = .6 & B=~Br = .6 & B=~Bc =.6"), c("A=~Ab", "A=~Al", "A=~Af", "A=~An", "A=~Ar", "A=~Ac", "B=~Bb", "B=~Bl", "B=~Bf", "B=~Bn", "B=~Br", "B=~Bc"))

})

expect_equal(params_in_hyp("A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl
"), c("A=~Ab.boy", "A=~Ab.girl", "A=~Al.boy", "A=~Al.girl", "A=~Af.boy",
     "A=~Af.girl", "A=~An.boy", "A=~An.girl", "A=~Ar.boy", "A=~Ar.girl",
     "A=~Ac.boy", "A=~Ac.girl", "B=~Bb.boy", "B=~Bb.girl", "B=~Bl.boy",
     "B=~Bl.girl", "B=~Bf.boy", "B=~Bf.girl", "B=~Bn.boy", "B=~Bn.girl",
     "B=~Br.boy", "B=~Br.girl", "B=~Bc.boy", "B=~Bc.girl", "Ab~1.boy",
     "Ab~1.girl", "Al~1.boy", "Al~1.girl", "Af~1.boy", "Af~1.girl",
     "An~1.boy", "An~1.girl", "Ar~1.boy", "Ar~1.girl", "Ac~1.boy",
     "Ac~1.girl", "Bb~1.boy", "Bb~1.girl", "Bl~1.boy", "Bl~1.girl",
     "Bf~1.boy", "Bf~1.girl", "Bn~1.boy", "Bn~1.girl", "Br~1.boy",
     "Br~1.girl", "Bc~1.boy", "Bc~1.girl"))
