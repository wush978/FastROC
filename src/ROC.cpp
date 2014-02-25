#include <Rcpp.h>

using namespace Rcpp;

class roc_functor {
  NumericVector reference;
public:
  roc_functor(SEXP reference_src) : reference(reference_src) { }
  bool operator()(int i, int j) {
    return reference[i] < reference[j];
  }
};

//[[Rcpp::export]]
SEXP ROC(SEXP Ranswer, SEXP Restimate) {
  IntegerVector answer(Ranswer);
  NumericVector estimate(Restimate);
  if (answer.size() != estimate.size()) throw std::invalid_argument("");
  std::vector<int> index(estimate.size());
  for(int i = 0;i < index.size();i++) index[i] = i;
  roc_functor f(wrap(estimate));
  std::sort(index.begin(), index.end(), f);
  double state[4];
  state[0] = 0.0;
  state[1] = 0.0;
  state[2] = (double) std::accumulate(answer.begin(), answer.end(), 0);
  state[3] = (double) (answer.size() - state[2]);
  NumericVector retval_x(index.size()), retval_y(index.size());
  for(int i = 0;i < index.size();i++) {
    if (answer[index[i]]) {
      state[0]++;
    }
    else {
      state[1]++;
    }
    retval_x[i] = state[1] / state[3];
    retval_y[i] = state[0] / state[2];
  }
  List retval;
  retval["x"] = retval_x;
  retval["y"] = retval_y;
  return retval;
}