#ifndef fun_club_hh
#define fun_club_hh 1

#include <string>
#include <map>
#include <set>

struct Encoder {
  std::map<std::string, int> m;                              // arg - int
  std::map<int, std::pair<std::string, std::string> > m_inv; // int - (arg, printable_arg)
  std::set<int> holes;                                       // int holes
  std::set<int> values;                                      // int
};
typedef std::map<std::string, Encoder> Encoders;

#endif
