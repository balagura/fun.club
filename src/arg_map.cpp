#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <iostream>
#include <cstdlib>  // for atoi()
#include <Rcpp.h>
#include "../inst/include/fun.club.h"
using namespace Rcpp;

//'
//'   String to integer encoder
//'
//'   Initializes C++ Encoder containers.
//'
//'   @return R external pointer which "wraps" the C++ pointer to the newly
//'   created C++ Encoder. The latter will be deleted when R external pointer will
//'   go out of scope.
//'
//'   @author Vladislav BALAGURA <balagura@cern.ch>
//'
//'   @keywords internal
//'
// [[Rcpp::export]]
XPtr<Encoders > new_arg_encoder() {
  return XPtr<Encoders>( new(Encoders) );
}

/**
   String to integer encoder

   Converts a character representation of function arguments to a positive
   integer. Together with the name of the function object it gives the unique
   identificator of the result returned by the function. If the result is
   discarded later, the integer number is "freed" and might be reused. In
   discarding / reassigning, the integers are kept as low as possible.

   @param fo The name of the function object

   @param arg The unique character representation of function arguments,
   normally obtained by their serialization and not in "human-readable" format

   @param printable_arg The "human-readable" character representation of
   function arguments of the form `a = 10, 20, ...`. It is used for
   `message()`s, `stop()`s etc., ie. when this argument combination is exposed
   to the user. `printable_arg` is also unique except when the arguments can
   not be `deparse()`d properly, see `?.deparseOpts`. Because of the latter,
   `serialize()`d version `arg` above is used as a main unique representation,
   while `deparse()`d is just supplementary.

   @return List with `i` and `new` components: `i` contains the integer number
   uniquely linked to this combination of arguments, and `new` = TRUE / FALSE
   flags whether this is a new combination.

   @author Vladislav BALAGURA <balagura@cern.ch>

   @keywords internal

*/
// [[Rcpp::export]]
List add_arg(XPtr<Encoders> xptr, std::string fo, std::string arg, std::string printable_arg) {
  Encoder& xx = (*xptr)[ fo ];
  std::map<std::string, int>::iterator iter = xx.m.find(arg);
  if (iter != xx.m.end()) {
    return List::create(Named("i") = iter->second,
			Named("new")= false);
  } else {
    /**
       `xx.holes` keep "holes" arising after removal of integers from the
       "middle". The lowest "free hole" is assigned each time. If there are no
       "holes" and `xx.holes` is empty, this can only be if all integers in
       the range 1 : `xx.m.size()` are in use. Then, the next `xx.m.size()+1`
       is assigned.
    */
    int i;
    if (!xx.holes.empty()) {
      i = *xx.holes.begin();
      xx.holes.erase(xx.holes.begin());
    } else
      i = xx.m.size() + 1;
    xx.m.insert(std::make_pair(arg, i));
    xx.m_inv.insert(std::make_pair(i, std::make_pair(arg, printable_arg)));
    xx.values.insert(i);
    return List::create(Named("i") = i,
			Named("new")= true);
  }
}

/**
   String to integer encoder

   Returns the unique positive integer corresponding to the result calculated
   by the function `fo` with the given arguments.

   @param fo The name of the function object

   @param arg The unique character representation of function arguments,
   normally obtained by their serialization and not in "human-readable" format

   @return Positive integer which uniquely codes the combination of the
   function and its arguments. If such a combination is not found, zero is
   returned.

   @author Vladislav BALAGURA <balagura@cern.ch>

   @keywords internal

*/
// [[Rcpp::export]]
int ind_arg(XPtr<Encoders> xptr, std::string fo, std::string arg) {
  Encoders::const_iterator it1 = xptr->find( fo );
  if (it1 == xptr->end()) return 0;
  std::map<std::string, int>::const_iterator it2 = it1->second.m.find( arg );
  if (it2 == it1->second.m.end()) return 0;
  return it2->second;
}

/**
   String to integer encoder

   Returns "human-readable" printable representation of the arguments
   corresponding to the given function object name `fo` and the integer number `ind`.

   @param fo The name of the function object
   @param ind The integer number which uniquely codes the combination of arguments

   @return The "human-readable" representation of the combinations of
   arguments as a character string vector with one element. If the function
   object or the integer is not found, an empty character vector is returned.

   @author Vladislav BALAGURA <balagura@cern.ch>

   @keywords internal

*/
// [[Rcpp::export]]
CharacterVector printable_arg(XPtr<Encoders> xptr, const std::string& fo, int ind) {
  Encoders::const_iterator it1 = xptr->find( fo );
  if (it1 == xptr->end()) return CharacterVector();
  std::map<int, std::pair<std::string, std::string> >::const_iterator it2 =
    it1->second.m_inv.find( ind );
  if (it2 == it1->second.m_inv.end()) return CharacterVector();
  CharacterVector res(1);
  res[0] = it2->second.second;
  return res;
}

/**
   String to integer encoder

   Deletes a character string from the associative collection of (string -
   integer) pairs.

   @param fo The name of the function object
   @param arg The character representation of function arguments to be deleted

   @return 0 for successful removal, 1 if `arg` is not found

   @author Vladislav BALAGURA <balagura@cern.ch>
   @keywords internal

 */
// [[Rcpp::export]] 
int rm_arg(XPtr<Encoders> xptr, std::string fo, int ind) {
  Encoder& xx = (*xptr)[ fo ];
  std::map<int, std::pair<std::string, std::string> >::iterator iter = xx.m_inv.find(ind);
  if (iter == xx.m_inv.end()) return 1; 
  xx.holes.insert(ind);
  xx.values.erase(ind);
  xx.m.erase(iter->second.first);
  xx.m_inv.erase(iter);
  if (xx.values.empty())
    xx.holes.clear();
  else
    // consider as holes only those that are less than `max(values)`,
    // `values` set (integers corresponding to strings) is only for that.
    xx.holes.erase(xx.holes.lower_bound(*(--xx.values.end())), xx.holes.end());
  return(0);
}

/**
   String to integer encoder
   
   Initializes encoder with (string - printable string - integer) triples
   given in vectors `args`, `printable_args`, `inds` for a single function
   object named `fo_name`. `x[fo_name].holes` is filled with "holes", ie. with
   not used integers from the range 1 : max_used_integer.
   
   @param fo_name The name of the function object
   @param args The vector of character strings
   @param inds The corresponding integer numbers
   
   @return void
   
   @author Vladislav BALAGURA <balagura@cern.ch>
   @keywords internal

*/
// [[Rcpp::export]] 
void load_fo_arg(XPtr<Encoders> xptr,
		 std::string fo_name,
		 std::vector<std::string> args,
		 std::vector<std::string> printable_args,
		 std::vector<int> inds) {
  Encoder& xx = (*xptr)[ fo_name ];
  std::vector<int>::const_iterator it = std::max_element(inds.begin(), inds.end());
  if (it != inds.end()) {
    int max_i = *it;
    for (int i=1; i<=max_i; ++i) xx.holes.insert(i);
    for (int i=0; i<int(args.size()); ++i) {
      xx.m    [ args[i] ] = inds[i];
      xx.m_inv[ inds[i] ] = std::make_pair(args[i], printable_args[i]);
      xx.holes.erase(inds[i]);
    }
    std::copy(inds.begin(), inds.end(), std::inserter(xx.values, xx.values.begin()));
  }
}

/**
   String to integer encoder
   
   Creates the encoder with (string - printable string - integer) triples for
   all function objects. This structure is stored to disk when fun.club is
   deleted, eg. between the R sessions. The function below is used to restore
   the saved object. Internally, it calls `load_fo_arg(fo_name, args,
   printable_args, inds)` for every function object.

   @param arg_encoder The list with elements named after the corresponding
   function objects. Each element is a list with three vectors: `arg` with
   strings, `printable_arg` with "human-readable" versions of the strings, and
   `ind` with integers.
   
   @return The R external pointer to the `new`ly created `Encoders` object
   populated with `arg_encoder` data.
   
   @author Vladislav BALAGURA <balagura@cern.ch>
   @keywords internal

*/
// [[Rcpp::export]] 
XPtr<Encoders > load_arg(List arg_encoder) {
  XPtr<Encoders> xptr = new_arg_encoder();
  std::vector<std::string> fo_names = arg_encoder.names();
  for (int i = 0; i < int(arg_encoder.size()); ++i) {
    List fo = as<List>( arg_encoder[i] );
    load_fo_arg(xptr, fo_names[i], fo["arg"], fo["printable_arg"], fo["ind"]);
  }
  return xptr;
}

std::string get_key  (std::pair<std::string, int> p) { return p.first; }
int         get_value(std::pair<std::string, int> p) { return p.second; }
/**
   String to integer encoder
   
   Dumps (string - integer) pairs for saving. `holes` and `values` are not
   saved as they are redundant.
   
   @return List with the elements named by the corresponding function
   object. Every element is itself a list with the `arg`, `printable_arg` and
   `i` fields containing strings, "human-readable" strings and integers,
   respectively.
   
   @author Vladislav BALAGURA <balagura@cern.ch>
   @keywords internal

*/
// [[Rcpp::export]] 
List dump_arg(XPtr<Encoders> xptr) {
  std::map<std::string, List> res;
  for (Encoders::const_iterator i1 = xptr->begin(); i1 != xptr->end(); ++i1) {
    const std::map<std::string, int>& m = i1->second.m;
    const std::map<int, std::pair<std::string, std::string> >&
      m_inv = i1->second.m_inv;
    std::vector<std::string> keys(m.size());
    std::vector<int>         values(m.size());
    std::vector<std::string> inv(m.size());
    std::transform(m.begin(), m.end(), keys.begin(),   get_key);
    std::transform(m.begin(), m.end(), values.begin(), get_value);
    for (int i=0; i<int(values.size()); ++i) {
      inv[i] = m_inv.find( values[i] )->second.second;
    }
    res[i1->first] = List::create(_["arg"] = keys,
				  _["printable_arg"] = inv,
				  _["ind"] = values);
  }
  return wrap(res);
}

/**
   String to integer encoder
   
   prints the encoder content for debugging
   
   @return void
   
   @author Vladislav BALAGURA <balagura@cern.ch>
   @keywords internal

*/
// [[Rcpp::export]] 
void print_arg(XPtr<Encoders> xptr) {
  for (Encoders::const_iterator
	 i1 = xptr->begin(); i1 != xptr->end(); ++i1) {
    for (std::map<std::string, int>::const_iterator
	   i2 = i1->second.m.begin(); i2 != i1->second.m.end(); ++i2) {
      std::cout << i1->first << " " << i2->first << " " << i2->second << "\n";
    }
    for (std::map<int, std::pair<std::string, std::string> >::const_iterator
	   i2 = i1->second.m_inv.begin(); i2 != i1->second.m_inv.end(); ++i2) {
      std::cout << "inversed " << i1->first << " " << i2->first << " "
		<< i2->second.first << " " << i2->second.second << "\n";
    }
    for (std::set<int>::const_iterator
	   i2 = i1->second.holes.begin(); i2 != i1->second.holes.end(); ++i2) {
      std::cout << "hole " << i1->first << " " << *i2 << "\n";
    }
    for (std::set<int>::const_iterator
	   i2 = i1->second.values.begin(); i2 != i1->second.values.end(); ++i2) {
      std::cout << "int " << i1->first << " " << *i2 << "\n";
    }
  }
}

/**
   String to integer encoder
   
   Clears and resets the encoder.
   
   @return void
   
   @author Vladislav BALAGURA <balagura@cern.ch>
   @keywords internal

*/
// [[Rcpp::export]] 
void clear_arg(XPtr<Encoders> xptr) {
  xptr->clear();
}
    
