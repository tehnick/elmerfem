#ifndef MGERROR_H
#define MGERROR_H

#include <iostream>

template<class T>
inline void blm_error(const char* msg, const T &opt)
{
	std::cerr << "*** ERROR: " << msg << ' ' << opt << std::endl;
	exit(1);
}

inline void blm_error(const char* msg)
{
	std::cerr << "*** ERROR: " << msg << std::endl;
	exit(1);
}

#endif
