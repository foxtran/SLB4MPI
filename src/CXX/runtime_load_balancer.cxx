#include <SLB4MPI/runtime_load_balancer.hpp>
#include <SLB4MPI.hpp>

#include <iostream>
#include <cstdlib>
#include <memory>

SLB4MPI::RuntimeLoadBalancer::RuntimeLoadBalancer(const std::string& lbtype, const MPI_Comm communicator, const int64_t lower_bound, const int64_t upper_bound, const int64_t min_chunk_size, const int64_t max_chunk_size) :
  SLB4MPI::AbstractLoadBalancer::AbstractLoadBalancer(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size) {
  enum class LBType { env, rb_static, local_static, dynamic, guided, work_stealing, unknown };
  LBType lbtypeid = LBType::env;
  std::string envval = std::string();
  if (lbtype == std::string("env")) {
    char const* Cenvval = std::getenv("SLB4MPI_LOAD_BALANCER");
    envval = Cenvval == nullptr ? std::string() : std::string(Cenvval);
    if (envval == std::string() || envval == std::string("static")) {
      lbtypeid = LBType::rb_static;
    } else if (envval == std::string("local_static")) {
      lbtypeid = LBType::local_static;
    } else if (envval == std::string("dynamic")) {
      lbtypeid = LBType::dynamic;
    } else if (envval == std::string("guided")) {
      lbtypeid = LBType::guided;
    } else if (envval == std::string("work_stealing")) {
      lbtypeid = LBType::work_stealing;
    } else {
      std::cerr << "Unknown runtime load balancer taken from environmnent: '" << envval << "'!" << std::endl;
      std::cerr << "Please, check SLB4MPI_LOAD_BALANCER environmental variable!" << std::endl;
      std::cerr << "Possible values are: static, local_static, dynamic, guided, workstealing." << std::endl;
      std::cerr << "Static load balancer will be used." << std::endl;
      lbtypeid = LBType::rb_static;
    }
  } else if (lbtype == std::string("static")) {
    lbtypeid = LBType::rb_static;
  } else if (lbtype == std::string("local_static")) {
    lbtypeid = LBType::local_static;
  } else if (lbtype == std::string("dynamic")) {
    lbtypeid = LBType::dynamic;
  } else if (lbtype == std::string("guided")) {
    lbtypeid = LBType::guided;
  } else if (lbtype == std::string("work_stealing")) {
    lbtypeid = LBType::work_stealing;
  } else {
    lbtypeid = LBType::unknown;
  }

  switch(lbtypeid) {
    case LBType::rb_static:
      this->lb = std::make_unique<StaticLoadBalancer>(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size);
      break;
    case LBType::local_static:
      this->lb = std::make_unique<LocalStaticLoadBalancer>(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size);
      break;
    case LBType::dynamic:
      this->lb = std::make_unique<DynamicLoadBalancer>(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size);
      break;
    case LBType::guided:
      this->lb = std::make_unique<GuidedLoadBalancer>(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size);
      break;
    case LBType::work_stealing:
      this->lb = std::make_unique<WorkStealingLoadBalancer>(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size);
      break;
    case LBType::env:
      // must never happen
      std::cerr << "EnvLoadBalancer" << std::endl;
      std::exit(1);
      break;
    case LBType::unknown:
      // can be a bug in a program
      std::cerr << "Unknown runtime load balancer: '" << lbtype << "'!" << std::endl;
      std::exit(1);
      break;
  }
}

bool SLB4MPI::RuntimeLoadBalancer::get_range(int64_t& lower_bound, int64_t& upper_bound) {
  return this->lb->get_range(lower_bound, upper_bound);
}
