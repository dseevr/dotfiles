require 'irb/ext/save-history'

IRB.conf[:SAVE_HISTORY] = 9999
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-history"
IRB.conf[:AUTO_INDENT] = true
IRB.conf[:USE_READLINE] = true
IRB.conf[:PROMPT_MODE]  = :SIMPLE

require 'irbtools'
require 'irbtools/more'

require "did_you_mean"
require "looksee"
require "binding_of_caller"
require "bond"

begin
  require 'hirb'
  Hirb::Formatter.dynamic_config['ActiveRecord::Base']
  Hirb.enable
rescue
end

def time(times = 1)
  require 'benchmark'
  ret = nil
  Benchmark.bm { |x| x.report { times.times { ret = yield } } }
  ret
end
