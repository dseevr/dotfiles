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

def cc(price, strike, premium, contract_count)

  # ----- validations ---------------------------------------------------------

  price = price.to_f
  strike = strike.to_f
  premium = premium.to_f

  [price, strike, premium].each do |x|
    raise "all inputs except contract count must be > 0.0" unless x > 0.0
  end

  unless contract_count.is_a?(Integer) && contract_count > 0
    raise "contract_count must be an integer > 0"
  end

  # ----- logic begins --------------------------------------------------------

  profit_per_share = (strike - price) + premium
  profit_percentage = (profit_per_share / price) * 100.0
  share_count = contract_count * 100
  cost_basis = share_count * price

  puts "Premium per share: $%.2f (%.2f%% of $%.2f)" % [
    premium,
    (premium / price) * 100,
    price,
  ]

  puts "Total premium received: $%.2f (%.2f%% of %d shares worth $%.2f)" % [
    premium * contract_count,
    (premium / price) * 100,
    contract_count * 100,
    cost_basis,
  ]

  if strike >= price
    puts "Allows for $%.2f (%.2f%% of $%.2f) upward movement in the underlying" % [
      strike - price,
      ((strike-price) / price) * 100.0,
      price,
    ]
  else
    puts "Deducting $%.2f (%.2f%% of $%.2f) profit per share due to strike < price" % [
      price - strike,
      ((price-strike) / price) * 100.0,
      price,
    ]

    puts "Maximum profit capped to total premium received!"
  end

  puts "Maximum profit: $%.2f (%.2f%% of %d shares worth $%.2f)" % [
    profit_percentage * price * contract_count,
    profit_percentage,
    share_count,
    cost_basis,
  ]

  puts "Break even price: $%.2f (%.2f%% decline from $%.2f)" % [
    price - premium,
    (1 - (price - premium) / price) * 100,
    price,
  ]
end

def bh(b1, b2, cash=nil)
  total = b1.to_f + b2.to_f

  p2 = b1/total
  p1 = 1.0 - p2

  if cash
    c1 = cash * p1
    c2 = cash - c1

    puts "Long: $%.2f (%.1f%%)" % [c1, p1 * 100]
    puts "Short: $%.2f (%.1f%%)" % [c2, p2 * 100]
  else
    puts "Long: #{p1}"
    puts "Short: #{p2}"
  end
end
