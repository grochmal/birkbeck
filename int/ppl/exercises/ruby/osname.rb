#!/usr/bin/env ruby

class OpSys
  def initialize(osname)
    @osname = osname
    @info = {:Windows => 'doXWindows',
	     :Linux   => 'doXLinux',
	     :Mac     => 'doXMac'     }
  end

  def doX
    puts (@info[@osname] or 'unknown OS')
  end
end

os = OpSys.new :Linux
os.doX
os = OpSys.new :Windows
os.doX
os = OpSys.new :Mac
os.doX
os = OpSys.new :SthElse
os.doX

