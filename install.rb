#!/usr/bin/env ruby
# encoding: utf-8

require "fileutils"

FILE_DIR = Dir.pwd + "/files"
HOME_DIR = ENV["HOME"]

# get a list of all files
filenames = Dir.entries(FILE_DIR).reject { |f| %w[. ..].include?(f) }

special_linkages = {}
special_linkages["htoprc"] = ".config/htop"

filenames.each do |filename|
  target_dir = nil

  if special_linkages.key?(filename)
    target_dir = HOME_DIR + "/" + special_linkages[filename]
    FileUtils.mkdir_p(target_dir)
  else
    target_dir = HOME_DIR
  end

  target_filename = target_dir + "/" + filename
  source_filename = FILE_DIR + "/" + filename

  exists = false

  begin
    exists = File.lstat(target_filename).symlink?
  rescue Errno::ENOENT
  end

  if exists
    puts "WARNING: skipping `#{filename}' because symlink `#{target_filename}' already exists"
  else
    puts "Linking `#{source_filename}' -> `#{target_filename}'"
    FileUtils.ln_s(source_filename, target_filename)
  end
end
