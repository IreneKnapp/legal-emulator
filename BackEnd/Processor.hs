module Processor
  (
  )
  where


class Processor state where
  cycleProcessor :: state -> state
