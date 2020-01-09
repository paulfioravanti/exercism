require "spec"
require "../src/*"

describe "ResistorColorDuo" do
  it "Brown and black" do
    ResistorColorDuo.value(["brown", "black"]).should eq(10)
  end

  it "Blue and grey" do
    ResistorColorDuo.value(["blue", "grey"]).should eq(68)
  end

  it "Yellow and violet" do
    ResistorColorDuo.value(["yellow", "violet"]).should eq(47)
  end

  it "Orange and orange" do
    ResistorColorDuo.value(["orange", "orange"]).should eq(33)
  end
end
