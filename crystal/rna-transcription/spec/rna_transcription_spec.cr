require "spec"
require "../src/*"

describe "RnaComplement" do
  describe "#of_dna" do
    it "correctly transcribes cytosine to guanine" do
      RnaComplement.of_dna("C").should eq "G"
    end

    it "correctly transcribes guanine to cytocine" do
      RnaComplement.of_dna("G").should eq "C"
    end

    it "correctly transcribes thymine to adenine" do
      RnaComplement.of_dna("T").should eq "A"
    end

    it "correctly transcribes adenine to uracil" do
      RnaComplement.of_dna("A").should eq "U"
    end

    it "correctly transcribes all dna nucleotides to their rna compliment" do
      RnaComplement.of_dna("ACGTGGTCTTAA").should eq "UGCACCAGAAUU"
    end
  end
end
