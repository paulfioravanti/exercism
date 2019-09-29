defmodule ScaleGeneratorTest do
  use ExUnit.Case

  @major_scale_pattern "MMmMMMm"
  @minor_scale_pattern "MmMMmMM"
  @dorian_scale_pattern "MmMMMmM"
  @mixolydian_scale_pattern "MMmMMmM"
  @lydian_scale_pattern "MMMmMMm"
  @phrygian_scale_pattern "mMMMmMM"
  @locrian_scale_pattern "mMMmMMM"
  @harmonic_minor_scale_pattern "MmMMmAm"
  @melodic_minor_scale_pattern "MmMMMMm"
  @octatonic_scale_pattern "MmMmMmMm"
  @hexatonic_scale_pattern "MMMMMM"
  @pentatonic_scale_pattern "MMAMA"
  @enigmatic_scale_pattern "mAMMMmm"

  describe "step to next note" do
    test "with half-tone interval" do
      assert ScaleGenerator.step(~w(C C# D D# E F F# G G# A A# B), "C", "m") == "C#"
    end

    test "with full tone interval" do
      assert ScaleGenerator.step(~w(C C# D D# E F F# G G# A A# B), "C", "M") == "D"
    end

    test "with accidental interval" do
      assert ScaleGenerator.step(~w(C C# D D# E F F# G G# A A# B), "C", "A") == "D#"
    end
  end

  describe "generate chromatic scale" do
    test "starting with A" do
      assert ScaleGenerator.chromatic_scale("A") == ~w(A A# B C C# D D# E F F# G G# A)
    end

    test "starting with C" do
      assert ScaleGenerator.chromatic_scale("C") == ~w(C C# D D# E F F# G G# A A# B C)
    end

    test "starting with G" do
      assert ScaleGenerator.chromatic_scale("G") == ~w(G G# A A# B C C# D D# E F F# G)
    end

    test "works with with lowercase notes" do
      assert ScaleGenerator.chromatic_scale("f#") == ~w(F# G G# A A# B C C# D D# E F F#)
    end
  end

  describe "generate flat chromatic scale" do
    test "starting with A" do
      assert ScaleGenerator.flat_chromatic_scale("A") == ~w(A Bb B C Db D Eb E F Gb G Ab A)
    end

    test "starting with C" do
      assert ScaleGenerator.flat_chromatic_scale("C") == ~w(C Db D Eb E F Gb G Ab A Bb B C)
    end

    test "starting with G" do
      assert ScaleGenerator.flat_chromatic_scale("G") == ~w(G Ab A Bb B C Db D Eb E F Gb G)
    end

    test "works with with lowercase notes" do
      assert ScaleGenerator.flat_chromatic_scale("Gb") == ~w(Gb G Ab A Bb B C Db D Eb E F Gb)
    end
  end

  describe "find chromatic scale for flat tonics" do
    test "using F" do
      assert ScaleGenerator.find_chromatic_scale("F") == ~w(F Gb G Ab A Bb B C Db D Eb E F)
    end

    test "using Bb" do
      assert ScaleGenerator.find_chromatic_scale("Bb") == ~w(Bb B C Db D Eb E F Gb G Ab A Bb)
    end

    test "using Eb" do
      assert ScaleGenerator.find_chromatic_scale("Eb") == ~w(Eb E F Gb G Ab A Bb B C Db D Eb)
    end

    test "using Ab" do
      assert ScaleGenerator.find_chromatic_scale("Ab") == ~w(Ab A Bb B C Db D Eb E F Gb G Ab)
    end

    test "using Db" do
      assert ScaleGenerator.find_chromatic_scale("Db") == ~w(Db D Eb E F Gb G Ab A Bb B C Db)
    end

    test "using Gb" do
      assert ScaleGenerator.find_chromatic_scale("Gb") == ~w(Gb G Ab A Bb B C Db D Eb E F Gb)
    end

    test "using d" do
      assert ScaleGenerator.find_chromatic_scale("d") == ~w(D Eb E F Gb G Ab A Bb B C Db D)
    end

    test "using g" do
      assert ScaleGenerator.find_chromatic_scale("g") == ~w(G Ab A Bb B C Db D Eb E F Gb  G)
    end

    test "using c" do
      assert ScaleGenerator.find_chromatic_scale("c") == ~w(C Db D Eb E F Gb G Ab A Bb B  C)
    end

    test "using f" do
      assert ScaleGenerator.find_chromatic_scale("f") == ~w(F Gb G Ab A Bb B C Db D Eb E F)
    end

    test "using bb" do
      assert ScaleGenerator.find_chromatic_scale("bb") == ~w(Bb B C Db D Eb E F Gb G Ab A Bb)
    end

    test "using eb" do
      assert ScaleGenerator.find_chromatic_scale("eb") == ~w(Eb E F Gb G Ab A Bb B C Db D Eb)
    end
  end

  describe "find chromatic scale for non-flat tonics" do
    test "using A" do
      assert ScaleGenerator.find_chromatic_scale("A") == ~w(A A# B C C# D D# E F F# G G# A)
    end

    test "using A#" do
      assert ScaleGenerator.find_chromatic_scale("A#") == ~w(A# B C C# D D# E F F# G G# A A#)
    end

    test "using B" do
      assert ScaleGenerator.find_chromatic_scale("B") == ~w(B C C# D D# E F F# G G# A A# B)
    end

    test "using C" do
      assert ScaleGenerator.find_chromatic_scale("C") == ~w(C C# D D# E F F# G G# A A# B C)
    end

    test "using C#" do
      assert ScaleGenerator.find_chromatic_scale("C#") == ~w(C# D D# E F F# G G# A A# B C C#)
    end

    test "using D" do
      assert ScaleGenerator.find_chromatic_scale("D") == ~w(D D# E F F# G G# A A# B C C# D)
    end

    test "using D#" do
      assert ScaleGenerator.find_chromatic_scale("D#") == ~w(D# E F F# G G# A A# B C C# D D#)
    end

    test "using E" do
      assert ScaleGenerator.find_chromatic_scale("E") == ~w(E F F# G G# A A# B C C# D D# E)
    end

    test "using F#" do
      assert ScaleGenerator.find_chromatic_scale("F#") == ~w(F# G G# A A# B C C# D D# E F F#)
    end

    test "using G" do
      assert ScaleGenerator.find_chromatic_scale("G") == ~w(G G# A A# B C C# D D# E F F# G)
    end

    test "using G#" do
      assert ScaleGenerator.find_chromatic_scale("G#") == ~w(G# A A# B C C# D D# E F F# G G#)
    end
  end

  describe "generate scale from tonic and pattern" do
    test "C Major scale" do
      assert ScaleGenerator.scale("C", @major_scale_pattern) == ~w(C D E F G A B C)
    end

    test "G Major scale" do
      assert ScaleGenerator.scale("G", @major_scale_pattern) == ~w(G A B C D E F# G)
    end

    test "f# minor scale" do
      assert ScaleGenerator.scale("f#", @minor_scale_pattern) == ~w(F# G# A B C# D E F#)
    end

    test "b flat minor scale" do
      assert ScaleGenerator.scale("bb", @minor_scale_pattern) == ~w(Bb C Db Eb F Gb Ab Bb)
    end

    test "D Dorian scale" do
      assert ScaleGenerator.scale("d", @dorian_scale_pattern) == ~w(D E F G A B C D)
    end

    test "E flat Mixolydian scale" do
      assert ScaleGenerator.scale("Eb", @mixolydian_scale_pattern) == ~w(Eb F G Ab Bb C Db Eb)
    end

    test "a Lydian scale" do
      assert ScaleGenerator.scale("a", @lydian_scale_pattern) == ~w(A B C# D# E F# G# A)
    end

    test "e Phrygian scale" do
      assert ScaleGenerator.scale("e", @phrygian_scale_pattern) == ~w(E F G A B C D E)
    end

    test "g Locrian scale" do
      assert ScaleGenerator.scale("g", @locrian_scale_pattern) == ~w(G Ab Bb C Db Eb F G)
    end

    test "d Harmonic minor scale" do
      assert ScaleGenerator.scale("d", @harmonic_minor_scale_pattern) == ~w(D E F G A Bb Db D)
    end

    test "C Melodic minor scale" do
      assert ScaleGenerator.scale("C", @melodic_minor_scale_pattern) == ~w(C D D# F G A B C)
    end

    test "C Octatonic scale" do
      assert ScaleGenerator.scale("C", @octatonic_scale_pattern) == ~w(C D D# F F# G# A B C)
    end

    test "D flat Hexatonic scale" do
      assert ScaleGenerator.scale("Db", @hexatonic_scale_pattern) == ~w(Db Eb F G A B Db)
    end

    test "A Pentatonic scale" do
      assert ScaleGenerator.scale("A", @pentatonic_scale_pattern) == ~w(A B C# E F# A)
    end

    test "G Enigmatic scale" do
      assert ScaleGenerator.scale("G", @enigmatic_scale_pattern) == ~w(G G# B C# D# F F# G)
    end
  end
end
