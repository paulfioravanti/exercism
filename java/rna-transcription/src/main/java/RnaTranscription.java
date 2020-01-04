import java.util.Map;

class RnaTranscription {
    private enum Dna {
        G,
        C,
        T,
        A
    }

    private enum Rna {
        C,
        G,
        A,
        U
    }

    private static final Map<Dna, Rna> RNA_TRANSCRIPTIONS = Map.of(
        Dna.G, Rna.C,
        Dna.C, Rna.G,
        Dna.T, Rna.A,
        Dna.A, Rna.U
    );

    String transcribe(String dnaStrand) {
        if (dnaStrand.isEmpty()) {
            return dnaStrand;
        }

        StringBuilder rnaStrand = new StringBuilder();
        for (String dna : dnaStrand.split("")) {
            rnaStrand.append(RNA_TRANSCRIPTIONS.get(Dna.valueOf(dna)));
        }
        return rnaStrand.toString();
    }
}
