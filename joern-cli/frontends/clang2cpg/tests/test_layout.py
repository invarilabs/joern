from pathlib import Path
import subprocess
import unittest

MODULE = Path(__file__).resolve().parents[1]
REPO = Path(subprocess.check_output(['git','rev-parse','--show-toplevel'],cwd=MODULE,text=True).strip())
UPSTREAM = '07896d7a9c66920e41052e82a19362006f517d33'


class IsolationTest(unittest.TestCase):
    def test_upstream_c2cpg_is_restored(self):
        diff = subprocess.check_output(['git','diff',UPSTREAM,'--','joern-cli/frontends/c2cpg'],cwd=REPO,text=True)
        self.assertEqual(diff, '', 'c2cpg must match the pinned upstream tree')

    def test_standalone_build_and_compatible_main(self):
        self.assertTrue((MODULE/'build.sbt').is_file())
        self.assertTrue((MODULE/'project/build.properties').is_file())
        shim = MODULE/'src/main/scala/io/joern/c2cpg/Main.scala'
        self.assertIn('io.joern.clang2cpg.Main.main(args)', shim.read_text())

    def test_no_cdt_or_completed_facts_dependency(self):
        sources = '\n'.join(p.read_text() for p in (MODULE/'src/main/scala').rglob('*.scala'))
        self.assertTrue(sources)
        for forbidden in ('org.eclipse', 'clangFactsCommand', 'completedFacts', 'CTypeIdentity'):
            self.assertNotIn(forbidden, sources)
