import subprocess
import tempfile
import unittest
from pathlib import Path

from pylon import skills


def run(*args, cwd):
    subprocess.run(args, cwd=cwd, check=True, capture_output=True)


class SkillsTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = Path(tmp.name)
        # A local repo stands in for GitHub.
        self.remote = self.make_remote("remote")
        self.add_skill("workshop")
        self.checkout = self.tmp / "share/pylon/agent-policy"
        self.checkouts = [self.checkout]
        self.tool_dirs = [self.tmp / "claude/skills", self.tmp / "agents/skills"]

    def make_remote(self, name):
        remote = self.tmp / name
        remote.mkdir()
        run("git", "init", "-q", "-b", "main", cwd=remote)
        run("git", "config", "user.email", "t@example.com", cwd=remote)
        run("git", "config", "user.name", "t", cwd=remote)
        return remote

    def add_skill(self, name, remote=None):
        remote = remote or self.remote
        skill = remote / "skills" / name
        skill.mkdir(parents=True)
        (skill / "SKILL.md").write_text(f"# {name}\n")
        run("git", "add", "-A", cwd=remote)
        run("git", "commit", "-q", "-m", f"add {name}", cwd=remote)

    def update(self):
        skills.update_checkout(self.checkout, str(self.remote))
        return skills.link_skills(self.checkouts, self.tool_dirs)

    def add_source(self):
        remote = self.make_remote("other-remote")
        self.add_skill("other", remote)
        checkout = self.tmp / "share/pylon/other"
        skills.update_checkout(checkout, str(remote))
        self.checkouts.append(checkout)
        return remote, checkout

    def test_links_every_skill_into_every_tool(self):
        self.assertEqual(self.update(), [])
        for tool_dir in self.tool_dirs:
            self.assertEqual((tool_dir / "workshop/SKILL.md").read_text(),
                             "# workshop\n")

    def test_update_follows_main(self):
        self.update()
        self.add_skill("other")
        self.update()
        self.assertTrue((self.tool_dirs[0] / "other/SKILL.md").is_file())

    def test_removed_skill_loses_its_link(self):
        self.update()
        run("git", "rm", "-q", "-r", "skills/workshop", cwd=self.remote)
        self.add_skill("other")
        self.update()
        self.assertFalse((self.tool_dirs[0] / "workshop").is_symlink())

    def test_leaves_foreign_entries_alone(self):
        foreign = self.tool_dirs[0] / "workshop"
        foreign.mkdir(parents=True)
        problems = self.update()
        self.assertEqual(len(problems), 1)
        self.assertFalse(foreign.is_symlink())
        self.assertTrue((self.tool_dirs[1] / "workshop").is_symlink())

    def test_refuses_local_changes(self):
        self.update()
        (self.checkout / "skills/workshop/SKILL.md").write_text("edited\n")
        with self.assertRaises(skills.Error):
            skills.update_checkout(self.checkout, str(self.remote))

    def test_refuses_other_branch(self):
        self.update()
        run("git", "switch", "-q", "-c", "topic", cwd=self.checkout)
        with self.assertRaises(skills.Error):
            skills.update_checkout(self.checkout, str(self.remote))

    def test_links_from_several_sources(self):
        self.add_source()
        self.assertEqual(self.update(), [])
        names = sorted(p.name for p in self.tool_dirs[0].iterdir())
        self.assertEqual(names, ["other", "workshop"])

    def test_same_name_in_two_sources_is_skipped(self):
        self.update()
        remote, checkout = self.add_source()
        self.add_skill("workshop", remote)
        skills.update_checkout(checkout, str(remote))
        problems = self.update()
        self.assertEqual(len(problems), 1)
        self.assertFalse((self.tool_dirs[0] / "workshop").exists())
        self.assertTrue((self.tool_dirs[0] / "other").is_symlink())

    def test_remove_keeps_foreign_entries(self):
        self.update()
        foreign = self.tool_dirs[0] / "mine"
        foreign.mkdir()
        skills.remove_links(self.checkouts, self.tool_dirs)
        self.assertEqual([p.name for p in self.tool_dirs[0].iterdir()], ["mine"])


class PreferencesTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = Path(tmp.name)
        self.checkout = self.tmp / "agent-policy"
        self.checkout.mkdir()
        self.files = [self.tmp / "claude/CLAUDE.md", self.tmp / "codex/AGENTS.md"]

    def write_preferences(self):
        (self.checkout / "preferences.md").write_text("Beijing time.\n")

    def test_links_every_global_file(self):
        self.write_preferences()
        self.assertEqual(skills.link_preferences(self.checkout, self.files), [])
        for f in self.files:
            self.assertEqual(f.read_text(), "Beijing time.\n")

    def test_no_preferences_no_links(self):
        self.write_preferences()
        skills.link_preferences(self.checkout, self.files)
        (self.checkout / "preferences.md").unlink()
        skills.link_preferences(self.checkout, self.files)
        for f in self.files:
            self.assertFalse(f.is_symlink())

    def test_leaves_existing_file_alone(self):
        self.write_preferences()
        self.files[0].parent.mkdir()
        self.files[0].write_text("the human's own\n")
        problems = skills.link_preferences(self.checkout, self.files)
        self.assertEqual(len(problems), 1)
        self.assertEqual(self.files[0].read_text(), "the human's own\n")
        self.assertTrue(self.files[1].is_symlink())

    def test_remove_keeps_existing_file(self):
        self.write_preferences()
        self.files[0].parent.mkdir()
        self.files[0].write_text("the human's own\n")
        skills.link_preferences(self.checkout, self.files)
        skills.remove_preferences(self.checkout, self.files)
        self.assertTrue(self.files[0].is_file())
        self.assertFalse(self.files[1].exists())


if __name__ == "__main__":
    unittest.main()
