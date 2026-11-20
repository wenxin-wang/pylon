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
        self.remote = self.tmp / "remote"
        self.remote.mkdir()
        run("git", "init", "-q", "-b", "main", cwd=self.remote)
        run("git", "config", "user.email", "t@example.com", cwd=self.remote)
        run("git", "config", "user.name", "t", cwd=self.remote)
        self.add_skill("workshop")
        self.checkout = self.tmp / "share/pylon/main"
        self.tool_dirs = [self.tmp / "claude/skills", self.tmp / "agents/skills"]

    def add_skill(self, name):
        skill = self.remote / "skills" / name
        skill.mkdir(parents=True)
        (skill / "SKILL.md").write_text(f"# {name}\n")
        run("git", "add", "-A", cwd=self.remote)
        run("git", "commit", "-q", "-m", f"add {name}", cwd=self.remote)

    def update(self):
        skills.update_checkout(self.checkout, str(self.remote))
        return skills.link_skills(self.checkout, self.tool_dirs)

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

    def test_remove_keeps_foreign_entries(self):
        self.update()
        foreign = self.tool_dirs[0] / "mine"
        foreign.mkdir()
        skills.remove_links(self.checkout, self.tool_dirs)
        self.assertEqual([p.name for p in self.tool_dirs[0].iterdir()], ["mine"])


if __name__ == "__main__":
    unittest.main()
