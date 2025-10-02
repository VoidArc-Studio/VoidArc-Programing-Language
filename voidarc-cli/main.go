package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

const (
	BackendCheck = "voidarc-backend"
	BackendBuild = "voidarc-backend-1"
	SourceExt    = ".va"
	BuildDir     = "build"
	StdLibDir    = "std"
)

var (
	StyleTitle   = lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("#FAFAFA")).Background(lipgloss.Color("#7D56F4"))
	StyleError   = lipgloss.NewStyle().Foreground(lipgloss.Color("#FF0000"))
	StyleInfo    = lipgloss.NewStyle().Foreground(lipgloss.Color("#00FF00"))
	StyleSuggest = lipgloss.NewStyle().Foreground(lipgloss.Color("#FFFF00"))
)

type ErrorModel struct {
	errors    []string
	suggests  []string
	cursor    int
	quit      bool
	showAll   bool
}

func NewErrorModel(errors, suggests []string) ErrorModel {
	return ErrorModel{errors: errors, suggests: suggests}
}

func (m ErrorModel) Init() tea.Cmd {
	return nil
}

func (m ErrorModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			m.quit = true
			return m, tea.Quit
		case "up":
			if m.cursor > 0 {
				m.cursor--
			}
		case "down":
			if m.cursor < len(m.errors)-1 {
				m.cursor++
			}
		case "enter":
			m.showAll = !m.showAll
		}
	}
	return m, nil
}

func (m ErrorModel) View() string {
	if m.quit {
		return ""
	}
	s := StyleTitle.Render("Błędy VoidArc:\n\n")
	for i, err := range m.errors {
		prefix := "  "
		if i == m.cursor {
			prefix = "> "
		}
		s += StyleError.Render(prefix + err + "\n")
		if m.showAll && i < len(m.suggests) {
			s += StyleSuggest.Render("  Sugestia: " + m.suggests[i] + "\n")
		}
	}
	s += "\nNaciśnij q aby wyjść, enter aby przełączyć sugestie.\n"
	return s
}

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	command := os.Args[1]
	args := os.Args[2:]

	switch command {
	case "init":
		handleInit(args)
	case "check":
		handleCheck()
	case "build":
		handleBuild(args)
	case "run":
		handleRun(args)
	case "clear":
		handleClear()
	default:
		printUsage()
		os.Exit(1)
	}
}

func printUsage() {
	fmt.Println(StyleInfo.Render("Użycie CLI VoidArc:"))
	fmt.Println("  voidarc-lang init <nazwa-projektu>  - Tworzy nowy projekt")
	fmt.Println("  voidarc-lang check                  - Sprawdza pliki źródłowe")
	fmt.Println("  voidarc-lang build [windows|linux]  - Buduje binarkę")
	fmt.Println("  voidarc-lang run [args...]          - Buduje i uruchamia")
	fmt.Println("  voidarc-lang clear                  - Czyści artefakty")
}

func handleInit(args []string) {
	if len(args) != 1 {
		fmt.Println(StyleError.Render("Użycie: init <nazwa-projektu>"))
		os.Exit(1)
	}
	projectName := args[0]
	if err := os.Mkdir(projectName, 0755); err != nil {
		fmt.Println(StyleError.Render("Błąd: " + err.Error()))
		os.Exit(1)
	}
	mainFile := filepath.Join(projectName, "main"+SourceExt)
	sampleCode := `use std::console;

fn main() -> i32 {
    let x: i32 = 42;
    console.log("Witaj w VoidArc! Wartość x to ${x}");
    return 0;
}`
	if err := ioutil.WriteFile(mainFile, []byte(sampleCode), 0644); err != nil {
		fmt.Println(StyleError.Render("Błąd zapisu: " + err.Error()))
		os.Exit(1)
	}

	stdDir := filepath.Join(projectName, StdLibDir)
	if err := os.Mkdir(stdDir, 0755); err != nil {
		fmt.Println(StyleError.Render("Błąd: " + err.Error()))
		os.Exit(1)
	}
	consoleFile := filepath.Join(stdDir, "console.va")
	stdCode := `pub fn log(msg: string) {
    // Implementacja natywna
}`
	if err := ioutil.WriteFile(consoleFile, []byte(stdCode), 0644); err != nil {
		fmt.Println(StyleError.Render("Błąd zapisu: " + err.Error()))
		os.Exit(1)
	}

	fmt.Println(StyleInfo.Render("Projekt zainicjowany: " + projectName))
}

func handleCheck() {
	sources, err := findAllSources(".")
	if err != nil {
		fmt.Println(StyleError.Render("Błąd wyszukiwania źródeł: " + err.Error()))
		os.Exit(1)
	}
	if len(sources) == 0 {
		fmt.Println(StyleError.Render("Brak plików .va"))
		os.Exit(1)
	}

	cmd := exec.Command(BackendCheck, append([]string{"check"}, sources...)...)
	output, err := cmd.CombinedOutput()
	outStr := string(output)
	if err != nil {
		errorLines := strings.Split(outStr, "\n")
		var errors, suggests []string
		for _, line := range errorLines {
			if strings.HasPrefix(line, "Error:") {
				errors = append(errors, line[7:])
			} else if strings.HasPrefix(line, "Suggest:") {
				suggests = append(suggests, line[9:])
			}
		}
		p := tea.NewProgram(NewErrorModel(errors, suggests))
		if _, err := p.Run(); err != nil {
			fmt.Println(StyleError.Render("Błąd TUI: " + err.Error()))
		}
		os.Exit(1)
	}
	fmt.Println(StyleInfo.Render("Sprawdzenie OK"))
}

func handleBuild(args []string) {
	target := getTarget(args)
	handleCheck()

	sources, _ := findAllSources(".")
	cmdArgs := []string{"build", "--target", target}
	cmdArgs = append(cmdArgs, sources...)
	cmd := exec.Command(BackendBuild, cmdArgs...)
	output, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Println(StyleError.Render("Budowa nieudana: " + string(output)))
		os.Exit(1)
	}
	fmt.Println(StyleInfo.Render("Zbudowano: " + getBinaryName(target)))
}

func handleRun(args []string) {
	target := getTarget([]string{})
	handleBuild([]string{target})
	bin := getBinaryName(target)
	if runtime.GOOS == "windows" || target == "windows" {
		bin = ".\\" + bin
	} else {
		bin = "./" + bin
	}
	cmd := exec.Command(bin, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fmt.Println(StyleError.Render("Uruchomienie nieudane: " + err.Error()))
	}
}

func handleClear() {
	if err := os.RemoveAll(BuildDir); err != nil {
		fmt.Println(StyleError.Render("Błąd czyszczenia: " + err.Error()))
	}
	files, _ := ioutil.ReadDir(".")
	for _, f := range files {
		name := f.Name()
		if strings.HasSuffix(name, ".exe") || name == "main" || strings.HasSuffix(name, ".c") || strings.HasSuffix(name, ".o") || strings.HasSuffix(name, ".obj") {
			os.Remove(name)
		}
	}
	fmt.Println(StyleInfo.Render("Wyczyszczono"))
}

func findAllSources(dir string) ([]string, error) {
	var sources []string
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(info.Name(), SourceExt) {
			sources = append(sources, path)
		}
		return nil
	})
	return sources, err
}

func getTarget(args []string) string {
	if len(args) > 0 {
		t := args[0]
		if t == "windows" || t == "linux" {
			return t
		}
	}
	if runtime.GOOS == "windows" {
		return "windows"
	}
	return "linux"
}

func getBinaryName(target string) string {
	if target == "windows" {
		return "main.exe"
	}
	return "main"
}
