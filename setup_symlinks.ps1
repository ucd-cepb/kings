<#
.SYNOPSIS
    Creates a symlink from this repo to the Box-synced Kings_Large_Files/data
    folder (where large files are streamed). Windows companion to setup_symlinks.sh.

.DESCRIPTION
    Run once after cloning (or whenever the symlink needs to be recreated).

    Symlink creation on Windows requires one of:
      - Developer Mode enabled (Settings -> Privacy & security -> For developers)
      - Running this script from an elevated (Administrator) PowerShell session

    If script execution is blocked, unblock this session with:
      Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass

.PARAMETER BoxKingsData
    Optional explicit path to the Box 'Kings_Large_Files\data' folder.
    Use this to override auto-detection.

.EXAMPLE
    .\setup_symlinks.ps1

.EXAMPLE
    .\setup_symlinks.ps1 "C:\Users\me\Box\Kings_Large_Files\data"
#>

param(
    [Parameter(Position = 0)]
    [string]$BoxKingsData
)

$ErrorActionPreference = 'Stop'

$RepoDir = Split-Path -Parent $MyInvocation.MyCommand.Path

# --- Detect or accept the Box Kings_Large_Files\data root ---------------------

if (-not $BoxKingsData) {
    $candidates = @(
        (Join-Path $env:USERPROFILE 'Box\Kings_Large_Files\data'),
        (Join-Path $env:USERPROFILE 'Box Sync\Kings_Large_Files\data'),
        (Join-Path $env:USERPROFILE 'Box-Box\Kings_Large_Files\data')
    )
    $BoxKingsData = $candidates | Where-Object { Test-Path -LiteralPath $_ } | Select-Object -First 1

    if (-not $BoxKingsData) {
        Write-Host "ERROR: Could not find Kings_Large_Files\data in Box." -ForegroundColor Red
        Write-Host "Searched:"
        $candidates | ForEach-Object { Write-Host "  $_" }
        Write-Host ""
        Write-Host "Re-run with an explicit path:"
        Write-Host "  .\setup_symlinks.ps1 'C:\path\to\Box\Kings_Large_Files\data'"
        exit 1
    }
}

Write-Host "Using Box Kings_Large_Files\data at: $BoxKingsData"

# --- Create symlink -----------------------------------------------------------

$LinkPath = Join-Path $RepoDir 'data'

# Remove existing symlink or warn if something else is in the way
if (Test-Path -LiteralPath $LinkPath) {
    $item = Get-Item -LiteralPath $LinkPath -Force
    if ($item.LinkType -eq 'SymbolicLink' -or $item.Attributes.ToString() -match 'ReparsePoint') {
        Remove-Item -LiteralPath $LinkPath -Force
    } else {
        Write-Host "WARNING: $LinkPath exists and is not a symlink - skipping." -ForegroundColor Yellow
        exit 1
    }
}

if (-not (Test-Path -LiteralPath $BoxKingsData)) {
    Write-Host "  MISSING  $BoxKingsData  (symlink not created)" -ForegroundColor Red
    exit 1
}

try {
    New-Item -ItemType SymbolicLink -Path $LinkPath -Target $BoxKingsData | Out-Null
    Write-Host "  OK  $LinkPath -> $BoxKingsData" -ForegroundColor Green
} catch {
    Write-Host "ERROR: Failed to create symlink." -ForegroundColor Red
    Write-Host $_.Exception.Message
    Write-Host ""
    Write-Host "Symlink creation on Windows requires one of:"
    Write-Host "  - Developer Mode enabled (Settings -> Privacy & security -> For developers)"
    Write-Host "  - Running this script from an elevated (Administrator) PowerShell"
    exit 1
}

Write-Host ""
Write-Host "Done. Symlink created."
