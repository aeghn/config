# 读取输入
$inputText = Read-Host -Prompt "请输入新的 ip"

# 文件路径变量
$filePath = "C:\Windows\System32\drivers\etc\hosts"

# 寻找文件中以 chinslt.com 关键字开头的行，并进行替换
$matched = $false
$content = Get-Content -Path $filePath | ForEach-Object {
    if ($_ -match '^chinslt\.com') {
        $matched = $true
        $_ -replace '.*chinslt\.com', "$inputText chinslt.com"
    } else {
        $_
    }
}

if (-not $matched) {
    $content += "$inputText chinslt.com"
}

$content | Set-Content -Path $filePath