# ��ȡ����
$inputText = Read-Host -Prompt "�������µ� ip"

# �ļ�·������
$filePath = "C:\Windows\System32\drivers\etc\hosts"

# Ѱ���ļ����� chinslt.com �ؼ��ֿ�ͷ���У��������滻
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