# get the MIME type using .NET's MimeMapping class

param (
  [string]$filePath
)

Write-Host [System.Web.MimeMapping]::GetMimeMapping($filePath)
