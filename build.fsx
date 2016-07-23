#r @"C:\Users\Owner\Source\Repos\ProConFs\packages\FAKE.4.8.0\tools\FakeLib.dll"

open Fake

// Default target
Target "Default" (fun _ ->
    trace "Hello World from FAKE"
)

// start build
RunTargetOrDefault "Default"
