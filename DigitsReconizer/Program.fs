// Learn more about F# at http://fsharp.org

open System
open System.IO

type Observation = { Label:string; Pixels:int[] }
type Distance = int[]*int[] -> int
type Classifier = int[] -> string


let toObservation (csvData:string) = 
        let columns = csvData.Split(',')
        let label = columns.[0]
        let pixels = columns.[1..] |> Array.map int
        { Label = label; Pixels = pixels }

let reader path = 
        let data = File.ReadAllLines path
        data.[1..] |> Array.map toObservation
let trainingpath = __SOURCE_DIRECTORY__ + @"/../Data/trainingsample.csv"
let training = reader trainingpath
let mutable count = 0

let manhattanDistance (pixels1,pixels2) = 
        let zip = Array.zip pixels1 pixels2
        let map = Array.map (fun (x,y) -> abs (x-y)) zip 
        let sum = Array.sum map
        printfn "count: %i, sum: %i" count sum
        count <- count + 1
        sum

let euclideanDistance (pixels1, pixels2) = 
        Array.zip pixels1 pixels2
        |> Array.map (fun (x, y) -> pown (x-y) 2)
        |> Array.sum

let train (trainingset:Observation[]) (dist:Distance) = 
        let classify (pixels:int[]) = 
                trainingset
                |> Array.minBy (fun x -> dist (x.Pixels, pixels)) |> fun x -> x.Label
        classify

let validationpath = __SOURCE_DIRECTORY__ + @"/../Data/validationsample.csv"
let validation = reader validationpath

let evaluate validationSet classifier = 
        validationSet
        |> Array.averageBy (fun x -> if classifier x.Pixels = x.Label then 1. else 0.)
        |> printfn "Correct: %.3f"

let manhattanModel = train training euclideanDistance

printfn "Euclidean"
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
evaluate validation manhattanModel
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey(true) |> ignore