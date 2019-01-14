module RobotName

open System

type Robot(name) =
    do  
        Robot.ExistingNames = Robot.ExistingNames @ [name] |> ignore

    static member ExistingNames:List<string> = []
    member _this.Name = name


let mkRobot() = 
    let rand = new Random()
    let rec generateName() : string =
        let name =
            [
                yield! [1..2] |> List.map (fun _ -> ['A'..'Z'].[rand.Next(['A'..'Z'].Length)])
                yield! [1..3] |> List.map (fun _ -> ['0'..'9'].[rand.Next(['0'..'9'].Length)])
            ]
            |> String.Concat
        
        if List.contains name Robot.ExistingNames then generateName()
        else name

    new Robot(generateName())

let name (robot: Robot) = 
    printf "%s" robot.Name
    robot.Name

let reset robot = mkRobot()

