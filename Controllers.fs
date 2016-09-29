namespace TodoApi.Controllers

open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Suave
open Suave.Filters
open Suave.Operators
open TodoApi.Models

module Controller = 
    let fromJson<'a> json =
        let obj = JsonConvert.DeserializeObject(json, typeof<'a>) 
        if isNull obj then
            None
        else
            Some(obj :?> 'a)

    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm =
            System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>

    let JSON value =
        let settings = new JsonSerializerSettings()
        settings.ContractResolver <- new CamelCasePropertyNamesContractResolver()

        JsonConvert.SerializeObject(value, settings)
        |> Successful.OK
        >=> Writers.setMimeType "application/json; charset=utf-8"
    
    let handleResource f requestError = function
        | Some r -> r |> f
        | _ -> requestError

    let handleResourceBADREQUEST = 
        (fun f -> handleResource f (RequestErrors.BAD_REQUEST "No Resource from request"))

    let handleResourceNOTFOUND = 
        (fun f -> handleResource f (RequestErrors.NOT_FOUND "Resource not found"))

    let handleResourceCONFLICT = 
        (fun f -> handleResource f (RequestErrors.CONFLICT "Resource already exists"))
  
module TodoController = 
    let getAll db =
        warbler (fun _ -> db.GetAll() |> Controller.JSON)
    
    let find db =
        db.Find 
        >> (Controller.handleResourceNOTFOUND Controller.JSON)

    let add db =
        let addDb =
            db.Add 
            >> (Controller.handleResourceCONFLICT Controller.JSON)
        request (Controller.getResourceFromReq >> (Controller.handleResourceBADREQUEST addDb))
    
    let update db key =
        let updateDb =
            db.Update
            >> (Controller.handleResourceNOTFOUND Controller.JSON)
        request (Controller.getResourceFromReq >> (Controller.handleResourceBADREQUEST updateDb))

    let remove db =
        db.Remove 
        >> (Controller.handleResourceNOTFOUND Controller.JSON)

    let todoController (db:TodoRepository) = 
        pathStarts "/api/todo" >=> choose [
            POST >=> path "/api/todo" >=> (add db)
            GET >=> path "/api/todo" >=> (getAll db)
            GET >=> pathScan "/api/todo/%s" (find db)
            DELETE >=> pathScan "/api/todo/%s" (remove db)
            PUT >=> pathScan "/api/todo/%s" (update db)  
        ]