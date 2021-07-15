# Search

A library for searching lists of data y content and date


## Examples

The data is `colors` with element of the form

```elm
type alias Datum = { content: String, dateTime: Time.Posix }
```

Simple search
```
> search NotCaseSensitive "yellow" colors 
  |> List.map .content
  ["alizarin yellow","yellow ochre","french yellow"]
```  
  
Search on fragments
```
> search NotCaseSensitive "fr" colors |> List.map .content
  ["french yellow"]
```

Conjunctive search
```
> search NotCaseSensitive "yell french" colors |> List.map .content
["french yellow"] : List String
```

Negation
```
> search NotCaseSensitive "yell -french" colors |> List.map .content
["alizarin yellow","yellow ochre"]
```

Date-time:
```
> search NotCaseSensitive "@before:7/1/2021" colors 
  |> List.map .content
  ["alizarin yellow","brown umber"]
```

Date-time and word fragment
```
> search NotCaseSensitive "@before:7/1/2021 yellow" colors 
  |> List.map .content
  ["alizarin yellow"]
```