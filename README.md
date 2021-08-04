# Search

A library for searching and sorting lists of data.

Suppose we have data with fields `title: String` and `creationDate: Teime.Posix` and others.
First, we rig up a function

```elm
dataTransform : {a | title : String , creationDate : Time.Posix} 
                -> {targetContent : String, targetDate : Time.Posix }
dataTransform  { content, dateTime } = {targetContent = content , targetDate = dateTime }
```

Second, we define

```elm
search = Search.search dataTransform
sort = Sort.sort dataTransform
```
Then we can do searches and sorts as below:


## Examples

The data is `colors` with elements of the form

```
type alias Datum = { content: String, dateTime: Time.Posix }
```

```
> colors 
  |> List.map .content
  ["alizarin yellow","brown umber","yellow ochre","pthalo blue"
  ,"french yellow","alizarin crimson, cadmium purple"]
```

Simple search
```
> search NotCaseSensitive "yellow" colors 
  |> List.map .content
  ["alizarin yellow","yellow ochre","french yellow"]
```  
  
Search on fragments
```
> search NotCaseSensitive "fr" colors 
  |> List.map .content
  ["french yellow"]
```

Conjunctive search
```
> search NotCaseSensitive  "yell french" colors 
  |> List.map .content
  ["french yellow"] : List String
```

Negation
```
> search NotCaseSensitive "yell -french" colors 
  |> List.map .content
  ["alizarin yellow","yellow ochre"]
```

Date-time:
```
> search NotCaseSensitive  "@before:7/1/2021" colors 
  |> List.map .content
  ["alizarin yellow","brown umber"]
```

Date-time and word fragment
```
> search NotCaseSensitive "@before:7/1/2021 yellow" colors 
  |> List.map .content
  ["alizarin yellow"]
```

## Sorting

### Alphabetical

```
> sort (Alpha Increasing) colors 
  |> List.map .content
  ["alizarin crimson, cadmium purple","alizarin yellow"
  ,"brown umber","french yellow","pthalo blue","yellow ochre"]
```

### Date

```
> sort (DateTime  Decreasing) colors |> List.map .content
  ["alizarin crimson, cadmium purple","french yellow","pthalo blue"
  ,"yellow ochre","brown umber","alizarin yellow"]
```

### Random order

```
> sort tranform (Random seed1) colors 
  |> List.map .content
  ["alizarin crimson, cadmium purple","alizarin yellow"
  ,"brown umber","yellow ochre","french yellow","pthalo blue"]
  
> sort (Random seed2) colors 
  |> List.map .content
  ["yellow ochre","pthalo blue","brown umber","french yellow"
  ,"alizarin yellow","alizarin crimson, cadmium purple"]
```

