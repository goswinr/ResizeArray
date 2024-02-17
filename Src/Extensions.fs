namespace ResizeArray

open System
open System.Collections.Generic
// open NiceString

#nowarn "44" //for opening the hidden but not Obsolete UtilResizeArray module
open UtilResizeArray


[<AutoOpen>]
module AutoOpenExtensions =

    type List<'T> with
                /// Gets an item at index
        /// (Use ResizeArray.GetNeg(i) member if you want to use negative indices too)
        member inline xs.Get index =
            if index < 0 then ArgumentOutOfRangeException.Raise "resizeArray.Get(%d) failed for ResizeArray of %d items, use resizeArray.GetNeg method if you want negative indices too:\r\n%s" index xs.Count xs.ToNiceStringLong
            if index >= xs.Count then ArgumentOutOfRangeException.Raise "resizeArray.Get(%d) failed for ResizeArray of %d items:\r\n%s" index xs.Count xs.ToNiceStringLong
            xs.[index]


        /// Sets an item at index
        /// (Use ResizeArray.SetNeg(i) member if you want to use negative indices too)
        member inline xs.Set index value =
            if index < 0 then ArgumentOutOfRangeException.Raise "The curried function resizeArray.Set %d value, failed for negative number on ResizeArray of %d items, use resizeArray.SetNeg method if you want top use negative indices too, for setting %s " index xs.Count (toNiceString value)
            if index >= xs.Count then ArgumentOutOfRangeException.Raise "tThe curried function resizeArray.Set %d value, failed for ResizeArray of %d items. for setting %s " index xs.Count (toNiceString value)
            xs.[index] <- value

        /// Shallow Structural equality comparison.
        /// Compares each element in both lists for equality. 
        /// However nested ResizeArrays inside a ResizeArray are only compared for referential equality in .NET.
        /// When used in Fable (JavaScript) the nested ResizeArrays are compared for structural equality 
        /// as per the Fable implementation of Javascript Arrays. 
        /// (Like the default behavior of Collections.Generic.List)
        /// Raises ArgumentNullException if either list is null.
        member this.IsEqualTo(other: ResizeArray<'T>) =
            if isNullSeq other then 
                ArgumentNullException.Raise "this.IsEqualTo other"
            elif Object.ReferenceEquals(this, other) then
                true
            elif this.Count <> other.Count then
                false
            else
                let comparer = EqualityComparer<'T>.Default // for  structural equality to be implemented on this class without putting the <'T when 'T : equality> constraint on 'T?
                let rec eq i =
                    if i < this.Count then
                        if comparer.Equals(this.[i], other.[i]) then
                            eq (i + 1)
                        else
                            false
                    else
                        true
                eq 0

        /// Gets the index of the last item in the ResizeArray.
        /// Equal to this.Count - 1
        /// Returns -1 for empty ResizeArray.
        member inline xs.LastIndex =
            // don't fail so that a loop for i=0 to xs.LastIndex will work for empty ResizeArray 
            //if xs.Count = 0 then ArgumentOutOfRangeException.Raise "ResizeArray.LastIndex: Failed to get LastIndex of empty %s" xs.ToNiceStringLong // ResizeArray<%s>" (typeof<'T>).FullName
            xs.Count - 1

        /// Get (or set) the last item in the ResizeArray.
        /// Equal to this.[this.Count - 1]
        member inline xs.Last
            with get () =
                if xs.Count = 0 then ArgumentOutOfRangeException.Raise "ResizeArray.Last: Failed to get last item of empty %s" xs.ToNiceStringLong // ResizeArray<%s>" (typeof<'T>).FullName
                xs.[xs.Count - 1]
            and set (v: 'T) =
                if xs.Count = 0 then ArgumentOutOfRangeException.Raise "ResizeArray.Last: Failed to set last item of %s to %s" xs.ToNiceStringLong (toNiceString v)
                xs.[xs.Count - 1] <- v

        /// Get (or set) the second last item in the ResizeArray.
        /// Equal to this.[this.Count - 2]
        member inline xs.SecondLast
            with get () =
                if xs.Count < 2 then ArgumentOutOfRangeException.Raise "ResizeArray.SecondLast: Failed to get second last item of %s" xs.ToNiceStringLong
                xs.[xs.Count - 2]
            and set (v: 'T) =
                if xs.Count < 2 then ArgumentOutOfRangeException.Raise "ResizeArray.SecondLast: Failed to set second last item of %s to %s" xs.ToNiceStringLong (toNiceString v)
                xs.[xs.Count - 2] <- v


        /// Get (or set) the third last item in the ResizeArray.
        /// Equal to this.[this.Count - 3]
        member inline xs.ThirdLast
            with get () =
                if xs.Count < 3 then ArgumentOutOfRangeException.Raise "ResizeArray.ThirdLast: Failed to get third last item of %s." xs.ToNiceStringLong
                xs.[xs.Count - 3]
            and set (v: 'T) =
                if xs.Count < 3 then ArgumentOutOfRangeException.Raise "ResizeArray.ThirdLast: Failed to set third last item of %s to %s" xs.ToNiceStringLong (toNiceString v)
                xs.[xs.Count - 3] <- v

        /// Get (or set) the first item in the ResizeArray.
        /// Equal to this.[0]
        member inline xs.First
            with get () =
                if xs.Count = 0 then ArgumentOutOfRangeException.Raise "ResizeArray.First: Failed to get first item of empty %s" xs.ToNiceStringLong // ResizeArray<%s>" (typeof<'T>).FullName
                xs.[0]
            and set (v: 'T) =
                if xs.Count = 0 then ArgumentOutOfRangeException.Raise "ResizeArray.First: Failed to set first item of empty %s" xs.ToNiceStringLong // ResizeArray<%s> to %s" (typeof<'T>).FullName (toNiceString v)
                xs.[0] <- v

        /// Gets the the only item in the ResizeArray.
        /// Fails if the ResizeArray does not have exactly one element.
        member inline xs.FirstAndOnly =
            if xs.Count = 0 then ArgumentOutOfRangeException.Raise "ResizeArray.FirstOnly: Failed to get first item of empty %s" xs.ToNiceStringLong // ResizeArray<%s>" (typeof<'T>).FullName
            if xs.Count > 1 then ArgumentOutOfRangeException.Raise "ResizeArray.FirstOnly: ResizeArray is expected to have only one item but has %d ResizeArray: %s" xs.Count xs.ToNiceStringLong
            xs.[0]


        /// Get (or set) the second item in the ResizeArray.
        /// Equal to this.[1]
        member inline xs.Second
            with get () =
                if xs.Count < 2 then ArgumentOutOfRangeException.Raise "ResizeArray.Second: Failed to get second item of %s" xs.ToNiceStringLong
                xs.[1]
            and set (v: 'T) =
                if xs.Count < 2 then ArgumentOutOfRangeException.Raise "ResizeArray.Second: Failed to set second item of %s to %s" xs.ToNiceStringLong (toNiceString v)
                xs.[1] <- v

        /// Get (or set) the third item in the ResizeArray.
        /// Equal to this.[2]
        member inline xs.Third
            with get () =
                if xs.Count < 3 then ArgumentOutOfRangeException.Raise "ResizeArray.Third: Failed to get third item of %s" xs.ToNiceStringLong
                xs.[2]
            and set (v: 'T) =
                if xs.Count < 3 then ArgumentOutOfRangeException.Raise "ResizeArray.Third: Failed to set third item of %s to %s" xs.ToNiceStringLong (toNiceString v)
                xs.[2] <- v

        /// Checks if this.Count = 0
        member inline xs.IsEmpty = xs.Count = 0


        /// Checks if this.Count = 1
        member inline xs.IsSingleton = xs.Count = 1

        /// Checks if this.Count > 0
        /// Same as xs.HasItems
        member inline xs.IsNotEmpty = xs.Count > 0

        /// Checks if this.Count > 0
        /// Same as xs.IsNotEmpty
        member inline xs.HasItems = xs.Count > 0

        /// Insert an item at the beginning of the list = index 0,
        /// (moving all other items up by one index)
        member inline xs.InsertAtStart x = xs.Insert(0, x)


        /// Gets an item in the ResizeArray by index.
        /// Allows for negative index too ( -1 is last item,  like Python)
        /// (From the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        member inline xs.GetNeg index =
            let len = xs.Count
            let ii = if index < 0 then len + index else index
            if ii < 0 || ii >= len then ArgumentOutOfRangeException.Raise "resizeArray.GetNeg: Failed to get (negative) index %d from ResizeArray of %d items: %s" index xs.Count xs.ToNiceStringLong
            xs.[ii]

        /// Sets an item in the ResizeArray by index.
        /// Allows for negative index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        member inline xs.SetNeg index value =
            let len = xs.Count
            let ii = if index < 0 then len + index else index
            if ii < 0 || ii >= len then ArgumentOutOfRangeException.Raise "resizeArray.SetNeg: Failed to set (negative) index %d to %s in %s" index (toNiceString value) xs.ToNiceStringLong
            xs.[ii] <- value

        /// Any index will return a value.
        /// ResizeArray is treated as an endless loop in positive and negative direction
        member inline xs.GetLooped index =
            let len = xs.Count
            if len = 0 then ArgumentOutOfRangeException.Raise "resizeArray.GetLooped: Failed to get (looped) index %d from ResizeArray of 0 items" index
            let t = index % len
            let ii = if t >= 0 then t else t + len
            xs.[ii]

        /// Any index will set a value.
        /// ResizeArray is treated as an endless loop in positive and negative direction
        member inline xs.SetLooped index value =
            let len = xs.Count
            if len = 0 then ArgumentOutOfRangeException.Raise "resizeArray.SetLooped: Failed to set (looped) index %d to %s in ResizeArray of 0 items" index (toNiceString value)
            let t = index % len
            let ii = if t >= 0 then t else t + len
            xs.[ii] <- value

        /// Get and remove last item from ResizeArray
        member inline xs.Pop() =
            if xs.Count = 0 then ArgumentOutOfRangeException.Raise " resizeArray.Pop() failed for %s" xs.ToNiceStringLong
            let i = xs.Count - 1
            let v = xs.[i]
            xs.RemoveAt(i)
            v

        /// Get and remove item at index from ResizeArray
        member inline xs.Pop(index: int) =
            if index < 0 then ArgumentOutOfRangeException.Raise "resizeArray.Pop(%d) failed for ResizeArray of %d items, index must be positive." index xs.Count
            if index >= xs.Count then ArgumentOutOfRangeException.Raise "resizeArray.Pop(%d) failed for ResizeArray of %d items" index xs.Count
            let v = xs.[index]
            xs.RemoveAt(index)
            v

        /// Creates a shallow copy of the list
        /// (for a ResizeArray of structs this is like a deep copy)
        member inline xs.Clone() = 
            xs.GetRange(0, xs.Count) // fastest way to create a shallow copy

        /// <summary>Get the index for the element offset elements away from the end of the collection.
        /// This member exists to support F# indexing from back: ^0 is last item, ^1 is second last</summary>
        /// <param name="rank">The rank of the index. (unused in ResizeArray)</param>
        /// <param name="offset">The offset from the end.</param>
        /// <returns>The corresponding index from the start.</returns>
        member inline xs.GetReverseIndex(rank, offset: int) : int =
            if offset < 0 then ArgumentOutOfRangeException.Raise " resizeArray.[^%d]: index from back is negative for ResizeArray of %d items" offset xs.Count
            if offset >= xs.Count then ArgumentOutOfRangeException.Raise " resizeArray.[^%d]: index from back is equal or bigger than resizeArray.Count %d" offset xs.Count
            xs.Count - offset - 1

        /// This member enables F# slicing notation operator. e.g:  xs.[1..3].
        /// The resulting ResizeArray includes the end index.
        /// Raises an ArgumentOutOfRangeException if indices are out of range.
        /// For indexing from the end use the ^ prefix. e.g. ^0 for the last item.
        member inline xs.GetSlice(startIdx: option<int>, endIdx: option<int>) : ResizeArray<'T> =
            //.GetSlice maps onto slicing operator .[1..3]
            let inline debugTxt (i: int option) =
                match i with
                | None -> " "
                | Some i -> i.ToString()

            let count = xs.Count

            let stIdx =
                match startIdx with
                | None -> 0
                | Some si ->
                    if si < 0 || si >= count then ArgumentOutOfRangeException.Raise "resizeArray.[%s..%s], GetSlice: start index must be between 0 and %d for ResizeArray of %d items." (debugTxt startIdx) (debugTxt endIdx) (count - 1) count
                    si

            let enIdx =
                match endIdx with
                | None -> count - 1
                | Some ei ->
                    if ei < 0 || ei >= count then
                        ArgumentOutOfRangeException.Raise "resizeArray.[%s..%s], GetSlice: end index must be between 0 and %d for ResizeArray of %d items." (debugTxt startIdx) (debugTxt endIdx) (count - 1) count
                    else
                        ei

            // end must be same or bigger than start
            if enIdx >= 0 && stIdx > enIdx then ArgumentOutOfRangeException.Raise "resizeArray.[%s..%s], The given start index must be smaller than or equal to the end index for ResizeArray of %d items." (debugTxt startIdx) (debugTxt endIdx) count

            xs.GetRange(stIdx, enIdx - stIdx + 1)

        /// This member enables F# slicing notation operator e.g.: xs.[1..3] <- ys.
        /// The the end index is included.
        /// For indexing from the end use the ^ prefix. e.g. ^0 for the last item.
        member inline xs.SetSlice(startIdx: option<int>, endIdx: option<int>, newValues: IList<'T>) : unit =
            //.SetSlice maps onto slicing operator .[1..3] <- xs
            let inline debugTxt (i: int option) =
                match i with
                | None -> " "
                | Some i -> i.ToString()

            let count = xs.Count

            let stIdx =
                match startIdx with
                | None -> 0
                | Some si ->
                    if si < 0 || si >= count then ArgumentOutOfRangeException.Raise "resizeArray.[%s..%s], GetSlice: start index must be between 0 and %d for ResizeArray of %d items." (debugTxt startIdx) (debugTxt endIdx) (count - 1) count
                    si

            let enIdx =
                match endIdx with
                | None -> count - 1
                | Some ei ->
                    if ei < 0 || ei >= count then
                        ArgumentOutOfRangeException.Raise "resizeArray.[%s..%s], GetSlice: end index must be between 0 and %d for ResizeArray of %d items." (debugTxt startIdx) (debugTxt endIdx) (count - 1) count
                    else
                        ei

            // end must be same or bigger than start
            if enIdx >= 0 && stIdx > enIdx then ArgumentOutOfRangeException.Raise "resizeArray.[%s..%s], The given start index must be smaller than or equal to the end index for ResizeArray of %d items." (debugTxt startIdx) (debugTxt endIdx) count

            let countToAdd = enIdx - stIdx + 1
            if newValues.Count <> countToAdd then ArgumentOutOfRangeException.Raise "resizeArray.[%s..%s], SetSlice expected %d item in newValues IList but only found %d" (debugTxt startIdx) (debugTxt endIdx) countToAdd newValues.Count

            for i = stIdx to enIdx do
                xs.[i] <- newValues.[i - stIdx]


        /// A property like the ToString() method,
        /// But with richer formatting
        /// Listing includes the first 6 items
        member xs.ToNiceString = toNiceString xs

        /// A property like the ToString() method,
        /// But with richer formatting
        /// Listing includes the first 50 items
        member xs.ToNiceStringLong = toNiceStringLong xs
