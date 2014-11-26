package com.avsystem.scex.util

import java.{lang => jl, util => ju}

import com.avsystem.scex.presentation.Attributes
import com.avsystem.scex.presentation.SymbolAttributes._
import com.avsystem.scex.util.JavaCollectionExtensions._

/**
 * This is de facto UMP-specific, but since UMP is not a Scala project, I leave it here for now to avoid
 * compiling it dynamically at UMP startup.
 *
 * Author: ghik
 * Created: 11/26/14.
 */
//TODO: It may be good to move this to separate, UMP-specific artifact.
object PredefinedAttributes {
  val basicOperations = attributes {
    on { any: Any =>
      (any == _) --> Attributes(paramNames = List("other"))
      (any != _) --> Attributes(paramNames = List("other"))
      any + (_: String) --> Attributes(documentation =
        "Concatenates string representation of this value with some other string")
      any -> (_: Any) --> Attributes(documentation =
        "Creates a pair out of this value and some other value")
    }

    on { c: Comparable[Any@plus] =>
      c.compareTo _ --> Attributes(paramNames = List("other"), documentation =
        "Compares this value to some other value using natural ordering. Returns negative number when this value is " +
          "less than the other value, positive number when this value is greater than the other value and zero when the " +
          "two values are equal.")
    }

    on { s: String =>
      s + (_: Any) --> Attributes(paramNames = List("other"), documentation =
        "Concatenates this string with string representation of some other value")
      s.charAt _ --> Attributes(paramNames = List("index"), documentation =
        "Returns character at given index in this string. First character has index 0. Fails if index is out of bounds.")
      s.compareTo _ --> Attributes(paramNames = List("anotherString"), documentation =
        "Compares this string lexicographically to some other string. Returns negative value when this string " +
          "lexicographically precedes the other string, positive value when this string lexicographically follows the " +
          "other string and zero when the two strings are equal.")
      s.compareToIgnoreCase _ --> Attributes(paramNames = List("anotherString"), documentation =
        "Compares two strings lexicographically, like <tt>compareTo</tt>, but ignoring case differences")
      s.concat _ --> Attributes(paramNames = List("str"), documentation =
        "Concatenates this string with some other string")
      s.contains _ --> Attributes(paramNames = List("s"), documentation =
        "Checks if this string contains another string as its substring")
      s.endsWith _ --> Attributes(paramNames = List("suffix"), documentation =
        "Checks if this string has some other string at its end")
      s.equalsIgnoreCase _ --> Attributes(paramNames = List("anotherString"), documentation =
        "Compares this string with other string for equality, ignoring case differences")
      s.indexOf(_: String) --> Attributes(paramNames = List("str"), documentation =
        "Returns the index within this string of the first occurrence of the specified substring or -1 when there is none.")
      s.indexOf(_: String, _: Int) --> Attributes(paramNames = List("str", "fromIndex"), documentation =
        "Returns the index within this string of the first occurrence of the specified substring, starting at the specified " +
          "index or -1 when there is none.")
      s.isEmpty --> Attributes(documentation =
        "Checks if this string is empty")
      s.lastIndexOf(_: String) --> Attributes(paramNames = List("str"), documentation =
        "Returns the index within this string of the last occurrence of the specified substring or -1 when there is none.")
      s.lastIndexOf(_: String, _: Int) --> Attributes(paramNames = List("str", "fromIndex"), documentation =
        "Returns the index within this string of the last occurrence of the specified substring, searching backward " +
          "starting at the specified index. Returns -1 when there is no occurrence of specified substring.")
      s.length --> Attributes(documentation =
        "Returns length of this string")
      s.matches _ --> Attributes(paramNames = List("regex"), documentation =
        "Tells whether or not this string matches the given regular expression.")
      s.replace(_: Char, _: Char) --> Attributes(paramNames = List("oldChar", "newChar"), documentation =
        "Replaces all occurences of <tt>oldChar</tt> with <tt>newChar</tt> in this string")
      s.replace(_: CharSequence, _: CharSequence) --> Attributes(paramNames = List("target", "replacement"), documentation =
        "Replaces all occurences of <tt>target</tt> in this string with <tt>replacement</tt>")
      s.replaceAll(_: String, _: String) --> Attributes(paramNames = List("regex", "replacement"), documentation =
        "Replaces all substrings in this string that match given regular expression with given replacement.")
      s.replaceFirst(_: String, _: String) --> Attributes(paramNames = List("regex", "replacement"), documentation =
        "Replaces first substring in this string that matches given regular expression with given replacement.")
      s.split(_: String) --> Attributes(paramNames = List("regex"), documentation =
        "Splits this string around matches of given regular expression.")
      s.split(_: String, _: Int) --> Attributes(paramNames = List("regex", "limit"), documentation =
        "Splits this string around matches of given regular expression, ensuring than no more than given limit " +
          "of parts is returned. Therefore, last part may still contain a substring that matches given regular expression.")
      s.startsWith(_: String) --> Attributes(paramNames = List("prefix"), documentation =
        "Checks if this string starts with specified prefix")
      s.substring(_: Int) --> Attributes(paramNames = List("beginIndex"), documentation =
        "Returns substring of this string that starts at specified index and ends at the end of this string. The index of " +
          "first character is 0.")
      s.substring(_: Int, _: Int) --> Attributes(paramNames = List("beginIndex", "endIndex"), documentation =
        "Returns substring of this string that starts at specified begin index and ends <i>just before</i> specified " +
          "end index. The index of first character is 0.")
      s.toLowerCase --> Attributes(documentation =
        "Converts all characters in this string to their lowercase counterpart.")
      s.toUpperCase --> Attributes(documentation =
        "Converts all characters in this string to their uppercase counterpart.")
      s.trim --> Attributes(documentation =
        "Removes any whitespace characters at the beginning and end of this string.")
      s.capitalize --> Attributes(documentation =
        "Converts first character in this string to its uppercase counterpart.")
      s.nonEmpty --> Attributes(documentation =
        "Checks if this string is non-empty.")
      s.reverse --> Attributes(documentation =
        "Reverses this string.")
      s.stripLineEnd --> Attributes(documentation =
        "Strip trailing line end character from this string if it has one.")
      s.stripPrefix(_: String) --> Attributes(documentation =
        "Strips given prefix from the beginning of this string.")
      s.stripSuffix(_: String) --> Attributes(documentation =
        "Strips given suffix from the end of this string.")
      s.filter(_: Char => Boolean) --> Attributes(documentation =
        "Filters this string by leaving only characters satisfying given predicate.")
      s.take(_: Int) --> Attributes(documentation =
        "Returns prefix of this string with specified length.")
      s.takeRight(_: Int) --> Attributes(documentation =
        "Returns suffix of this string with specified length.")
      s.takeWhile(_: Char => Boolean) --> Attributes(documentation =
        "Returns longest prefix of this string for which every character satisfies given predicate.")
      s.drop(_: Int) --> Attributes(documentation =
        "Returns this string with given number of first characters dropped.")
      s.dropRight(_: Int) --> Attributes(documentation =
        "Returns this string with given number of last characters dropped.")
      s.dropWhile(_: Char => Boolean) --> Attributes(documentation =
        "Returns this string without longest prefix of characters that satisfy given predicate.")
    }

    on { d: ju.Date =>
      d.after _ --> Attributes(paramNames = List("when"), documentation =
        "Checks if this date is strictly after specified other date.")
      d.before _ --> Attributes(paramNames = List("when"), documentation =
        "Checks if this date is strictly before specified other date.")
      d.compareTo _ --> Attributes(paramNames = List("anotherDate"))
    }

  }

  val javaCollectionExtensions = attributes {
    on { c: ju.Collection[Any@plus] =>
      c.size --> Attributes(documentation =
        "Returns number of elements in this collection")
      c.isEmpty --> Attributes(documentation =
        "Checks if this collection is empty")
      c.contains _ --> Attributes(paramNames = List("o"), documentation =
        "Checks if this collection contains given element")
      c.containsAll _ --> Attributes(paramNames = List("c"), documentation =
        "Checks if this collection contains all of the elements of other collection")
      c ++ (_: Nothing) --> Attributes(documentation =
        "Creates new collection with elements from both this and the other collection")
      c.filter(_: Nothing) --> Attributes(documentation =
        "Filters this collection by leaving only elements satisfying given predicate")
      c.map(_: Nothing) --> Attributes(documentation =
        "Transforms this collection by applying given function on every element")
      c.fold(_: Any)(_: Nothing) --> Attributes(documentation =
        "Applies a binary operator to a start value and every element of this collection, in unspecified order. Returns final value.")
      c.containsAny(_: Nothing) --> Attributes(documentation =
        "Checks if this collection contains any of the elements of other collection")
      c.find(_: Nothing) --> Attributes(documentation =
        "Returns any element of this collection satisfying given predicate or fails when no such element exists. " +
          "Note that in case of failure, you can provide fallback value with <tt>?</tt> operator, e.g. <tt>list(1,2,3).find(_ < 0) ? 0</tt>")
      c.count(_: Nothing) --> Attributes(documentation =
        "Returns number of elements in this collection that satisfy given predicate.")
      c.exists(_: Nothing) --> Attributes(documentation =
        "Checks if this collection contains any element satisfying given predicate. For empty collection, always returns <tt>false</tt>.")
      c.forall(_: Nothing) --> Attributes(documentation =
        "Checks if every element of this collection satisfies given predicate. For empty collection, always returns <tt>true</tt>")
      c.min(_: Nothing) --> Attributes(documentation =
        "Finds the smallest element of this collection. Fails for empty collection. In case of failure, you can provide fallback value " +
          "with <tt>?</tt> operator, e.g. <tt>emptyColl.min ? 0</tt>.")
      c.minBy(_: Nothing)(_: Nothing) --> Attributes(documentation =
        "Finds element for which given function returns smallest value. Fails for empty collection. In case of failure, you can " +
          "provide fallback value with <tt>?</tt> operator, e.g. <tt>emptyColl.minBy(_.toInt) ? 0</tt>.")
      c.max(_: Nothing) --> Attributes(documentation =
        "Finds the largest element of this collection. Fails for empty collection. In case of failure, you can provide fallback value " +
          "with <tt>?</tt> operator, e.g. <tt>emptyColl.max ? 0</tt>.")
      c.maxBy(_: Nothing)(_: Nothing) --> Attributes(documentation =
        "Finds element for which given function returns largest value. Fails for empty collection. In case of failure, you can " +
          "provide fallback value with <tt>?</tt> operator, e.g. <tt>emptyColl.maxBy(_.toInt) ? 0</tt>.")
      c.toList --> Attributes(documentation =
        "Converts this collection to a list. Order of elements is unspecified.")
      c.toSet --> Attributes(documentation =
        "Converts this collection to a set. This will remove all duplicate elements.")
      c.nonEmpty --> Attributes(documentation =
        "Checks if this collection is non-empty")
      c.anyElement --> Attributes(documentation =
        "Returns arbitrary element of this collection. This may be handy when the collection is expected to contain only one element. " +
          "Fails for empty collection. In case of failure, you can provide fallback value with <tt>?</tt> operator, e.g. " +
          "<tt>emptyColl.anyElement ? 0</tt>")
      c.sum(_: Nothing) --> Attributes(documentation =
        "Computes a sum of elements of this collection. Returns zero for empty collection.")
    }

    on { c: ju.Collection[String] =>
      c.join(_: String) --> Attributes(documentation =
        "Concatenates all elements of this collection using given separator. Order of elements is unspecified. " +
          "Returns empty string for empty collection.")
      c.join --> Attributes(documentation =
        "Concatenates all elements of this collection. Order of elements is unspecified. " +
          "Returns empty string for empty collection.")
    }

    on { l: ju.List[Any@plus] =>
      l.get _ --> Attributes(paramNames = List("index"), documentation =
        "Returns element at given index in this list. First element has index 0. Fails if index is out of range." +
          "In case of failure, you can provide fallback value with <tt>?</tt> operator, e.g. <tt>emptyList.get(1) ? 0</tt>")
      l ++ (_: Nothing) --> Attributes(documentation =
        "Concatenates this list with another list")
      l.filter(_: Nothing) --> Attributes(documentation =
        "Filters this list by leaving only elements satisfying given predicate")
      l.map(_: Nothing) --> Attributes(documentation =
        "Transforms this list by applying given function on every element")
      l.apply(_: Int) --> Attributes(documentation =
        "Returns element at given index in this list. First element has index 0. Fails if index is out of range. In case of failure, " +
          "you can provide fallback value with <tt>?</tt> operator, e.g. <tt>emptyList.apply(1) ? 0</tt>. " +
          "This method can be invoked using shorter syntax, e.g. <tt>someList(1)</tt> instead of <tt>someList.apply(1)</tt>")
      l.foldLeft(_: Any)(_: Nothing) --> Attributes(documentation =
        "Applies a binary operator to a start value and every element of this list, going left to right. Returns final value.")
      l.foldRight(_: Any)(_: Nothing) --> Attributes(documentation =
        "Applies a binary operator to a start value and every element of this list, going right to left. Returns final value.")
      l.drop(_: Int) --> Attributes(documentation =
        "Drops specified number of elements from beginning of this list. Returns empty list if this list has less elements " +
          "than the specified number")
      l.dropRight(_: Int) --> Attributes(documentation =
        "Drops specified number of elements from end of this list. Returns empty list if this list has less elements that " +
          "the specified number.")
      l.dropWhile(_: Nothing) --> Attributes(documentation =
        "Drops longest prefix of elements of this list which satisfy given predicate")
      l.take(_: Int) --> Attributes(documentation =
        "Returns prefix of this list with specified length. Returns the same list if it's too short.")
      l.takeRight(_: Int) --> Attributes(documentation =
        "Returns suffix of this list with specified length. Returns the same list if it's too short.")
      l.takeWhile(_: Nothing) --> Attributes(documentation =
        "Returns longest prefix of elements of this list which satisfy given predicate")
      l.slice(_: Int, _: Int) --> Attributes(documentation =
        "Returns sublist of this list with elements at indices starting at <tt>from</tt> and ending at <tt>until-1</tt>. " +
          "The <tt>from</tt> argument will be adjusted to 0 if it's negative and <tt>until</tt> will be adjusted to " +
          "<tt><i>size</i>-1</tt> if it's larger than the size of this list.")
      l.sorted(_: Nothing) --> Attributes(documentation =
        "Returns this list sorted using natural ordering")
      l.sortBy(_: Nothing)(_: Nothing) --> Attributes(documentation =
        "Returns this list sorted using natural ordering on values returned by given function")
      l.reverse --> Attributes(documentation =
        "Returns this list reversed")
    }

    on { l: ju.List[String] =>
      l.join(_: String) --> Attributes(documentation =
        "Concatenates all elements of this list using given separator. Returns empty string for empty list.")
      l.join --> Attributes(documentation =
        "Concatenates all elements of this list. Returns empty string for empty list.")
    }

    on { s: ju.Set[Any@plus] =>
      s ++ (_: Nothing) --> Attributes(documentation =
        "Returns an union of this set and another set")
      s.filter(_: Nothing) --> Attributes(documentation =
        "Filters this set by leaving only elements satisfying given predicate")
      s.union(_: Nothing) --> Attributes(documentation =
        "Returns an union of this set and another set")
      s.intersect(_: Nothing) --> Attributes(documentation =
        "Returns an intersection of this set and another set")
      s.diff(_: Nothing) --> Attributes(documentation =
        "Returns a difference of this set and another set, i.e. a set with all elements of this set which are not in the " +
          "other set.")
    }

    on { m: ju.Map[Any@plus, Any@plus] =>
      m.isEmpty --> Attributes(documentation =
        "Checks if this map is empty")
      m.size --> Attributes(documentation =
        "Returns the number of key-value bindings in this map")
      m.get _ --> Attributes(paramNames = List("key"), documentation =
        "Returns the value to which specified key is mapped or <tt>null</tt> if there is no mapping for given key. " +
          "In case of no mapping, you can provide fallback value with <tt>?</tt> operator, e.g. <tt>someMap.get('key') ? 0</tt>")
      m.keySet --> Attributes(documentation =
        "Returns a set of all keys in this map")
      m.values --> Attributes(documentation =
        "Returns a collection of all values in this map")
      m.containsKey _ --> Attributes(paramNames = List("key"), documentation =
        "Checks if this map contains mapping for specified key")
      m.containsValue _ --> Attributes(paramNames = List("value"), documentation =
        "Checks if this map contains mapping with specified value")
      m ++ (_: Nothing) --> Attributes(documentation =
        "Merges this map with another map. If the two maps contain mappings with the same key, mappings from the other map will " +
          "override mappings from this map.")
      m.apply(_: Any) --> Attributes(documentation =
        "Returns the value to which specified key is mapped or <tt>null</tt> if there is no mapping for given key. " +
          "In case of no mapping, you can provide fallback value with <tt>?</tt> operator, e.g. <tt>someMap.apply('key') ? 0</tt>. " +
          "This method can be used with shorter syntax, e.g. <tt>someMap('key')</tt> instead of <tt>someMap.apply('key')</tt>")
      m.entries --> Attributes(documentation =
        "Returns a collection of key-value mappings in this map")
      m.filter(_: Nothing) --> Attributes(documentation =
        "Filters this map by leaving only mappings satisfying given predicate")
      m.map(_: Nothing) --> Attributes(documentation =
        "Transforms this map by applying given function to every mapping. If some mappings have the same key after transformation, " +
          "an arbitrary one is used and all other are discarded.")
      m.nonEmpty --> Attributes(documentation =
        "Checks if this map contains any mappings")
    }

    on { c: ju.Collection[Entry[Any@plus, Any@plus]] =>
      c.toMap --> Attributes(documentation =
        "Creates a map with mappings from this collection. If some mappings in this collection have the same key, an arbitrary " +
          "one is used and all other are discarded.")
    }

    list _ --> Attributes(documentation = "Creates a list from given elements, e.g. <tt>list(1, 2, 3)</tt>")
    set _ --> Attributes(documentation = "Creates a set from given elements, e.g. <tt>set(1, 2, 3)</tt>")
    map _ --> Attributes(documentation = "Creates a map from given key-value pairs, e.g. <tt>map(1 -> 'one', 2 -> 'two')</tt>")

  }

}
