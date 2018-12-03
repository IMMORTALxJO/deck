#!/usr/bin/env python3
import unittest
import sys
sys.path.append('../')
from templinator import merge_states as merge

class Merge(unittest.TestCase):

  def test_dict(self):
    old = {
      "a": 1
    }
    changes = {
      "b": 2 
    }
    result = {
      "a": 1,
      "b": 2
    }
    self.assertEqual(merge(old, changes), result)

  def test_dict_nested(self):
    old = {
      "a": {
        "b": 1
      }
    }
    changes = {
      "a": {
        "c": 2
      } 
    }
    result = {
      "a": {
        "b": 1,
        "c": 2
      }
    }
    self.assertEqual(merge(old, changes), result)

  def test_dict_rewrite(self):
    old = {
      "a": {
        "b": 1
      }
    }
    changes = {
      "a": {
        "b": 2
      } 
    }
    result = {
      "a": {
        "b": 2
      }
    }
    self.assertEqual(merge(old, changes), result)

  def test_dict_empty_before(self):
    old = {}
    changes = {
      "a": {
        "b": 2
      },
      "c": "String"
    }
    result = changes
    self.assertEqual(merge(old, changes), result)

  def test_dict_empty_changes(self):
    old = {
      "a": {
        "b": 2
      },
      "c": "String"
    }
    changes = {}
    result = old
    self.assertEqual(merge(old, changes), result)

  def test_dict_nested_in_nested(self):
    old = {
      "a": {
        "b": {
          "c": 1
        }
      }
    }
    changes = {
      "a": {
        "b": {
          "d": 2
        }
      }
    }
    result = {
      "a": {
        "b": {
          "c": 1,
          "d": 2
        }
      }
    }
    self.assertEqual(merge(old, changes), result)

  def test_dict_add_new_key(self):
    old = {
      "a": {
        "b": {
          "c": 1
        }
      }
    }
    changes = {
      "b": {
        "c": {
          "d": 2
        }
      }
    }
    result = {
      "a": {
        "b": {
          "c": 1
        }
      },
      "b": {
        "c": {
          "d": 2
        }
      }
    }
    self.assertEqual(merge(old, changes), result)

  def test_dict_type_change(self):
    old = {
      "a": {
        "b": {
          "c": 1
        }
      }
    }
    changes = 'String'
    result = changes
    self.assertEqual(merge(old, changes), result)

  def test_array_same_len(self):
    old = [1,2,3]
    changes = [1,3,2]
    result = changes
    self.assertEqual(merge(old, changes), result)

  def test_array_low_len(self):
    old = [1,2,3]
    changes = [1,3]
    result = [1,3,3]
    self.assertEqual(merge(old, changes), result)

  def test_array_long_len(self):
    old = [1,2,3]
    changes = [1,3,2,4]
    result = [1,3,2,4]
    self.assertEqual(merge(old, changes), result)

  def test_array_nested(self):
    old = [1,2,[1,2,3],4]
    changes = [1,2,[3,2,1]]
    result = [1,2,[3,2,1],4]
    self.assertEqual(merge(old, changes), result)

  def test_dict_with_nested_array(self):
    old = {"a":{"b":[3,2]}}
    changes = {"a":{"b":[1,2,3]}}
    result = changes
    self.assertEqual(merge(old, changes), result)

  def test_array_with_nested_dict(self):
    old = [{"a":1},{"b":2}]
    changes = [{"c":3}]
    result = [{"a":1,"c":3},{"b":2}]
    self.assertEqual(merge(old, changes), result)

  def test_deep_exception(self):
    old = [[[[[[[[[[[[[[[[[[[[[[ 1 ]]]]]]]]]]]]]]]]]]]]]]
    changes = [[[[[[[[[[[[[[[[[[[[[[ 2 ]]]]]]]]]]]]]]]]]]]]]]
    with self.assertRaises(Exception):
      merge(old, changes, maxdeep=4)

if __name__ == '__main__':
  unittest.main(verbosity=2)
