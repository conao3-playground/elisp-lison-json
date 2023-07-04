# elisp-lison-json

## Usage

### 000_sample.el

```json
$ cat sample/000_sample.el 
'( :a 1 :b 2 :c 3)

$ cat sample/000_sample.el | emacs --script lisfy-json.el -f batch-lisfy-json
{
  "a": 1,
  "b": 2,
  "c": 3
}
```

### 001_s3.el

```lisp
'( :AWSTemplateFormatVersion "2010-09-09"
   :Transform "AWS::Serverless-2016-10-31"
   :Parameters
   ( :Prefix ( :Type "String")
     :Env ( :Type "String"))
   :Resources
   ( :ManualBucket
     ( :Type "AWS::S3::Bucket"
       :DeletionPolicy "Retain"
       :UpdateReplacePolicy "Retain"
       :Properties
       ( :BucketName ( :Fn::Sub "${Prefix}-manual-${AWS::AccountId}")))
     :ScriptBucket
     ( :Type "AWS::S3::Bucket"
       :DeletionPolicy "Retain"
       :UpdateReplacePolicy "Retain"
       :Properties
       ( :BucketName ( :Fn::Sub "${Prefix}-script-${AWS::AccountId}"))))
   :Outputs
   ( :ManualBucket
     ( :Value ( :Ref "ManualBucket")
       :Export ( :Name ( :Fn::Sub "${Prefix}-ManualBucket")))
     :ScriptBucket
     ( :Value ( :Ref "ScriptBucket")
       :Export ( :Name ( :Fn::Sub "${Prefix}-ScriptBucket")))))
```

```json
$ cat sample/001_s3.el | emacs --script lisfy-json.el -f batch-lisfy-json
{
  "AWSTemplateFormatVersion": "2010-09-09",
  "Transform": "AWS::Serverless-2016-10-31",
  "Parameters": {
    "Prefix": {
      "Type": "String"
    },
    "Env": {
      "Type": "String"
    }
  },
  "Resources": {
    "ManualBucket": {
      "Type": "AWS::S3::Bucket",
      "DeletionPolicy": "Retain",
      "UpdateReplacePolicy": "Retain",
      "Properties": {
        "BucketName": {
          "Fn::Sub": "${Prefix}-manual-${AWS::AccountId}"
        }
      }
    },
    "ScriptBucket": {
      "Type": "AWS::S3::Bucket",
      "DeletionPolicy": "Retain",
      "UpdateReplacePolicy": "Retain",
      "Properties": {
        "BucketName": {
          "Fn::Sub": "${Prefix}-script-${AWS::AccountId}"
        }
      }
    }
  },
  "Outputs": {
    "ManualBucket": {
      "Value": {
        "Ref": "ManualBucket"
      },
      "Export": {
        "Name": {
          "Fn::Sub": "${Prefix}-ManualBucket"
        }
      }
    },
    "ScriptBucket": {
      "Value": {
        "Ref": "ScriptBucket"
      },
      "Export": {
        "Name": {
          "Fn::Sub": "${Prefix}-ScriptBucket"
        }
      }
    }
  }
}
```

### 002_s3_eval.el

lison is evaled, so you can use all Elisp for constructing JSON.

```lisp
(progn
  (defun bucket (name)
    `( :Type "AWS::S3::Bucket"
       :DeletionPolicy "Retain"
       :UpdateReplacePolicy "Retain"
       :Properties
       ( :BucketName ( :Fn::Sub ,(format "${Prefix}-%s-${AWS::AccountId}" name)))))

  (defun list-parameters (&rest names)
    (mapcan (lambda (name)
              `(,(intern (concat ":" name)) (:Type "String")))
            names))
  
  (defun list-outputs (&rest names)
    (mapcan (lambda (name)
              `( ,(intern (concat ":" name))
                 ( :Value ( :Ref ,name)
                   :Export ( :Name ( :Fn::Sub ,(format "${Prefix}-%s" name))))))
            names))
  
  `( :AWSTemplateFormatVersion "2010-09-09"
     :Transform "AWS::Serverless-2016-10-31"
     :Parameters
     ,(list-parameters
       "Prefix"
       "Env")
     :Resources
     ( :ManualBucket ,(bucket "manual")
       :ScriptBucket ,(bucket "script"))
     :Outputs
     ,(list-outputs
       "ManualBucket" "ScriptBucket")))
```

```json
$ cat sample/002_s3_eval.el | emacs --script lisfy-json.el -f batch-lisfy-json
{
  "AWSTemplateFormatVersion": "2010-09-09",
  "Transform": "AWS::Serverless-2016-10-31",
  "Parameters": {
    "Prefix": {
      "Type": "String"
    },
    "Env": {
      "Type": "String"
    }
  },
  "Resources": {
    "ManualBucket": {
      "Type": "AWS::S3::Bucket",
      "DeletionPolicy": "Retain",
      "UpdateReplacePolicy": "Retain",
      "Properties": {
        "BucketName": {
          "Fn::Sub": "${Prefix}-manual-${AWS::AccountId}"
        }
      }
    },
    "ScriptBucket": {
      "Type": "AWS::S3::Bucket",
      "DeletionPolicy": "Retain",
      "UpdateReplacePolicy": "Retain",
      "Properties": {
        "BucketName": {
          "Fn::Sub": "${Prefix}-script-${AWS::AccountId}"
        }
      }
    }
  },
  "Outputs": {
    "ManualBucket": {
      "Value": {
        "Ref": "ManualBucket"
      },
      "Export": {
        "Name": {
          "Fn::Sub": "${Prefix}-ManualBucket"
        }
      }
    },
    "ScriptBucket": {
      "Value": {
        "Ref": "ScriptBucket"
      },
      "Export": {
        "Name": {
          "Fn::Sub": "${Prefix}-ScriptBucket"
        }
      }
    }
  }
}
```
