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
