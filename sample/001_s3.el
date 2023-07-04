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
