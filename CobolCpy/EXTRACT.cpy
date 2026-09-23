      ******************************************************************        
      * COBOL DECLARATION FOR TABLE V0METOD.EXTRACT                    *        
      ******************************************************************        
       01  EXTRACT.                                                             
           10 EXTRACTID            PIC X(8).                                    
           10 TYPEX                PIC X(8).                                    
           10 DESCRIPTION          PIC X(25).                                   
           10 COPYTEXT             PIC X(8).                                    
           10 SIZEX                PIC S99999V USAGE COMP-3.                    
           10 VALID-NO-DAYS        PIC S999V USAGE COMP-3.                      
           10 EXTRACT-FILE         PIC X(42).                                   
           10 EXTRACT-JOB          PIC X(52).                                   
           10 JOB-TYPE             PIC X(1).                                    
           10 OWNER                PIC X(7).                                    
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *        
      ******************************************************************        
