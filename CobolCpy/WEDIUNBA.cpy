000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI UNB INTERCHANGE HEADER                                           
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000050*                                                                         
000060*    FUNCTION,                                                            
000070*    -TO START, IDENTIFY AND SPECIFY AN INTERCHANGE                       
000080*                                                                         
000100 01  WEDIUNB.                                                             
000230     03 UNB-IDPTYP                             PIC X(03).                 
000240*                                              UNB                        
000501     03 UNB-LENGTH                             PIC 9(03).                 
000504*                                              LENGTH = 125               
000505*                                                                         
000522     03 UNB-S001-SYNTAX-IDENTIFIER.                                       
000523*                                                                         
000524        05 UNB-0001-SYNTAX-ID                  PIC X(04).                 
000525*                                                                         
000526        05 UNB-0002-SYNTAX-VERSION-NO          PIC 9(01).                 
000527*                                                                         
000528     03 UNB-S002-INTERCHANGE-SENDER.                                      
000529*                                                                         
000530        05 UNB-0004-SENDER-ID                  PIC X(14).                 
000533*                                                                         
000536     03 UNB-S003-INTERCHANGE-RECIP.                                       
000537*                                                                         
000538        05 UNB-0010-RECIPIENT-ID               PIC X(14).                 
000541*                                                                         
000544     03 UNB-S004-DATE-TIME-PREP.                                          
000545*                                                                         
000546        05 UNB-0017-DATE                       PIC X(06).                 
000547*                                                                         
000548        05 UNB-0019-TIME                       PIC 9(04).                 
000549*                                                                         
000550     03 UNB-0020-INTERC-CONTROL-REF            PIC X(14).                 
000551*                                                                         
000552     03 UNB-S005-REC-REF-PASSW.                                           
000553*                                                                         
000554        05 UNB-0020-RECIPIENT-REF              PIC X(14).                 
000555                                                                          
000556        05 UNB-0025-RECIPIENT-REF-QUAL         PIC X(02).                 
000557*                                                                         
000558     03 UNB-0026-APPL-REF                      PIC X(14).                 
000559*                                                                         
000560     03 UNB-0029-PROC-PRIORITY-CODE            PIC X(01).                 
000561*                                                                         
000562     03 UNB-0031-ACKNOLEDGE-REQ                PIC 9(01).                 
000563*                                                                         
000564     03 UNB-0032-COMMUNIC-AGREE-ID             PIC X(35).                 
000565*                                                                         
000566     03 UNB-0035-TEST-INDICTOR                 PIC 9(01).                 
000567*                                                                         
000570*** END OF VILMAII-COPY LENGTH=131                                        
