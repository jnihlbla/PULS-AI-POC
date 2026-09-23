000010 01  6102-W6GX6102.                                                       
000020*                                 KVALITET PROVPLAN                       
000030*                                 FYSISK NYCKEL W6GXKEY:                  
000040*                                   (IDPROVPL + KDPROVPL)                 
000050     03 6102-IDPROVPL        PIC X.                                       
000060*                                 PROVTAGNINGSPLAN                        
000070*                                 SAMPLE PLAN                             
000080     03 6102-KDPROVPL        PIC X.                                       
000090*                                 PROVPLAN NORMAL/REDUCERAT UTTAG         
000100*                                 SAMPLE PLAN NORMAL/REDUCED              
000110*                                 SELECTION                               
000120     03 6102-KVSKPLOT        PIC S9(3)           COMP-3.                  
000130*                                 SKIPLOT RÄKNARE                         
000140*                                 SKIPLOT COUNTER                         
000150     03 6102-KVAVIS-TAB      OCCURS 5 TIMES                               
000160                             INDEXED 6102-AVIS-IX.                        
000170*                                 KVAVIS-TABELL FÖR                       
000180*                                 SKIPLOT KONTROLL I KVALITET             
000190        05 6102-KVAVIS-FOM   PIC S9(7)           COMP-3.                  
000200*                                 AVISERAT ANTAL FR O M                   
000210*                                 NOTIFIED QTY FROM                       
000220        05 6102-KVAVIS-TOM   PIC S9(7)           COMP-3.                  
000230*                                 AVISERAT ANTAL T O M                    
000240*                                 NOTIFIED QTY TO                         
000250        05 6102-KVPROVPL     PIC S9(7)           COMP-3.                  
000260*                                 KVAL.KONTROLL ANTAL ENL                 
000270*                                 PROVPLAN                                
000280*                                 QUALITY CONTROL PCS SAMPLE PLAN         
000290*** END COPY W6GX6102  LENGTH=64                                          
