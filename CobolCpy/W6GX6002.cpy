000010 01  6002-W6GX6002.                                                       
000020*                                 KVALITE NUMMEWREGISTER                  
000030*                                 FYSISK NYCKEL: KDSEGKEY 1               
000040     03 6002-KDSEGKEY        PIC X.                                       
000050*                                 TEKNISK SEGMENT-NYCKEL                  
000060*                                 TECHNICAL SEGMENT KEY                   
000070     03 6002-IDKR            OCCURS 2 TIMES                               
000080                             PIC 9(5).                                    
000090*                                 KONTROLLRAPPORT NUMMER                  
000100*                                 INSPECTION REPORT NUMBER                
000110     03 6002-IDVERNR-MIN     OCCURS 4 TIMES                               
000120                             PIC 9(8).                                    
000130*                                 VERIFIKATIONSNUMMER                     
000140*                                 VERIFICATION NUMBER                     
000150     03 6002-IDVERNR-MAX     OCCURS 4 TIMES                               
000160                             PIC 9(8).                                    
000170*                                 VERIFIKATIONSNUMMER                     
000180*                                 VERIFICATION NUMBER                     
000190     03 6002-IDVERNR-AKT     OCCURS 4 TIMES                               
000200                             PIC 9(8).                                    
000210*                                 VERIFIKATIONSNUMMER                     
000220*                                 VERIFICATION NUMBER                     
000230     03 FILLER               PIC X(3).                                    
      *** END COPY W6GX6002    LENGTH=110                                       
