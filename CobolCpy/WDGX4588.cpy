000010 01  4588-WDGX4588.                                                       
000020*                                 NUMMERSERIE FÖR TULLSYSTEM              
000030*                                 FYSISK NYCKEL: KDSEGKEY                 
000040*                                  (SKALL VARA "1")                       
000050     03 4588-KDSEGKEY        PIC X.                                       
000060*                                 TEKNISK SEGMENT-NYCKEL                  
000070*                                 TECHNICAL SEGMENT KEY                   
000080     03 4588-IDTULLNR-AKT    PIC 9(7).                                    
000090*                                 NUMMERSERIE INGÅENDE I TULLID           
000100*                                                                         
000110*                                 SERIAL NUMBER IN CUSTOMS ID             
000120     03 4588-IDTULLNR-MIN    PIC 9(7).                                    
000130*                                 NUMMERSERIE TULLID MINVÄRDE             
000140*                                                                         
000150*                                 SERIAL NO. CUSTOMS ID MIN VALUE         
000160     03 4588-IDTULLNR-MAX    PIC 9(7).                                    
000170*                                 NUMMERSERIE TULLID MAXVÄRDE             
000180*                                                                         
000190*                                 SERIAL NO. CUSTOMS ID MAX VALUE         
000200     03 FILLER               PIC X(18).                                   
      *** END COPY WDGX4588    LENGTH=40                                        
