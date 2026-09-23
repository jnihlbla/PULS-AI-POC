000100 01  MID2-W2I10902.                                                       
000200*                                 MID FR≈N ANDRA PROGRAM                  
000300     03 MID2-KVANTART        PIC 9(5).                                    
000400*                                 ANTAL-ARTIKLAR                          
000500     03 MID2-INAREA          OCCURS 18 TIMES.                             
000600*                                 ORDERING≈NG  INPUT                      
000700        05 MID2-IDARTNR      PIC 9(8).                                    
000800*                                 ARTIKELNUMMER                           
000900        05 MID2-IDDC         PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 MID2-KDTECKEN     PIC X.                                       
001200*                                 PLUS ELLER MINUS (+ -)                  
001300        05 MID2-KDOI         PIC X(2).                                    
001400*                                 ORDERING≈NGSTYP                         
001500        05 MID2-CLEARGROUP.                                               
001600*                                 CLEARINGAREA F÷R ORDERING≈NG            
001700           07 MID2-CLEARAREA OCCURS 7 TIMES.                              
001800*                                 CLEARINGAREA F÷R ORDERING≈NG            
001900              09 MID2-IDDC-CLEAR                                          
002000                             PIC X(2).                                    
002100*                                 LAGERPRIORITERING VID                   
002200*                                 ORDERCLEARING                           
002300              09 MID2-FLLF   PIC X.                                       
002400*                                 ARTIKEL LAGERF÷RES                      
002500              09 MID2-FLCLEAR                                             
002600                             PIC X.                                       
002700*                                 ORDERRAD CLEAR FLAGGA                   
002800        05 MID2-KVOI         PIC 9(7).                                    
002900*                                 ORDERING≈NG I STYCK PER TIDSENH         
003000        05 MID2-TIUPPDAT     PIC 9(6).                                    
003100*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
003200*** END OF VILMAII-COPY LENGTH= 977 BYTES                                 
