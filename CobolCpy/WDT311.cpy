000100 01  FPCK-WDT311.                                                         
000200*                                 FÖRPACKNINGSHISTORIK                    
000300*                                 HISTORIK DATA                           
000400*                                 FYSISK NYCKEL: WDT3KY11                 
000500*                                 (DAREGDA9 + TIKLOCK9 + IDLAND)          
000600*                                                                         
000700     03 FPCK-DAREGDAT-9KOMPL PIC 9(8).                                    
000800*                                 DATUMETS 9-KOMPLEMENT                   
000900*                                 DATES 9-COMPLEMENT                      
001000     03 FPCK-TIKLOCK-9KOMPL  PIC S9(9)           COMP-3.                  
001100*                                 TID LAGRAT SOM 9-KOMPLEMENT             
001200*                                 TIME SAVED AS 9-COMPLEMENT              
001300     03 FPCK-IDLANDX2        PIC X(2).                                    
001400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001500*                                 2-LETTER CODE FOR COUNTRY               
001600     03 FPCK-BEFT            PIC S9(3)           COMP-3.                  
001700*                                 FÖRPACKNINGSTYP                         
001800*                                 PACKAGING TYPE                          
001900     03 FPCK-KDFORP.                                                      
002000*                                 FÖRPACKNINGSKOD                         
002100*                                 PACKAGING CODE                          
002200        05 FPCK-KDFORPPL     PIC 9.                                       
002300*                                 FÖRPACKNINGSPLATS                       
002400*                                 PREPACKING PLACE                        
002500        05 FPCK-KDFORPGP     PIC 9(2).                                    
002600*                                 FÖRPACKNINGSGRUPP                       
002700*                                 PREPACKING GROUP                        
002800        05 FPCK-KDFORPUF     PIC 9.                                       
002900*                                 UPPRÄKNINGSFAKTOR                       
003000*                                 ENUMERATION                             
003100     03 FPCK-IDUSER          PIC X(8).                                    
003200*                                 ANVÄNDARENS SÄKERHETS ID                
003300*                                 USER SECURITY-IDENTITY                  
003400     03 FPCK-TEBEFT          OCCURS 5 TIMES                               
003500                             PIC X(40).                                   
003600*                                 TEXT FÖRPACKNINGSINSTRUKTION            
003700*** END OF VILMAII-COPY LENGTH= 229 BYTES                                 
