000010*** EDIT ALLOWED                                                          
000100*                        *************************************            
000200*                        *** COPYTEXT FÖR UTSKRIFT AV FIL TILL            
000300*                        *** MEMO-API                                     
000400*                        ***                                              
000500*                        *** RADERNA ÄR ORDNADE EFTER HUR DE              
000600*                        *** SKA KOMMA PÅ FILEN                           
000700*                        *************************************            
000800 01  MEMO-RADER.                                                          
000900*                                                                         
001000     03  FOERSTA-RAD          PIC X(5)  VALUE ')SEND'.                    
001100*                                                   ***** REQUIRED        
001200     03  ARRIVAL-DATE.                                                    
001300         05  ABSOLUTE-ARRIVAL-DATE.                                       
001400             07 ARRIVAL            PIC X(8)  VALUE 'ARRIVAL '.            
001500             07 ABS-DATE.                                                 
001600                 09 DATE-AA        PIC X(2).                              
001700                 09 FILLER         PIC X     VALUE '-'.                   
001800                 09 DATE-MM        PIC X(2).                              
001900                 09 FILLER         PIC X     VALUE '-'.                   
002000                 09 DATE-DD        PIC X(2).                              
002100         05  RELATIVE-ARRIVAL-DATE.                                       
002200             07 ARRIVAL            PIC X(8)  VALUE 'ARRIVAL '.            
002300             07 REL-DATE.                                                 
002400                 09 FILLER         PIC X     VALUE '+'.                   
002500                 09 ANTAL-DAG-FRAM PIC 9(3).                              
002600                 09 FILLER         PIC X     VALUE '/'.                   
002700*                                                   ***** OPTIONAL        
002800     03  TITEL-RAD.                                                       
002900         05  FILLER           PIC X(6)  VALUE 'TITLE '.                   
003000         05  TITEL            PIC X(74).                                  
003100*                                                   ***** REQUIRED        
003200     03  DEST-RAD.                                                        
003300         05  FILLER           PIC X(5)  VALUE 'DEST '.                    
003400         05  MEMOID           PIC X(75).                                  
003500*                                                   ***** REQUIRED        
003600     03  MEMO-RAD             PIC X(4)  VALUE 'MEMO'.                     
003700*                                                   ***** REQUIRED        
003800     03  MEMO-TEXT            PIC X(80).                                  
003900*                                                   ***** OPTIONAL        
004000*                             HÄR LÄGGS SJÄLVA MEMOTEXTEN                 
004100     03  SISTA-RAD            PIC X(4)  VALUE ')END'.                     
004200*                                                   ***** REQUIRED        
