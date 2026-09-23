000100 01  W1117N1A.                                                            
000200*                                 COPYTEXT TILL FIL W1117N                
000300*                                 TILL NEVIS BASELINE                     
000400*                                 POSTTYP S01                             
000500*                                 ERSÄTTNING (ERSATT ARTIKEL)             
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDARTNR-ERS          PIC 9(9).                                    
000900*                                 ERSATT ARTIKELNUMMER                    
001000     03 IDLOPNR              PIC 9(5).                                    
001100*                                 LÖPNUMMER          IDLOPNR-002          
001200     03 TIAAMMDD             PIC 9(6).                                    
001300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001400     03 REKSIFFR-ERS         PIC 9.                                       
001500*                                 KONTROLLSIFFRA                          
001600     03 KDERS-OLD            PIC 9(3).                                    
001700*                                 ERSÄTTNINGSKOD                          
001800     03 KDERS-NEW            PIC 9(3).                                    
001900*                                 ERSÄTTNINGSKOD                          
002000     03 TIERSDAT             PIC 9(5).                                    
002100*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
002200     03 DIERS-ERS            PIC 9(4)V9(3).                               
002300*                                 ERSATT ARTIKELANTAL                     
002400     03 BEART-SVE            PIC X(25).                                   
002500*                                 SVENSK ARTIKELBENÄMNING                 
002600     03 IDBENNR              PIC 9(7).                                    
002700*                                 BENÄMNINGSNUMMER                        
002800*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
