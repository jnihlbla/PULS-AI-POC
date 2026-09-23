000100 01  W1117N2A.                                                            
000200*                                 COPYTEXT TILL FIL W1117N                
000300*                                 TILL NEVIS BASELINE                     
000400*                                 POSTTYP S2A OCH S2B                     
000500*                                 ERSÄTTNING (TILLK. ARTIKEL)             
000600*                                                                         
000700     03 IDPTYP               PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 IDARTNR-ERS          PIC 9(9).                                    
001000*                                 ERSATT ARTIKELNUMMER                    
001100     03 IDLOPNR              PIC 9(5).                                    
001200*                                 LÖPNUMMER          IDLOPNR-002          
001300     03 TIAAMMDD             PIC 9(6).                                    
001400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001500     03 IDKORTNR             PIC 9(3).                                    
001600*                                 KORTNUMMER                              
001700*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001800     03 FLTEXT               PIC X.                                       
001900*                                 FINNS TEXTINFORMATION ?                 
002000     03 TYPS2A.                                                           
002100        05 IDARTNR-TILLK     PIC 9(9).                                    
002200*                                 TILLKOMMANDE ARTIKELNUMMER              
002300        05 REKSIFFR-TILLK    PIC 9.                                       
002400*                                 KONTROLLSIFFRA                          
002500        05 DIERS-TILLK       PIC 9(4)V9(3).                               
002600*                                 KVANTITET I ERSÄTTN.                    
002700        05 BEART-SVE-TILLK   PIC X(25).                                   
002800*                                 SVENSK ARTIKELBENÄMNING                 
002900        05 IDBENNR           PIC 9(7).                                    
003000*                                 BENÄMNINGSNUMMER                        
003100     03 TYPS2B-FILLER REDEFINES TYPS2A.                                   
003200        05 TYPS2B.                                                        
003300           07 BEERS          PIC X(20).                                   
003400*                                 ERSÄTTNINGSTEXT                         
003500           07 FILLER         PIC X(27).                                   
003600        05 FILLER            PIC X(2).                                    
003700*** END OF VILMAII-COPY LENGTH= 76 BYTES                                  
