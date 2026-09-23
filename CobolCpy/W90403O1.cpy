000100 01  W90403O1.                                                            
000200*                                 COPYTEXT FÖR MOD W90403O1               
000300     03 TRANS-NUMMER.                                                     
000400*                                 TRANS-NUMMER INGÅR I TRANSKOD           
000500        05 TRANS-SIFF-1      PIC X.                                       
000600        05 TRANS-SIFF-2      PIC X.                                       
000700        05 TRANS-SIFF-3      PIC X.                                       
000800        05 TRANS-SIFF-4      PIC X.                                       
000900     03 MESSAGE              PIC X(41).                                   
001000*                                 MEDDELANDE                              
001100     03 IDARTNR-IN           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 IDARTNR-UT           PIC X(9).                                    
001500*                                 ARTIKELNUMMER                           
001600*                                 PART NUMBER                             
001700     03 FILLER               PIC X.                                       
001800     03 FILLER               PIC 9.                                       
001900     03 BLAEDRING-ANT        PIC X(3).                                    
002000*                                 ANTAL UTLAGDA FÄLT                      
002100     03 AREA-OUTPUT.                                                      
002200*                                                                         
002300        05 FILLER            PIC 9(3).                                    
002400        05 FILLER            PIC 9(3).                                    
002500        05 DIERS-ERS         PIC Z(3)9.9(3).                              
002600*                                 ERSATT ARTIKELANTAL                     
002700*                                 NUMBER OF SUPERSEDED                    
002800        05 FILLER            PIC Z(4)9.                                   
002900        05 FILLER            PIC Z(4)9.                                   
003000        05 FILLER            PIC Z(4)9.                                   
003100        05 FILLER            PIC Z(4)9.                                   
003200        05 STATUS.                                                        
003300           07 FILLER         PIC 9.                                       
003400           07 FILLER         PIC X.                                       
003500           07 FILLER         PIC 9.                                       
003600        05 RADER             OCCURS 10 TIMES.                             
003700*                                                                         
003800           07 KOLUMNER       OCCURS 3 TIMES.                              
003900*                                                                         
004000              09 IDARTNR-TILLK                                            
004100                             PIC Z(8)9.                                   
004200*                                 TILLKOMMANDE ARTIKELNUMMER              
004300*                                 REPLACEMENT PART NO.                    
004400              09 FILLER      PIC X.                                       
004500              09 FILLER      PIC 9.                                       
004600              09 DIERS-TILLK PIC -(4)9.9(3).                              
004700*                                 TILLKOMMANDE ARTIKELANTAL               
004800*                                 NUMBER OF SUPERSEDING                   
004900              09 FILLER      PIC X(5).                                    
005000        05 FILLER            PIC X(40).                                   
005100     03 LINE23.                                                           
005200        05 MESSAGE-BOTTOM    PIC X(79).                                   
005300*                                 MEDDELANDEFÄLT PÅ RAD 23                
005400*                                 MESSAGE FIELD ON LINE 23                
005500*** END OF VILMAII-COPY LENGTH= 974 BYTES                                 
