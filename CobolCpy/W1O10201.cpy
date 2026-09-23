000100 01  W1O10201.                                                            
000200*                                 COPYTEXT FÖR MOD W1O10201               
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
001700     03 STRECK               PIC X.                                       
001800     03 REKSIFFR             PIC 9.                                       
001900*                                 KONTROLLSIFFRA                          
002000*                                 PART NO CHECK DIGIT                     
002100     03 BLAEDRING-ANT        PIC X(3).                                    
002200*                                 ANTAL UTLAGDA FÄLT                      
002300     03 AREA-OUTPUT.                                                      
002400*                                                                         
002500        05 KDERS-C1          PIC Z9(2).                                   
002600*                                 ERSÄTTNINGSKOD                          
002700*                                 SUPERSESSION CODE                       
002800        05 KDERS-C2          PIC Z9(2).                                   
002900*                                 ERSÄTTNINGSKOD                          
003000*                                 SUPERSESSION CODE                       
003100        05 DIERS-ERS         PIC Z(3)9.9(3).                              
003200*                                 ERSATT ARTIKELANTAL                     
003300*                                 NUMBER OF SUPERSEDED                    
003400        05 TIERSDAT-REG      PIC 9(5).                                    
003500*                                 REGISTRERINGSDATUM  (ÅÅVVD)             
003600        05 TIERSDAT-PREL-C1  PIC 9(5).                                    
003700*                                 PREL ERSÄTTNINGSDATUM C1  ÅÅVVD         
003800        05 TIERSDAT-PREL-C2  PIC 9(5).                                    
003900*                                 PREL ERSÄTTNINGSDATUM C2  ÅÅVVD         
004000        05 TIERSDAT          PIC 9(5).                                    
004100*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
004200*                                 DATE OF SUPERSESSION (YYWWD)            
004300        05 STATUS.                                                        
004400           07 KDSTATUS-C1    PIC 9.                                       
004500*                                 STATUSKOD C1                            
004600           07 FILLER         PIC X.                                       
004700           07 KDSTATUS-C2    PIC 9.                                       
004800*                                 STATUSKOD C2                            
004900        05 RADER             OCCURS 10 TIMES.                             
005000*                                                                         
005100           07 KOLUMNER       OCCURS 3 TIMES.                              
005200*                                                                         
005300              09 IDARTNR-TILLK                                            
005400                             PIC Z(8)9.                                   
005500*                                 TILLKOMMANDE ARTIKELNUMMER              
005600*                                 REPLACEMENT PART NO.                    
005700              09 STRECK-TILLK                                             
005800                             PIC X.                                       
005900              09 REKSIFFR-TILLK                                           
006000                             PIC 9.                                       
006100*                                 KONTROLLSIFFRA                          
006200*                                 PART NO CHECK DIGIT                     
006300              09 DIERS-TILLK PIC -(4)9.9(3).                              
006400*                                 TILLKOMMANDE ARTIKELANTAL               
006500*                                 NUMBER OF SUPERSEDING                   
006600              09 FILLER      PIC X(5).                                    
006700        05 TEARTNOT          PIC X(40).                                   
006800*                                 ARTIKEL NOTERING                        
006900*                                 PART REMARKS NOTE                       
007000     03 LINE23.                                                           
007100        05 MESSAGE-BOTTOM    PIC X(79).                                   
007200*                                 MEDDELANDEFÄLT PÅ RAD 23                
007300*                                 MESSAGE FIELD ON LINE 23                
007400*** END OF VILMAII-COPY LENGTH= 974 BYTES                                 
