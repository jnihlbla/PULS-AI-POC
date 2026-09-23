000100 01  W4O10601.                                                            
000200*                                 COPYTEXT FÖR MOD W4O10601               
000300*                                                                         
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MESSAGE-RAD1         PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 IDARTNR-IN           PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 IDARTNR-UT           PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 STRECK               PIC X.                                       
001300     03 REKSIFFR             PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 IDDC-IN              PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 IDDC-UT              PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 AREA.                                                             
002000*                                 AREA SOM NOLLSTÄLLS MELLAN              
002100*                                 VARVEN                                  
002200        05 ADARTADR          OCCURS 2 TIMES.                              
002300*                                 ADRESS I LAGER                          
002400           07 ADLAGOMR       PIC Z9B.                                     
002500*                                 LAGEROMRÅDE                             
002600           07 ADGANG         PIC Z9B.                                     
002700*                                 GÅNG                                    
002800           07 ADPLATS        PIC Z(4)9.                                   
002900*                                 LAGERPLATSNUMMER                        
003000        05 TISKROT           PIC Z(6)9.                                   
003100*                                 SKROTNINGSDATUM                         
003200        05 KVMP              PIC Z(7)9.                                   
003300*                                 MAXPUNKT               KVMP-002         
003400        05 KVSKROT           PIC Z(7)9.                                   
003500*                                 SKROTAT ANTAL       KVSKROT-002         
003600        05 KVPB-SEP          PIC Z(5)9.9.                                 
003700*                                 SEPARAT PERIODBEHOV                     
003800        05 KVPB-SATS         PIC Z(5)9.9.                                 
003900*                                 SATS-PERIODBEHOV                        
004000        05 IDAVINR-SEN       PIC Z(6)9.                                   
004100*                                 AVINUMMER SENASTE INLEVERANS            
004200        05 KVLS              OCCURS 2 TIMES                               
004300                             PIC -(7)9.                                   
004400*                                 LAGERSALDO                              
004500        05 KVAVIS-SEN        PIC Z(7)9.                                   
004600*                                                  KVAVIS-SEN-003         
004700*                                 SENAST AVISERAD KVANTITET               
004800        05 KVRESS            PIC -(7)9.                                   
004900*                                 RESERVERAT ANTAL ARTIKLAR               
005000        05 KVSLAGER          PIC -(7)9.                                   
005100*                                 SÄKERHETSLAGER                          
005200        05 KVAKS-T           PIC -(7)9.                                   
005300*                                 DEL AV AK I EN TERMINAL                 
005400        05 KVAKS-DC          OCCURS 2 TIMES                               
005500                             PIC -(7)9.                                   
005600*                                 DEL AV AK SOM LIGGER I CDC              
005700        05 KVAKS-PAV         OCCURS 2 TIMES                               
005800                             PIC -(7)9.                                   
005900*                                 DEL AV AK PÅ VÄG                        
006000        05 KVEFRS            OCCURS 2 TIMES                               
006100                             PIC -(7)9.                                   
006200*                                 EJ FAKTURERAT ANTAL STYCK               
006300        05 KVUTRS            OCCURS 2 TIMES                               
006400                             PIC -(7)9.                                   
006500*                                 UTREDNINGSSALDO                         
006600        05 KVROS             PIC -(7)9.                                   
006700*                                 RESTORDERSALDO                          
006800        05 SUTPO-TOT         PIC -(6)9.                                   
006900*                                 TPO-KVANTITET, TOTAL                    
007000        05 KVBR              PIC -(7)9.                                   
007100*                                 BESTÄLLNINGSREST                        
007200        05 MESSAGE-RAD23     PIC X(79).                                   
007300*                                 MEDDELANDEFÄLT PÅ RAD 23                
007400*** END OF VILMAII-COPY LENGTH= 350 BYTES                                 
