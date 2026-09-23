000100 01  MOD-W6O10701.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W6O10701                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-ADINLOMR-IN      PIC X(4).                                    
000900*                                 INLEVERANSOMRÅDE                        
001000     03 MOD-ADINLOMR-UT      PIC X(4).                                    
001100*                                 INLEVERANSOMRÅDE                        
001200     03 MOD-KDINLOMR-IN      PIC X(3).                                    
001300*                                 TYP AV INLEVERANSOMRÅDE                 
001400     03 MOD-KDINLOMR-UT      PIC X(3).                                    
001500*                                 TYP AV INLEVERANSOMRÅDE                 
001600     03 MOD-ADINLOMR-PAR-IN  PIC X(4).                                    
001700*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
001800     03 MOD-ADINLOMR-PAR-UT  PIC X(4).                                    
001900*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
002000     03 MOD-KDINLUPF-IN      PIC X(4).                                    
002100*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
002200     03 MOD-KDINLUPF-UT      PIC X(4).                                    
002300*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
002400     03 MOD-IDDC-IN          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600     03 MOD-IDDC-UT          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 MOD-ADINLOMR-SPAR    PIC X(4).                                    
002900*                                 INLEVERANSOMRÅDE                        
003000     03 MOD-KDINLOMR-SPAR    PIC X(3).                                    
003100*                                 TYP AV INLEVERANSOMRÅDE                 
003200     03 MOD-ADINLOMR-PAR-SPAR                                             
003300                             PIC X(4).                                    
003400*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
003500     03 MOD-KDINLUPF-SPAR    PIC X(4).                                    
003600*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
003700     03 MOD-INPUT            OCCURS 14 TIMES.                             
003800*                                 INDATA FÖR UPPDATERING                  
003900        05 MOD-ADINLOMR      PIC X(4).                                    
004000*                                 INLEVERANSOMRÅDE                        
004100        05 MOD-KDINLOMR      PIC X(3).                                    
004200*                                 TYP AV INLEVERANSOMRÅDE                 
004300        05 MOD-ADINLOMR-PAR  PIC X(4).                                    
004400*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
004500        05 MOD-KDINLUPF      PIC X(4).                                    
004600*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
004700        05 MOD-ADINLOMR-BO   PIC X(4).                                    
004800*                                 BUFFERTOMRÅDE                           
004900        05 MOD-ADINLOMR-LPL  PIC X(4).                                    
005000*                                 LOSSNINGSPLATS                          
005100        05 MOD-ADPLATS-FOM   PIC 9(5).                                    
005200*                                 LAGERPLATS FRÅN OCH MED                 
005300        05 MOD-ADPLATS-TOM   PIC 9(5).                                    
005400*                                 LAGERPLATS TILL OCH MED                 
005500        05 MOD-ADGANG-FOM    PIC 9(2).                                    
005600*                                 GÅNG FRÅN OCH MED                       
005700        05 MOD-ADGANG-TOM    PIC 9(2).                                    
005800*                                 GÅNG TILL OCH MED                       
005900     03 MOD-TEMFSINF         PIC X(55).                                   
006000*                                 INFORMATIONSMEDDELANDE                  
