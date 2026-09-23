000100 01  MID-W6I10701.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I10701                                
000400     03 MID-ADINLOMR-IN      PIC X(4).                                    
000500*                                 INLEVERANSOMRÅDE                        
000600     03 MID-ADINLOMR-UT      PIC X(4).                                    
000700*                                 INLEVERANSOMRÅDE                        
000800     03 MID-KDINLOMR-IN      PIC X(3).                                    
000900*                                 TYP AV INLEVERANSOMRÅDE                 
001000     03 MID-KDINLOMR-UT      PIC X(3).                                    
001100*                                 TYP AV INLEVERANSOMRÅDE                 
001200     03 MID-ADINLOMR-PAR-IN  PIC X(4).                                    
001300*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
001400     03 MID-ADINLOMR-PAR-UT  PIC X(4).                                    
001500*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
001600     03 MID-KDINLUPF-IN      PIC X(4).                                    
001700*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
001800     03 MID-KDINLUPF-UT      PIC X(4).                                    
001900*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
002000     03 MID-IDDC-IN          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MID-IDDC-UT          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 MID-ADINLOMR-SPAR    PIC X(4).                                    
002500*                                 INLEVERANSOMRÅDE                        
002600     03 MID-KDINLOMR-SPAR    PIC X(3).                                    
002700*                                 TYP AV INLEVERANSOMRÅDE                 
002800     03 MID-ADINLOMR-PAR-SPAR                                             
002900                             PIC X(4).                                    
003000*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
003100     03 MID-KDINLUPF-SPAR    PIC X(4).                                    
003200*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
003300     03 MID-RAD              OCCURS 14 TIMES.                             
003400        05 MID-ADINLOMR      PIC X(4).                                    
003500*                                 INLEVERANSOMRÅDE                        
