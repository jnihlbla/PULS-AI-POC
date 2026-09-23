000010*** EDIT ALLOWED                                                          
000100 01  PRISTAB.                                                             
000200*                         PRISKLASSER. ANVÄNDS VID                        
000300*                         BERÄKNING AV PÅFYLLNADSPUNKT,                   
000400*                         PÅFYLLNADSKVANT OCH ÖVERLAGERPUNKT              
000500*                         FÖR SUPPORTLAGER.                               
000600*                         HÄMTAS VIA STANDARDPRIS.                        
000700*                                                                         
000800     03 S1-PRISKLASSER.                                                   
000900        05 FILLER PIC X(9) VALUE '000000100'.                             
001000        05 FILLER PIC X(9) VALUE '000000300'.                             
001100        05 FILLER PIC X(9) VALUE '000001000'.                             
001200        05 FILLER PIC X(9) VALUE '000003000'.                             
001300        05 FILLER PIC X(9) VALUE '000010000'.                             
001400        05 FILLER PIC X(9) VALUE '000030000'.                             
001500        05 FILLER PIC X(9) VALUE '000100000'.                             
001600        05 FILLER PIC X(9) VALUE '000300000'.                             
001700        05 FILLER PIC X(9) VALUE '999999999'.                             
001701                                                                          
001702     03 S2-PRISKLASSER.                                                   
001703        05 FILLER PIC X(9) VALUE '000000100'.                             
001704        05 FILLER PIC X(9) VALUE '000000300'.                             
001705        05 FILLER PIC X(9) VALUE '000001000'.                             
001706        05 FILLER PIC X(9) VALUE '000003000'.                             
001707        05 FILLER PIC X(9) VALUE '000010000'.                             
001708        05 FILLER PIC X(9) VALUE '000030000'.                             
001709        05 FILLER PIC X(9) VALUE '000100000'.                             
001710        05 FILLER PIC X(9) VALUE '000300000'.                             
001711        05 FILLER PIC X(9) VALUE '999999999'.                             
001712                                                                          
001713 01  FILLER REDEFINES PRISTAB.                                            
001720     03  S-PRISTAB       OCCURS 2.                                        
001900        05 S-PKLASS          OCCURS 9.                                    
002000           07 S-PRARTSTD-MAX     PIC 9(7)V9(2).                           
002010*                                                                         
002100*** END COPY PRISTAB     LENGTH=162                                       
