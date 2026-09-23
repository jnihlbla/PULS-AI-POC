000010*** EDIT ALLOWED                                                          
000100 01  WPRISKL.                                                             
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
001713     03 S3-PRISKLASSER.                                                   
001714        05 FILLER PIC X(9) VALUE '000000100'.                             
001715        05 FILLER PIC X(9) VALUE '000000300'.                             
001716        05 FILLER PIC X(9) VALUE '000001000'.                             
001717        05 FILLER PIC X(9) VALUE '000003000'.                             
001718        05 FILLER PIC X(9) VALUE '000010000'.                             
001719        05 FILLER PIC X(9) VALUE '000030000'.                             
001720        05 FILLER PIC X(9) VALUE '000100000'.                             
001721        05 FILLER PIC X(9) VALUE '000300000'.                             
001722        05 FILLER PIC X(9) VALUE '999999999'.                             
001723                                                                          
001724 01  FILLER REDEFINES WPRISKL.                                            
001730     03  S-PRISTAB       OCCURS 3.                                        
001900        05 S-PKLASS          OCCURS 9.                                    
002000           07 S-PRARTSTD-MAX     PIC 9(7)V9(2).                           
002010*                                                                         
002100*** END COPY WPRISKL     LENGTH=243                                       
