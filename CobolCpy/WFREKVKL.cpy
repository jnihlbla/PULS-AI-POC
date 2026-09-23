000010*** EDIT ALLOWED                                                          
000100 01  WFREKVKL.                                                            
000200*                         FREKVENSKLASS. ANVÄNDS VID                      
000300*                         BERÄKNING AV PÅFYLLNADSPUNKT,                   
000400*                         PÅFYLLNADSKVANT OCH ÖVERLAGERPUNKT              
000500*                         FÖR SUPPORTLAGER.                               
000510*                         HÄMTAS VIA PROGNOS.                             
000700*                                                                         
002100     03 S1-FREKVENSKLASSER.                                               
002200        05 FILLER PIC X(7) VALUE '0000001'.                               
002300        05 FILLER PIC X(7) VALUE '0000005'.                               
002400        05 FILLER PIC X(7) VALUE '0000012'.                               
002500        05 FILLER PIC X(7) VALUE '0000041'.                               
002600        05 FILLER PIC X(7) VALUE '0000083'.                               
002700        05 FILLER PIC X(7) VALUE '0000166'.                               
002800        05 FILLER PIC X(7) VALUE '0000250'.                               
002900        05 FILLER PIC X(7) VALUE '9999999'.                               
002910                                                                          
002920     03 S2-FREKVENSKLASSER.                                               
002930        05 FILLER PIC X(7) VALUE '0000000'.                               
002940        05 FILLER PIC X(7) VALUE '0000001'.                               
002950        05 FILLER PIC X(7) VALUE '0000002'.                               
002960        05 FILLER PIC X(7) VALUE '0000008'.                               
002970        05 FILLER PIC X(7) VALUE '0000025'.                               
002980        05 FILLER PIC X(7) VALUE '0000083'.                               
002990        05 FILLER PIC X(7) VALUE '0000250'.                               
002991        05 FILLER PIC X(7) VALUE '9999999'.                               
002992                                                                          
002993     03 S3-FREKVENSKLASSER.                                               
002994        05 FILLER PIC X(7) VALUE '0000000'.                               
002995        05 FILLER PIC X(7) VALUE '0000001'.                               
002996        05 FILLER PIC X(7) VALUE '0000002'.                               
002997        05 FILLER PIC X(7) VALUE '0000008'.                               
002998        05 FILLER PIC X(7) VALUE '0000025'.                               
002999        05 FILLER PIC X(7) VALUE '0000083'.                               
003000        05 FILLER PIC X(7) VALUE '0000250'.                               
003001        05 FILLER PIC X(7) VALUE '9999999'.                               
003002                                                                          
003003 01  FILLER REDEFINES WFREKVKL.                                           
003010     03 S-FREKVTAB        OCCURS 3.                                       
003100        05 S-FKLASS           OCCURS 8.                                   
003200           07 S-KVPB-REF-MAX  PIC 9(6)V9.                                 
007300*                                                                         
007400*** END COPY WFREKVKL    LENGTH=168                                       
