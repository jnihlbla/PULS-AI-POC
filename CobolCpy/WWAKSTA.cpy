000010*** EDIT ALLOWED                                                          
000100 01  WWAKSTA.                                                             
000200*                                                                         
000300*    AK-STATUSBENÄMNING                                                   
000400*                                                                         
000500     03  BEAKSTA-TAB.                                                     
000600         05  FILLER         PIC X(50)                                     
000700       VALUE 'R31 RAPPORTERAD                                  '.         
000800         05  FILLER         PIC X(50)                                     
000900       VALUE 'R31 GERAPPORTEERD                                '.         
001000         05  FILLER         PIC X(50)                                     
001100       VALUE 'DELRAPPORTERING TILL C1 PÅBÖRJAD                 '.         
001200         05  FILLER         PIC X(50)                                     
001300       VALUE 'DEELRAPPORTERING NAAR C2 BEGONNEN                '.         
001400         05  FILLER         PIC X(50)                                     
001500       VALUE 'RAPPORTERING TILL C1 AVSLUTAD                    '.         
001600         05  FILLER         PIC X(50)                                     
001700       VALUE 'RAPPORTERING NAAR C2 GEDAAN                      '.         
001800         05  FILLER         PIC X(50)                                     
001900       VALUE 'RAPPORTERING TILL C2 AVSLUTAD                    '.         
002000         05  FILLER         PIC X(50)                                     
002100       VALUE 'RAPPORTERING NAAR C1 GEDAAN                      '.         
002200         05  FILLER         PIC X(50)                                     
002300       VALUE 'DELRAPP. EGET CL PÅBÖRJAD, RAPP. ANDRA CL AVSLUT.'.         
002400         05  FILLER         PIC X(50)                                     
002500       VALUE 'DEELRAPP NAAR C2 BEG. RAPP NAAR C1 GEDAAN        '.         
002600         05  FILLER         PIC X(50)                                     
002700       VALUE 'HELA PARTIET RAPPORTERAT                         '.         
002800         05  FILLER         PIC X(50)                                     
002900       VALUE 'VOLLEDIG GERAPPORTEERD                           '.         
003000         05  FILLER         PIC X(50)                                     
003100       VALUE 'PARTIET DELETEMARKERAT                           '.         
003200         05  FILLER         PIC X(50)                                     
003300       VALUE 'PARTIET DELETEMARKERAT                           '.         
003400     03  WS-BEAKSTA REDEFINES BEAKSTA-TAB.                                
003500        05  FILLER  OCCURS 7.                                             
003600            07  FILLER  OCCURS 2.                                         
003700                09  BEAKSTA     PIC X(50).                                
003800*** END COPY WWAKSTACC0  LENGTH=700   OLD LENGTH=600                      
