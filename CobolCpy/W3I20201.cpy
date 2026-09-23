000100 01  MID-W3I20201.                                                        
000200*                                 MID-COPYTEXT FÖR W3020200               
000300     03 MID-IDFSGURV-IN      PIC X(8).                                    
000400*                                 URVALS IDENTITET                        
000500     03 MID-IDUSER-IN        PIC X(8).                                    
000600*                                 ANVÄNDARENS SÄKERHETS ID                
000700     03 MID-IDFSGURV-UT      PIC X(8).                                    
000800*                                 URVALS IDENTITET                        
000900     03 MID-IDUSER-UT        PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100     03 MID-IDFKNGRP-FOM-LO  PIC 9(4).                                    
001200*                                 FUNKTIONSGRUPP                          
001300     03 MID-IDFKNGRP-TOM-LO  PIC 9(4).                                    
001400*                                 FUNKTIONSGRUPP                          
001500     03 MID-IDFKNGRP-FOM-HI  PIC 9(4).                                    
001600*                                 FUNKTIONSGRUPP                          
001700     03 MID-IDFKNGRP-TOM-HI  PIC 9(4).                                    
001800*                                 FUNKTIONSGRUPP                          
001900     03 MID-DAREGDAT-DOLD    PIC 9(8).                                    
002000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002100     03 MID-TIREGTID-DOLD    PIC 9(6).                                    
002200*                                 REGISTRERINGSTID                        
002300     03 MID-TIFSGVV-FOM      PIC 9(4).                                    
002400*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
002500     03 MID-IDFSGURV         PIC X(8).                                    
002600*                                 URVALS IDENTITET                        
002700     03 MID-KDBORT           PIC X.                                       
002800      88 MID-KDCMD-INGENTING VALUE ' '.                                   
002900      88 MID-KDCMD-DELETE    VALUE 'D'                                    
003000                             'B'.                                         
003100      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
003200                             'Ä'.                                         
003300      88 MID-KDCMD-INSERT    VALUE 'I'                                    
003400                             'N'.                                         
003500      88 MID-KDCMD-SELECT    VALUE 'S'                                    
003600                             'V'.                                         
003700*                                 RAD-UPPDATERINGSKOMMANDO                
003800*                                  BLANK  = INGENTING                     
003900*                                  D , B  = DELETE                        
004000*                                  R , Ä  = REPLACE                       
004100*                                  I , N  = INSERT                        
004200*                                  S , V  = SELECT                        
004300     03 MID-IDPTYP           PIC X(3).                                    
004400*                                 POSTTYP                                 
004500     03 MID-TIFSGVV-TOM      PIC 9(4).                                    
004600*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
004700     03 MID-KDPRTYPG         PIC X.                                       
004800*                                 TYP AV PRISTILLÄMPNINGSGRUPP            
004900     03 MID-KDVVKL           OCCURS 5 TIMES                               
005000                             PIC 9.                                       
005100*                                 VOLYMVÄRDESKLASS                        
005200     03 MID-KDNIVA           PIC 9(2).                                    
005300*                                 NIVÅ NUMMER                             
005400     03 MID-KDPRODSL         OCCURS 7 TIMES                               
005500                             PIC 9(2).                                    
005600*                                 PRODUKTSLAG                             
005700     03 MID-KDSVAR           PIC X.                                       
005800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
005900     03 MID-IDKONCNR         OCCURS 8 TIMES                               
006000                             PIC 9(3).                                    
006100*                                 KONCERNNUMMER                           
006200     03 MID-IDLEVNR          OCCURS 8 TIMES                               
006300                             PIC X(5).                                    
006400*                                 LEVERANTÖRNUMMER                        
006500     03 MID-IDLKTO           OCCURS 8 TIMES                               
006600                             PIC 9(7).                                    
006700*                                 LAGERKONTO (FFHHHUU)                    
006800     03 MID-INFO-RAD         OCCURS 8 TIMES.                              
006900*                                 RADINFORMATION                          
007000        05 MID-KDMARK-FOM    PIC 9(3).                                    
007100*                                 MARKNADSKOD                             
007200        05 MID-KDMARK-TOM    PIC 9(3).                                    
007300*                                 MARKNADSKOD                             
007400     03 MID-INFO-RAD         OCCURS 4 TIMES.                              
007500*                                 RADINFORMATION                          
007600        05 MID-IDDISTR-FOM   PIC 9(4).                                    
007700*                                 DISTRIKTNUMMER                          
007800        05 MID-IDDISTR-TOM   PIC 9(4).                                    
007900*                                 DISTRIKTNUMMER                          
008000     03 MID-INFO-RAD         OCCURS 4 TIMES.                              
008100*                                 RADINFORMATION                          
008200        05 MID-IDANSK-FOM    PIC 9(3).                                    
008300*                                 ANSKAFFARNUMMER                         
008400        05 MID-IDANSK-TOM    PIC 9(3).                                    
008500*                                 ANSKAFFARNUMMER                         
008600     03 MID-INFO-RAD         OCCURS 8 TIMES.                              
008700*                                 RADINFORMATION                          
008800        05 MID-IDFKNGRP-FOM  PIC 9(4).                                    
008900*                                 FUNKTIONSGRUPP                          
009000        05 MID-IDFKNGRP-TOM  PIC 9(4).                                    
009100*                                 FUNKTIONSGRUPP                          
009200     03 MID-KDCMD            PIC X.                                       
009300      88 MID-KDCMD-INGENTING VALUE ' '.                                   
009400      88 MID-KDCMD-DELETE    VALUE 'D'                                    
009500                             'B'.                                         
009600      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
009700                             'Ä'.                                         
009800      88 MID-KDCMD-INSERT    VALUE 'I'                                    
009900                             'N'.                                         
010000      88 MID-KDCMD-SELECT    VALUE 'S'                                    
010100                             'V'.                                         
010200*                                 RAD-UPPDATERINGSKOMMANDO                
010300*                                  BLANK  = INGENTING                     
010400*                                  D , B  = DELETE                        
010500*                                  R , Ä  = REPLACE                       
010600*                                  I , N  = INSERT                        
010700*                                  S , V  = SELECT                        
010800     03 MID-INFO-RAD         OCCURS 4 TIMES.                              
010900*                                 RADINFORMATION                          
011000        05 MID-IDFKNGRP-FOM-IN                                            
011100                             PIC 9(4).                                    
011200*                                 FUNKTIONSGRUPP                          
011300        05 MID-IDFKNGRP-TOM-IN                                            
011400                             PIC 9(4).                                    
011500*                                 FUNKTIONSGRUPP                          
011600*** END OF VILMAII-COPY LENGTH= 426 BYTES                                 
