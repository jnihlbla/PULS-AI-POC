000100 01  MID-W3I20301.                                                        
000200*                                 MID-COPYTEXT FÖR W3020300               
000300     03 MID-IDFSGURV-IN      PIC X(8).                                    
000400*                                 URVALS IDENTITET                        
000500     03 MID-IDUSER-IN        PIC X(8).                                    
000600*                                 ANVÄNDARENS SÄKERHETS ID                
000700     03 MID-IDFSGURV-UT      PIC X(8).                                    
000800*                                 URVALS IDENTITET                        
000900     03 MID-IDUSER-UT        PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100     03 MID-IDARTNR-LO       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MID-IDARTNR-HI       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MID-DAREGDAT-DOLD    PIC 9(8).                                    
001600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001700     03 MID-TIREGTID-DOLD    PIC 9(6).                                    
001800*                                 REGISTRERINGSTID                        
001900     03 MID-TIFSGVV-FOM      PIC 9(4).                                    
002000*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
002100     03 MID-IDFSGURV         PIC X(8).                                    
002200*                                 URVALS IDENTITET                        
002300     03 MID-KDBORT           PIC X.                                       
002400      88 MID-KDCMD-INGENTING VALUE ' '.                                   
002500      88 MID-KDCMD-DELETE    VALUE 'D'                                    
002600                             'B'.                                         
002700      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
002800                             'Ä'.                                         
002900      88 MID-KDCMD-INSERT    VALUE 'I'                                    
003000                             'N'.                                         
003100*                                 RAD-UPPDATERINGSKOMMANDO                
003200*                                  BLANK  = INGENTING                     
003300*                                  D , B  = DELETE                        
003400*                                  R , Ä  = REPLACE                       
003500*                                  I , N  = INSERT                        
003600     03 MID-IDPTYP           PIC X(3).                                    
003700*                                 POSTTYP                                 
003800     03 MID-TIFSGVV-TOM      PIC 9(4).                                    
003900*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
004000     03 MID-KDPRTYPG         PIC X.                                       
004100*                                 TYP AV PRISTILLÄMPNINGSGRUPP            
004200     03 MID-IDKONCNR         OCCURS 8 TIMES                               
004300                             PIC 9(3).                                    
004400*                                 KONCERNNUMMER                           
004500     03 MID-INFO-RAD         OCCURS 8 TIMES.                              
004600*                                 RADINFORMATION                          
004700        05 MID-KDMARK-FOM    PIC 9(3).                                    
004800*                                 MARKNADSKOD                             
004900        05 MID-KDMARK-TOM    PIC 9(3).                                    
005000*                                 MARKNADSKOD                             
005100     03 MID-INFO-RAD         OCCURS 4 TIMES.                              
005200*                                 RADINFORMATION                          
005300        05 MID-IDDISTR-FOM   PIC 9(4).                                    
005400*                                 DISTRIKTNUMMER                          
005500        05 MID-IDDISTR-TOM   PIC 9(4).                                    
005600*                                 DISTRIKTNUMMER                          
005700     03 MID-INFO-RAD         OCCURS 35 TIMES.                             
005800*                                 RADINFORMATION                          
005900        05 MID-IDARTNR       PIC 9(8).                                    
006000*                                 ARTIKELNUMMER                           
006100     03 MID-KDCMD            PIC X.                                       
006200      88 MID-KDCMD-INGENTING VALUE ' '.                                   
006300      88 MID-KDCMD-DELETE    VALUE 'D'                                    
006400                             'B'.                                         
006500      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
006600                             'Ä'.                                         
006700      88 MID-KDCMD-INSERT    VALUE 'I'                                    
006800                             'N'.                                         
006900*                                 RAD-UPPDATERINGSKOMMANDO                
007000*                                  BLANK  = INGENTING                     
007100*                                  D , B  = DELETE                        
007200*                                  R , Ä  = REPLACE                       
007300*                                  I , N  = INSERT                        
007400     03 MID-IDARTNR-IN       OCCURS 7 TIMES                               
007500                             PIC 9(8).                                    
007600*                                 ARTIKELNUMMER                           
007700*** END OF VILMAII-COPY LENGTH= 526 BYTES                                 
