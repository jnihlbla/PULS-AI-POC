000100 01  MOD-W3O20301.                                                        
000200*                                 MOD-COPYTEXT FÖR W3020300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFSGURV-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDUSER-IN        PIC X(2).                                    
001000*                                 MFS BEHANDLING AV INPUTFÄLT             
001100     03 MOD-IDFSGURV-UT      PIC X(8).                                    
001200*                                 URVALS IDENTITET                        
001300     03 MOD-IDUSER-UT        PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500     03 MOD-IDARTNR-LO       PIC 9(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-HI       PIC 9(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-DAREGDAT-DOLD    PIC 9(8).                                    
002000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002100     03 MOD-TIREGTID-DOLD    PIC 9(6).                                    
002200*                                 REGISTRERINGSTID                        
002300     03 MOD-TIFSGVV-FOM-ATTR PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-TIFSGVV-FOM      PIC 9(4).                                    
002600*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
002700     03 MOD-IDFSGURV-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-IDFSGURV         PIC X(8).                                    
003000*                                 URVALS IDENTITET                        
003100     03 MOD-KDBORT-ATTR      PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KDBORT           PIC X.                                       
003400*                                 RAD-UPPDATERINGSKOMMANDO                
003500*                                  BLANK  = INGENTING                     
003600*                                  D , B  = DELETE                        
003700*                                  R , Ä  = REPLACE                       
003800*                                  I , N  = INSERT                        
003900     03 MOD-IDPTYP-ATTR      PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDPTYP           PIC X(3).                                    
004200*                                 POSTTYP                                 
004300     03 MOD-TIFSGVV-TOM-ATTR PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-TIFSGVV-TOM      PIC 9(4).                                    
004600*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
004700     03 MOD-KDPRTYPG-ATTR    PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-KDPRTYPG         PIC X.                                       
005000*                                 TYP AV PRISTILLÄMPNINGSGRUPP            
005100     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
005200*                                 RADINFORMATION                          
005300        05 MOD-IDKONCNR-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-IDKONCNR      PIC Z(2)9.                                   
005600*                                 KONCERNNUMMER                           
005700     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
005800*                                 RADINFORMATION                          
005900        05 MOD-KDMARK-FOM-ATTR                                            
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-KDMARK-FOM    PIC Z(3).                                    
006300*                                 MARKNADSKOD                             
006400        05 MOD-KDMARK-TOM-ATTR                                            
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 MOD-KDMARK-TOM    PIC Z(3).                                    
006800*                                 MARKNADSKOD                             
006900     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
007000*                                 RADINFORMATION                          
007100        05 MOD-IDDISTR-FOM-ATTR                                           
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
007500*                                 DISTRIKTNUMMER                          
007600        05 MOD-IDDISTR-TOM-ATTR                                           
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
008000*                                 DISTRIKTNUMMER                          
008100     03 MOD-INFO-RAD         OCCURS 35 TIMES.                             
008200*                                 RADINFORMATION                          
008300        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 MOD-IDARTNR       PIC Z(7)9.                                   
008600*                                 ARTIKELNUMMER                           
008700     03 MOD-KDCMD-ATTR       PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-KDCMD            PIC X.                                       
009000*                                 RAD-UPPDATERINGSKOMMANDO                
009100*                                  BLANK  = INGENTING                     
009200*                                  D , B  = DELETE                        
009300*                                  R , Ä  = REPLACE                       
009400*                                  I , N  = INSERT                        
009500     03 MOD-INFO-RAD         OCCURS 7 TIMES.                              
009600*                                 RADINFORMATION                          
009700        05 MOD-IDARTNR-IN-ATTR                                            
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000        05 MOD-IDARTNR-IN    PIC Z(7)9.                                   
010100*                                 ARTIKELNUMMER                           
010200     03 MOD-TEMFSINF         PIC X(55).                                   
010300*                                 INFORMATIONSMEDDELANDE                  
010400*** END OF VILMAII-COPY LENGTH= 775 BYTES                                 
