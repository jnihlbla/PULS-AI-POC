000100 01  MID-W4I75301.                                                        
000200*                                 MID-COPYTEXT FÖR W4075300               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-KDANMORS-IN      PIC X(2).                                    
001200*                                 ORSAK TILL LEVERANSANMÄRKNING           
001300     03 MID-KDANMORS-UT      PIC X(2).                                    
001400*                                 ORSAK TILL LEVERANSANMÄRKNING           
001500     03 MID-FLVISA-IN        PIC X.                                       
001600*                                 ALLMÄN FLAGGA FÖR DATAVISNING           
001700     03 MID-FLVISA-UT        PIC X.                                       
001800*                                 ALLMÄN FLAGGA FÖR DATAVISNING           
001900     03 MID-INPUT.                                                        
002000*                                 RADINFORMATION                          
002100        05 MID-KDCMD         OCCURS 8 TIMES                               
002200                             PIC X.                                       
002300         88 MID-KDCMD-INGENTING                                           
002400                             VALUE ' '.                                   
002500         88 MID-KDCMD-DELETE VALUE 'D'                                    
002600                             'B'.                                         
002700         88 MID-KDCMD-REPLACE                                             
002800                             VALUE 'R'                                    
002900                             'Ä'.                                         
003000         88 MID-KDCMD-INSERT VALUE 'I'                                    
003100                             'N'                                          
003200                             'A'.                                         
003300         88 MID-KDCMD-SELECT VALUE 'S'                                    
003400                             'V'.                                         
003500         88 MID-KDCMD-PRINT  VALUE 'P'                                    
003600                             'P'.                                         
003700         88 MID-KDCMD-COPY   VALUE 'C'                                    
003800                             'K'.                                         
003900*                                 RAD-UPPDATERINGSKOMMANDO                
004000*                                  BLANK  = INGENTING                     
004100*                                  D , B  = DELETE                        
004200*                                  R , Ä  = REPLACE                       
004300*                                  I,N,A  = INSERT                        
004400*                                  S , V  = SELECT                        
004500*                                  P , P  = PRINT                         
004600*                                  C , K  = COPY                          
004700        05 MID-KDCMD-DEF     PIC X.                                       
004800         88 MID-KDCMD-INGENTING                                           
004900                             VALUE ' '.                                   
005000         88 MID-KDCMD-DELETE VALUE 'D'                                    
005100                             'B'.                                         
005200         88 MID-KDCMD-REPLACE                                             
005300                             VALUE 'R'                                    
005400                             'Ä'.                                         
005500         88 MID-KDCMD-INSERT VALUE 'I'                                    
005600                             'N'                                          
005700                             'A'.                                         
005800         88 MID-KDCMD-SELECT VALUE 'S'                                    
005900                             'V'.                                         
006000         88 MID-KDCMD-PRINT  VALUE 'P'                                    
006100                             'P'.                                         
006200         88 MID-KDCMD-COPY   VALUE 'C'                                    
006300                             'K'.                                         
006400*                                 RAD-UPPDATERINGSKOMMANDO                
006500*                                  BLANK  = INGENTING                     
006600*                                  D , B  = DELETE                        
006700*                                  R , Ä  = REPLACE                       
006800*                                  I,N,A  = INSERT                        
006900*                                  S , V  = SELECT                        
007000*                                  P , P  = PRINT                         
007100*                                  C , K  = COPY                          
007200        05 MID-KDCMD-UPD     PIC X.                                       
007300         88 MID-KDCMD-INGENTING                                           
007400                             VALUE ' '.                                   
007500         88 MID-KDCMD-DELETE VALUE 'D'                                    
007600                             'B'.                                         
007700         88 MID-KDCMD-REPLACE                                             
007800                             VALUE 'R'                                    
007900                             'Ä'.                                         
008000         88 MID-KDCMD-INSERT VALUE 'I'                                    
008100                             'N'                                          
008200                             'A'.                                         
008300         88 MID-KDCMD-SELECT VALUE 'S'                                    
008400                             'V'.                                         
008500         88 MID-KDCMD-PRINT  VALUE 'P'                                    
008600                             'P'.                                         
008700         88 MID-KDCMD-COPY   VALUE 'C'                                    
008800                             'K'.                                         
008900*                                 RAD-UPPDATERINGSKOMMANDO                
009000*                                  BLANK  = INGENTING                     
009100*                                  D , B  = DELETE                        
009200*                                  R , Ä  = REPLACE                       
009300*                                  I,N,A  = INSERT                        
009400*                                  S , V  = SELECT                        
009500*                                  P , P  = PRINT                         
009600*                                  C , K  = COPY                          
009700        05 MID-IDDISTR-FOM-UPD                                            
009800                             PIC X(4).                                    
009900*                                 LÄGSTA DISTRIKTNR I INTERVALL           
010000        05 MID-IDDISTR-TOM-UPD                                            
010100                             PIC X(4).                                    
010200*                                 HÖGSTA DISTRIKTNR I INTERVALL           
010300        05 MID-IDKUNDNR-FOM-UPD                                           
010400                             PIC X(6).                                    
010500*                                 LÄGSTA KUNDNUMMER I INTERVALL           
010600        05 MID-IDKUNDNR-TOM-UPD                                           
010700                             PIC X(6).                                    
010800*                                 HÖGSTA KUNDNUMMER I INTERVALL           
010900        05 MID-KDANMORS-UPD  PIC X(2).                                    
011000*                                 ORSAK TILL LEVERANSANMÄRKNING           
011100        05 MID-KVDAGAR-LTRP-UPD                                           
011200                             PIC 9(3).                                    
011300*                                 DAGAR FÖR TRANSP. AV LEV.ANM.           
011400        05 MID-KVDAGAR-LEVANM-UPD                                         
011500                             PIC 9(3).                                    
011600*                                 ANT. DAGAR MAN KAN LEV.ANM.             
011700        05 MID-KVDAGAR-LTRP-LDC-UPD                                       
011800                             PIC 9(3).                                    
011900*                                 DAGAR FÖR TRP. AV LEV.ANM. LDC          
012000        05 MID-KVDAGAR-LEVANM-LDC-UPD                                     
012100                             PIC 9(3).                                    
012200*                                 ANT. DAGAR MAN KAN LEV.ANM. LDC         
012300        05 MID-KVDAGAR-RET-UPD                                            
012400                             PIC 9(3).                                    
012500*                                 NO. OF DAYS RETURN HANDLING FOR         
012600*                                 A DESCRAPENCY REPORT                    
012700        05 MID-KVDAGAR-RTRP-UPD                                           
012800                             PIC 9(3).                                    
012900*                                 DAGAR FÖR TRANSP. AV RETURER            
013000        05 MID-KVDAGAR-RET-LDC-UPD                                        
013100                             PIC 9(3).                                    
013200*                                 ANTAL DAGAR FÖR RETURHANT. LDC          
013300        05 MID-KVDAGAR-RTRP-LDC-UPD                                       
013400                             PIC 9(3).                                    
013500*                                 DAGAR FÖR TRP. AV LDC RETURER           
013600     03 MID-INFO-RAD         OCCURS 8 TIMES.                              
013700*                                 RADINFORMATION                          
013800        05 MID-IDDISTR-FOM   PIC 9(4).                                    
013900*                                 LÄGSTA DISTRIKTNR I INTERVALL           
014000        05 MID-IDDISTR-TOM   PIC 9(4).                                    
014100*                                 HÖGSTA DISTRIKTNR I INTERVALL           
014200        05 MID-IDKUNDNR-FOM  PIC 9(6).                                    
014300*                                 LÄGSTA KUNDNUMMER I INTERVALL           
014400        05 MID-IDKUNDNR-TOM  PIC 9(6).                                    
014500*                                 HÖGSTA KUNDNUMMER I INTERVALL           
014600        05 MID-KDANMORS      PIC X(2).                                    
014700*                                 ORSAK TILL LEVERANSANMÄRKNING           
014800        05 MID-KVDAGAR-LTRP  PIC 9(3).                                    
014900*                                 DAGAR FÖR TRANSP. AV LEV.ANM.           
015000        05 MID-KVDAGAR-LEVANM                                             
015100                             PIC 9(3).                                    
015200*                                 ANT. DAGAR MAN KAN LEV.ANM.             
015300        05 MID-KVDAGAR-LTRP-LDC                                           
015400                             PIC 9(3).                                    
015500*                                 DAGAR FÖR TRP. AV LEV.ANM. LDC          
015600        05 MID-KVDAGAR-LEVANM-LDC                                         
015700                             PIC 9(3).                                    
015800*                                 ANT. DAGAR MAN KAN LEV.ANM. LDC         
015900        05 MID-KVDAGAR-RET   PIC 9(3).                                    
016000*                                 NO. OF DAYS RETURN HANDLING FOR         
016100*                                 A DESCRAPENCY REPORT                    
016200        05 MID-KVDAGAR-RTRP  PIC 9(3).                                    
016300*                                 DAGAR FÖR TRANSP. AV RETURER            
016400        05 MID-KVDAGAR-RET-LDC                                            
016500                             PIC 9(3).                                    
016600*                                 ANTAL DAGAR FÖR RETURHANT. LDC          
016700        05 MID-KVDAGAR-RTRP-LDC                                           
016800                             PIC 9(3).                                    
016900*                                 DAGAR FÖR TRP. AV LDC RETURER           
017000     03 MID-IDDISTR-FOM-DEF  PIC X(4).                                    
017100*                                 LÄGSTA DISTRIKTNR I INTERVALL           
017200     03 MID-IDDISTR-TOM-DEF  PIC X(4).                                    
017300*                                 HÖGSTA DISTRIKTNR I INTERVALL           
017400     03 MID-KVDAGAR-LTRP-DEF PIC 9(3).                                    
017500*                                 DAGAR FÖR TRANSP. AV LEV.ANM.           
017600     03 MID-KVDAGAR-LEVANM-DEF                                            
017700                             PIC 9(3).                                    
017800*                                 ANT. DAGAR MAN KAN LEV.ANM.             
017900     03 MID-KVDAGAR-RET-DEF  PIC 9(3).                                    
018000*                                 NO. OF DAYS RETURN HANDLING FOR         
018100*                                 A DESCRAPENCY REPORT                    
018200     03 MID-KVDAGAR-RTRP-DEF PIC 9(3).                                    
018300*                                 DAGAR FÖR TRANSP. AV RETURER            
018400*** END OF VILMAII-COPY LENGTH= 470 BYTES                                 
