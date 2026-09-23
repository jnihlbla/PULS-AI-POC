000100 01  MOD-W4O75301.                                                        
000200*                                 MOD-COPYTEXT FÖR W4075300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-KDANMORS-IN      PIC X(2).                                    
001600*                                 ORSAK TILL LEVERANSANMÄRKNING           
001700     03 MOD-KDANMORS-UT      PIC X(2).                                    
001800*                                 ORSAK TILL LEVERANSANMÄRKNING           
001900     03 MOD-FLVISA-IN        PIC X.                                       
002000*                                 ALLMÄN FLAGGA FÖR DATAVISNING           
002100     03 MOD-FLVISA-UT        PIC X.                                       
002200*                                 ALLMÄN FLAGGA FÖR DATAVISNING           
002300     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
002400*                                 RADINFORMATION                          
002500        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-KDCMD         PIC X.                                       
002800*                                 RAD-UPPDATERINGSKOMMANDO                
002900*                                  BLANK  = INGENTING                     
003000*                                  D , B  = DELETE                        
003100*                                  R , Ä  = REPLACE                       
003200*                                  I,N,A  = INSERT                        
003300*                                  S , V  = SELECT                        
003400*                                  P , P  = PRINT                         
003500*                                  C , K  = COPY                          
003600        05 MOD-IDDISTR-FOM-ATTR                                           
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-IDDISTR-FOM   PIC Z(4).                                    
004000*                                 LÄGSTA DISTRIKTNR I INTERVALL           
004100        05 MOD-IDDISTR-TOM-ATTR                                           
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDDISTR-TOM   PIC Z(4).                                    
004500*                                 HÖGSTA DISTRIKTNR I INTERVALL           
004600        05 MOD-IDKUNDNR-FOM-ATTR                                          
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-IDKUNDNR-FOM  PIC Z(5)9.                                   
005000*                                 LÄGSTA KUNDNUMMER I INTERVALL           
005100        05 MOD-IDKUNDNR-TOM-ATTR                                          
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-IDKUNDNR-TOM  PIC Z(5)9.                                   
005500*                                 HÖGSTA KUNDNUMMER I INTERVALL           
005600        05 MOD-KDANMORS-ATTR PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-KDANMORS      PIC X(2).                                    
005900*                                 ORSAK TILL LEVERANSANMÄRKNING           
006000        05 MOD-KVDAGAR-LTRP-ATTR                                          
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-KVDAGAR-LTRP  PIC Z(3).                                    
006400*                                 DAGAR FÖR TRANSP. AV LEV.ANM.           
006500        05 MOD-KVDAGAR-LEVANM-ATTR                                        
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-KVDAGAR-LEVANM                                             
006900                             PIC Z(3).                                    
007000*                                 ANT. DAGAR MAN KAN LEV.ANM.             
007100        05 MOD-KVDAGAR-LTRP-LDC-ATTR                                      
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-KVDAGAR-LTRP-LDC                                           
007500                             PIC Z(3).                                    
007600*                                 DAGAR FÖR TRP. AV LEV.ANM. LDC          
007700        05 MOD-KVDAGAR-LEVANM-LDC-ATTR                                    
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 MOD-KVDAGAR-LEVANM-LDC                                         
008100                             PIC Z(3).                                    
008200*                                 ANT. DAGAR MAN KAN LEV.ANM. LDC         
008300        05 MOD-KVDAGAR-RET-ATTR                                           
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-KVDAGAR-RET   PIC Z(3).                                    
008700*                                 NO. OF DAYS RETURN HANDLING FOR         
008800*                                 A DESCRAPENCY REPORT                    
008900        05 MOD-KVDAGAR-RTRP-ATTR                                          
009000                             PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-KVDAGAR-RTRP  PIC Z(3).                                    
009300*                                 DAGAR FÖR TRANSP. AV RETURER            
009400        05 MOD-KVDAGAR-RET-LDC-ATTR                                       
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 MOD-KVDAGAR-RET-LDC                                            
009800                             PIC Z(3).                                    
009900*                                 ANTAL DAGAR FÖR RETURHANT. LDC          
010000        05 MOD-KVDAGAR-RTRP-LDC-ATTR                                      
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300        05 MOD-KVDAGAR-RTRP-LDC                                           
010400                             PIC Z(3).                                    
010500*                                 DAGAR FÖR TRP. AV LDC RETURER           
010600     03 MOD-KDCMD-DEF-ATTR   PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800     03 MOD-KDCMD-DEF        PIC X.                                       
010900*                                 RAD-UPPDATERINGSKOMMANDO                
011000*                                  BLANK  = INGENTING                     
011100*                                  D , B  = DELETE                        
011200*                                  R , Ä  = REPLACE                       
011300*                                  I,N,A  = INSERT                        
011400*                                  S , V  = SELECT                        
011500*                                  P , P  = PRINT                         
011600*                                  C , K  = COPY                          
011700     03 MOD-IDDISTR-FOM-DEF-ATTR                                          
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000     03 MOD-IDDISTR-FOM-DEF  PIC Z(3)9.                                   
012100*                                 LÄGSTA DISTRIKTNR I INTERVALL           
012200     03 MOD-IDDISTR-TOM-DEF-ATTR                                          
012300                             PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500     03 MOD-IDDISTR-TOM-DEF  PIC Z(3)9.                                   
012600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
012700     03 MOD-KVDAGAR-LTRP-DEF-ATTR                                         
012800                             PIC X(2).                                    
012900*                                 MFS ATTRIBUTFÄLT                        
013000     03 MOD-KVDAGAR-LTRP-DEF PIC Z(3).                                    
013100*                                 DAGAR FÖR TRANSP. AV LEV.ANM.           
013200     03 MOD-KVDAGAR-LEVANM-DEF-ATTR                                       
013300                             PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500     03 MOD-KVDAGAR-LEVANM-DEF                                            
013600                             PIC Z(3).                                    
013700*                                 ANT. DAGAR MAN KAN LEV.ANM.             
013800     03 MOD-KVDAGAR-RET-DEF-ATTR                                          
013900                             PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 MOD-KVDAGAR-RET-DEF  PIC Z(3).                                    
014200*                                 NO. OF DAYS RETURN HANDLING FOR         
014300*                                 A DESCRAPENCY REPORT                    
014400     03 MOD-KVDAGAR-RTRP-DEF-ATTR                                         
014500                             PIC X(2).                                    
014600*                                 MFS ATTRIBUTFÄLT                        
014700     03 MOD-KVDAGAR-RTRP-DEF PIC Z(3).                                    
014800*                                 DAGAR FÖR TRANSP. AV RETURER            
014900     03 MOD-KDCMD-UPD-ATTR   PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100     03 MOD-KDCMD-UPD        PIC X.                                       
015200*                                 RAD-UPPDATERINGSKOMMANDO                
015300*                                  BLANK  = INGENTING                     
015400*                                  D , B  = DELETE                        
015500*                                  R , Ä  = REPLACE                       
015600*                                  I,N,A  = INSERT                        
015700*                                  S , V  = SELECT                        
015800*                                  P , P  = PRINT                         
015900*                                  C , K  = COPY                          
016000     03 MOD-IDDISTR-FOM-UPD-ATTR                                          
016100                             PIC X(2).                                    
016200*                                 MFS ATTRIBUTFÄLT                        
016300     03 MOD-IDDISTR-FOM-UPD  PIC Z(3)9.                                   
016400*                                 LÄGSTA DISTRIKTNR I INTERVALL           
016500     03 MOD-IDDISTR-TOM-UPD-ATTR                                          
016600                             PIC X(2).                                    
016700*                                 MFS ATTRIBUTFÄLT                        
016800     03 MOD-IDDISTR-TOM-UPD  PIC Z(3)9.                                   
016900*                                 HÖGSTA DISTRIKTNR I INTERVALL           
017000     03 MOD-IDKUNDNR-FOM-UPD-ATTR                                         
017100                             PIC X(2).                                    
017200*                                 MFS ATTRIBUTFÄLT                        
017300     03 MOD-IDKUNDNR-FOM-UPD PIC Z(5)9.                                   
017400*                                 LÄGSTA KUNDNUMMER I INTERVALL           
017500     03 MOD-IDKUNDNR-TOM-UPD-ATTR                                         
017600                             PIC X(2).                                    
017700*                                 MFS ATTRIBUTFÄLT                        
017800     03 MOD-IDKUNDNR-TOM-UPD PIC Z(5)9.                                   
017900*                                 HÖGSTA KUNDNUMMER I INTERVALL           
018000     03 MOD-KDANMORS-UPD-ATTR                                             
018100                             PIC X(2).                                    
018200*                                 MFS ATTRIBUTFÄLT                        
018300     03 MOD-KDANMORS-UPD     PIC X(2).                                    
018400*                                 ORSAK TILL LEVERANSANMÄRKNING           
018500     03 MOD-KVDAGAR-LTRP-UPD-ATTR                                         
018600                             PIC X(2).                                    
018700*                                 MFS ATTRIBUTFÄLT                        
018800     03 MOD-KVDAGAR-LTRP-UPD PIC Z(3).                                    
018900*                                 DAGAR FÖR TRANSP. AV LEV.ANM.           
019000     03 MOD-KVDAGAR-LEVANM-UPD-ATTR                                       
019100                             PIC X(2).                                    
019200*                                 MFS ATTRIBUTFÄLT                        
019300     03 MOD-KVDAGAR-LEVANM-UPD                                            
019400                             PIC Z(3).                                    
019500*                                 ANT. DAGAR MAN KAN LEV.ANM.             
019600     03 MOD-KVDAGAR-LTRP-LDC-UPD-ATTR                                     
019700                             PIC X(2).                                    
019800*                                 MFS ATTRIBUTFÄLT                        
019900     03 MOD-KVDAGAR-LTRP-LDC-UPD                                          
020000                             PIC Z(3).                                    
020100*                                 DAGAR FÖR TRP. AV LEV.ANM. LDC          
020200     03 MOD-KVDAGAR-LEVNM-LDC-UPD-ATTR                                    
020300                             PIC X(2).                                    
020400*                                 MFS ATTRIBUTFÄLT                        
020500     03 MOD-KVDAGAR-LEVANM-LDC-UPD                                        
020600                             PIC Z(3).                                    
020700*                                 ANT. DAGAR MAN KAN LEV.ANM. LDC         
020800     03 MOD-KVDAGAR-RET-UPD-ATTR                                          
020900                             PIC X(2).                                    
021000*                                 MFS ATTRIBUTFÄLT                        
021100     03 MOD-KVDAGAR-RET-UPD  PIC Z(3).                                    
021200*                                 NO. OF DAYS RETURN HANDLING FOR         
021300*                                 A DESCRAPENCY REPORT                    
021400     03 MOD-KVDAGAR-RTRP-UPD-ATTR                                         
021500                             PIC X(2).                                    
021600*                                 MFS ATTRIBUTFÄLT                        
021700     03 MOD-KVDAGAR-RTRP-UPD PIC Z(3).                                    
021800*                                 DAGAR FÖR TRANSP. AV RETURER            
021900     03 MOD-KVDAGAR-RET-LDC-UPD-ATTR                                      
022000                             PIC X(2).                                    
022100*                                 MFS ATTRIBUTFÄLT                        
022200     03 MOD-KVDAGAR-RET-LDC-UPD                                           
022300                             PIC Z(3).                                    
022400*                                 ANTAL DAGAR FÖR RETURHANT. LDC          
022500     03 MOD-KVDAGAR-RTRP-LDC-UPD-ATTR                                     
022600                             PIC X(2).                                    
022700*                                 MFS ATTRIBUTFÄLT                        
022800     03 MOD-KVDAGAR-RTRP-LDC-UPD                                          
022900                             PIC Z(3).                                    
023000*                                 DAGAR FÖR TRP. AV LDC RETURER           
023100     03 MOD-TEMFSINF         PIC X(55).                                   
023200*                                 INFORMATIONSMEDDELANDE                  
023300*** END OF VILMAII-COPY LENGTH= 835 BYTES                                 
