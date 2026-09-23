000100 01  MOD-W0O81601.                                                        
000200*                                 MOD-COPYTEXT FÖR W00816                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-UTDATA.                                                       
001200*                                 UTDATA 0816                             
001300        05 MOD-BEARTKUL.                                                  
001400*                                 KULLAGERBENÄMNING                       
001500           07 MOD-BEARTKUL-1 PIC X(15).                                   
001600*                                 DEL AV KULLAGERBENÄMNING                
001700           07 MOD-BEARTKUL-2 PIC X(15).                                   
001800*                                 DEL AV KULLAGERBENÄMNING                
001900        05 MOD-BELEVKUL.                                                  
002000*                                 LEVERANSBENÄMNING                       
002100           07 MOD-BELEVKUL-1 PIC X(11).                                   
002200*                                 LEVERANSBENÄMNING DEL 1                 
002300           07 MOD-BELEVKUL-2 PIC X(10).                                   
002400*                                 LEVERANSBENÄMNING DEL 2                 
002500        05 MOD-DIKULLAG-INNER                                             
002600                             PIC Z(3)9.9.                                 
002700*                                 INNERDIAMETER PÅ KULLAGER               
002800        05 MOD-DIKULLAG-YTTER                                             
002900                             PIC Z(3)9.9.                                 
003000*                                 YTTERDIAMETER PÅ KULLAGER               
003100        05 MOD-IDLEVKUL.                                                  
003200*                                 LEVERANSIDENTITET                       
003300           07 MOD-IDLEVKUL-1 PIC X(8).                                    
003400*                                 DEL AV LEVERANSIDENTITET                
003500           07 MOD-IDLEVKUL-2 PIC X(8).                                    
003600*                                 DEL AV LEVERANSIDENTITET                
003700        05 MOD-IDSTAKUL      PIC Z(8)9.                                   
003800*                                 STATISTISKT NUMMER FRÅN USA             
003900        05 MOD-VKARTNTO      PIC Z(3)9.9(3).                              
004000*                                 ARTIKELVIKT NETTO (KG)                  
004100        05 MOD-KDARTURS      PIC X(2).                                    
004200*                                 ARTIKELURSPRUNGSKOD                     
004300     03 MOD-INDATA.                                                       
004400*                                 INDATA BILD 0816                        
004500        05 MOD-KDCMD-BEARTKUL-ATTR                                        
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-KDCMD-BEARTKUL                                             
004900                             PIC X.                                       
005000*                                 RAD-UPPDATERINGSKOMMANDO                
005100*                                  BLANK  = INGENTING                     
005200*                                  D , B  = DELETE                        
005300*                                  R , Ä  = REPLACE                       
005400*                                  I , N  = INSERT                        
005500        05 MOD-BEARTKUL-1-IN-ATTR                                         
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-BEARTKUL-1-IN PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000        05 MOD-BEARTKUL-2-IN-ATTR                                         
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-BEARTKUL-2-IN PIC X(2).                                    
006400*                                 MFS BEHANDLING AV INPUTFÄLT             
006500        05 MOD-KDCMD-BELEVKUL-ATTR                                        
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-KDCMD-BELEVKUL                                             
006900                             PIC X.                                       
007000*                                 RAD-UPPDATERINGSKOMMANDO                
007100*                                  BLANK  = INGENTING                     
007200*                                  D , B  = DELETE                        
007300*                                  R , Ä  = REPLACE                       
007400*                                  I , N  = INSERT                        
007500        05 MOD-BELEVKUL-1-IN-ATTR                                         
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-BELEVKUL-1-IN PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000        05 MOD-BELEVKUL-2-IN-ATTR                                         
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-BELEVKUL-2-IN PIC X(2).                                    
008400*                                 MFS BEHANDLING AV INPUTFÄLT             
008500        05 MOD-KDCMD-DIKULLAG-INNER-ATTR                                  
008600                             PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-KDCMD-DIKULLAG-INNER                                       
008900                             PIC X.                                       
009000*                                 RAD-UPPDATERINGSKOMMANDO                
009100*                                  BLANK  = INGENTING                     
009200*                                  D , B  = DELETE                        
009300*                                  R , Ä  = REPLACE                       
009400*                                  I , N  = INSERT                        
009500        05 MOD-DIKULLAG-INNER-IN-ATTR                                     
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800        05 MOD-DIKULLAG-INNER-IN                                          
009900                             PIC X(2).                                    
010000*                                 MFS BEHANDLING AV INPUTFÄLT             
010100        05 MOD-KDCMD-DIKULLAG-YTTER-ATTR                                  
010200                             PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-KDCMD-DIKULLAG-YTTER                                       
010500                             PIC X.                                       
010600*                                 RAD-UPPDATERINGSKOMMANDO                
010700*                                  BLANK  = INGENTING                     
010800*                                  D , B  = DELETE                        
010900*                                  R , Ä  = REPLACE                       
011000*                                  I , N  = INSERT                        
011100        05 MOD-DIKULLAG-YTTER-IN-ATTR                                     
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400        05 MOD-DIKULLAG-YTTER-IN                                          
011500                             PIC X(2).                                    
011600*                                 MFS BEHANDLING AV INPUTFÄLT             
011700        05 MOD-KDCMD-IDLEVKUL-ATTR                                        
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-KDCMD-IDLEVKUL                                             
012100                             PIC X.                                       
012200*                                 RAD-UPPDATERINGSKOMMANDO                
012300*                                  BLANK  = INGENTING                     
012400*                                  D , B  = DELETE                        
012500*                                  R , Ä  = REPLACE                       
012600*                                  I , N  = INSERT                        
012700        05 MOD-IDLEVKUL-1-IN-ATTR                                         
012800                             PIC X(2).                                    
012900*                                 MFS ATTRIBUTFÄLT                        
013000        05 MOD-IDLEVKUL-1-IN PIC X(2).                                    
013100*                                 MFS BEHANDLING AV INPUTFÄLT             
013200        05 MOD-IDLEVKUL-2-IN-ATTR                                         
013300                             PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500        05 MOD-IDLEVKUL-2-IN PIC X(2).                                    
013600*                                 MFS BEHANDLING AV INPUTFÄLT             
013700        05 MOD-KDCMD-IDSTAKUL-ATTR                                        
013800                             PIC X(2).                                    
013900*                                 MFS ATTRIBUTFÄLT                        
014000        05 MOD-KDCMD-IDSTAKUL                                             
014100                             PIC X.                                       
014200*                                 RAD-UPPDATERINGSKOMMANDO                
014300*                                  BLANK  = INGENTING                     
014400*                                  D , B  = DELETE                        
014500*                                  R , Ä  = REPLACE                       
014600*                                  I , N  = INSERT                        
014700        05 MOD-IDSTAKUL-IN-ATTR                                           
014800                             PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000        05 MOD-IDSTAKUL-IN   PIC X(2).                                    
015100*                                 MFS BEHANDLING AV INPUTFÄLT             
015200        05 MOD-KDCMD-VKARTNTO-ATTR                                        
015300                             PIC X(2).                                    
015400*                                 MFS ATTRIBUTFÄLT                        
015500        05 MOD-KDCMD-VKARTNTO                                             
015600                             PIC X.                                       
015700*                                 RAD-UPPDATERINGSKOMMANDO                
015800*                                  BLANK  = INGENTING                     
015900*                                  D , B  = DELETE                        
016000*                                  R , Ä  = REPLACE                       
016100*                                  I , N  = INSERT                        
016200        05 MOD-VKARTNTO-IN-ATTR                                           
016300                             PIC X(2).                                    
016400*                                 MFS ATTRIBUTFÄLT                        
016500        05 MOD-VKARTNTO-IN   PIC X(2).                                    
016600*                                 MFS BEHANDLING AV INPUTFÄLT             
016700        05 MOD-KDCMD-KDARTURS-ATTR                                        
016800                             PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000        05 MOD-KDCMD-KDARTURS                                             
017100                             PIC X.                                       
017200*                                 RAD-UPPDATERINGSKOMMANDO                
017300*                                  BLANK  = INGENTING                     
017400*                                  D , B  = DELETE                        
017500*                                  R , Ä  = REPLACE                       
017600*                                  I , N  = INSERT                        
017700        05 MOD-KDARTURS-IN-ATTR                                           
017800                             PIC X(2).                                    
017900*                                 MFS ATTRIBUTFÄLT                        
018000        05 MOD-KDARTURS-IN   PIC X(2).                                    
018100*                                 MFS BEHANDLING AV INPUTFÄLT             
018200     03 MOD-FLANNULL-ATTR    PIC X(2).                                    
018300*                                 MFS ATTRIBUTFÄLT                        
018400     03 MOD-FLANNULL         PIC X.                                       
018500*                                 ANNULLATION                             
018600     03 MOD-TEMFSINF         PIC X(55).                                   
018700*                                 INFORMATIONSMEDDELANDE                  
018800*** END OF VILMAII-COPY LENGTH= 286 BYTES                                 
