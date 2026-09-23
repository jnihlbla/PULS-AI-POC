000100 01  MID-W4I41301.                                                        
000200*                                                                         
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-INPUT-U-TRANS.                                                
001200*                                                                         
001300        05 MID-IDDC-RET      PIC X(2).                                    
001400*                                 MOTTAGANDE LAGER FÖR RETURER            
001500        05 MID-FLRETFG       PIC X.                                       
001600*                                 FARLIGT GODS RETUR FLAGGA               
001700        05 MID-FLOKFAK-G     PIC X.                                       
001800*                                 FLAGGA FAKTURATYP G GODKÄND             
001900        05 MID-FLOKFAK-K     PIC X.                                       
002000*                                 FLAGGA FAKTURATYP K GODKÄND             
002100        05 MID-FLOKFAK-N     PIC X.                                       
002200*                                 FLAGGA FAKTURATYP N GODKÄND             
002300        05 MID-FLOKFAK-R     PIC X.                                       
002400*                                 FLAGGA FAKTURATYP R GODKÄND             
002500        05 MID-KDGENFAK      PIC X.                                       
002600*                                 NORMAL FAKTURATYP                       
002700     03 MID-INPUT-V-TRANS.                                                
002800*                                                                         
002900        05 MID-FLLDCKND      PIC X.                                       
003000*                                 FL LDC-KUND                             
003100        05 MID-KVDAGAR-SDC   PIC 9(2).                                    
003200*                                 SDC-DAGAR                               
003300        05 MID-KVDAGAR-CDC   PIC 9(2).                                    
003400*                                 CDC-DAGAR                               
003500        05 MID-FLRETUR       PIC X.                                       
003600*                                 FLAGGA RETUR OK.                        
003700        05 MID-IDDC-RET72    OCCURS 3 TIMES                               
003800                             PIC X(2).                                    
003900*                                 MOTTAGANDE LAGER FÖR 72-RETURER         
004000        05 MID-RFSDC-GRP     OCCURS 4 TIMES.                              
004100*                                                                         
004200           07 MID-IDDC-RFS   PIC X(2).                                    
004300*                                 DC FÖR RFS DAGAR FÖRE REPDAG            
004400           07 MID-KVDAGAR-RFS                                             
004500                             PIC 9.                                       
004600*                                 ANT DGR FÖR RFS/DC FÖRE REPDAT          
004700        05 MID-KVDAGAR-RFS-DEF                                            
004800                             PIC 9.                                       
004900*                                 DEFAULT DAGAR RFS FÖRE REPDAT           
005000        05 MID-FLKVBRYT-ORDKL1                                            
005100                             PIC X.                                       
005200*                                 KVANTITET BRYTES                        
005300        05 MID-FLKVBRYT-ORDKL2                                            
005400                             PIC X.                                       
005500*                                 KVANTITET BRYTES                        
005600        05 MID-FLKVBRYT-ORDKL3                                            
005700                             PIC X.                                       
005800*                                 KVANTITET BRYTES                        
005900        05 MID-FLKVBRYT-ORDKL4                                            
006000                             PIC X.                                       
006100*                                 KVANTITET BRYTES                        
006200     03 MID-FLAUTREM         PIC X.                                       
006300*                                 AUTOMATISK REMISS (Y/N)                 
006400*** END OF VILMAII-COPY LENGTH= 58 BYTES                                  
