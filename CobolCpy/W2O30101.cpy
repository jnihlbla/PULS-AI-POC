000100 01  MOD-W2O30101.                                                        
000200*                                 MODCOPYTEXT TILL W20301.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDANSK-FOM-IN    PIC X(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MOD-IDANSK-TOM-IN    PIC X(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MOD-FLBYGGB-IN       PIC X.                                       
001200*                                 FLAGGA BYGGBAR SATSORDER                
001300     03 MOD-IDARTNR-IN       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDORDNSB-IN      PIC X(4).                                    
001600*                                 SATSORDERNUMMER-BAS                     
001700     03 MOD-IDORDNSS-IN      PIC X.                                       
001800*                                 SATSORDERNUMMER-SUFFIX                  
001900     03 MOD-ING-IDARTNR-IN   PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-KDSATKMB-IN      PIC X.                                       
002200*                                 KOMBINATIONSKOD SATS                    
002300     03 MOD-IDANSK-FOM-UT    PIC X(3).                                    
002400*                                 ANSKAFFARNUMMER                         
002500     03 MOD-IDANSK-TOM-UT    PIC X(3).                                    
002600*                                 ANSKAFFARNUMMER                         
002700     03 MOD-FLBYGGB-UT       PIC X.                                       
002800*                                 FLAGGA BYGGBAR SATSORDER                
002900     03 MOD-IDARTNR-UT       PIC X(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MOD-IDORDNSB-UT      PIC X(4).                                    
003200*                                 SATSORDERNUMMER-BAS                     
003300     03 MOD-IDORDNSS-UT      PIC X.                                       
003400*                                 SATSORDERNUMMER-SUFFIX                  
003500     03 MOD-ING-IDARTNR-UT   PIC X(9).                                    
003600*                                 ARTIKELNUMMER                           
003700     03 MOD-KDSATKMB-UT      PIC X.                                       
003800*                                 KOMBINATIONSKOD SATS                    
003900     03 MOD-IDANSK-ENTER     PIC 9(3).                                    
004000*                                 ANSKAFFARNUMMER                         
004100     03 MOD-IDANSK-NEXT      PIC 9(3).                                    
004200*                                 ANSKAFFARNUMMER                         
004300     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
004400*                                 ARTIKELNUMMER                           
004500     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
004600*                                 ARTIKELNUMMER                           
004700     03 MOD-IDORDNSB-ENTER   PIC 9(4).                                    
004800*                                 SATSORDERNUMMER-BAS                     
004900     03 MOD-IDORDNSS-ENTER   PIC X.                                       
005000*                                 SATSORDERNUMMER-SUFFIX                  
005100     03 MOD-IDORDNSB-NEXT    PIC 9(4).                                    
005200*                                 SATSORDERNUMMER-BAS                     
005300     03 MOD-IDORDNSS-NEXT    PIC X.                                       
005400*                                 SATSORDERNUMMER-SUFFIX                  
005500     03 MOD-TIREGDAT-ENTER   PIC 9(6).                                    
005600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005700     03 MOD-TIREGDAT-NEXT    PIC 9(6).                                    
005800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005900     03 MOD-FLBYGGB-ENTER    PIC X.                                       
006000*                                 FLAGGA BYGGBAR SATSORDER                
006100     03 MOD-FLBYGGB-NEXT     PIC X.                                       
006200*                                 FLAGGA BYGGBAR SATSORDER                
006300     03 MOD-CMD-RAD          OCCURS 14 TIMES                              
006400                             PIC X.                                       
006500*                                 BEHANDLINGSKOD-X                        
006600     03 MOD-IDANSK-RAD       OCCURS 14 TIMES                              
006700                             PIC Z(2)9.                                   
006800*                                 ANSKAFFARNUMMER                         
006900     03 MOD-IDARTNR-RAD      OCCURS 14 TIMES                              
007000                             PIC X(11).                                   
007100*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
007200     03 MOD-IDORDNST-RAD     OCCURS 14 TIMES.                             
007300*                                 SATSORDERNUMMER-TOTALT                  
007400        05 MOD-IDORDNSB      PIC Z(3)9.                                   
007500*                                 SATSORDERNUMMER-BAS                     
007600        05 MOD-IDORDNSS      PIC 9.                                       
007700*                                 SATSORDERNUMMER-SUFFIX                  
007800     03 MOD-TIREGDAT-RAD     OCCURS 14 TIMES                              
007900                             PIC 9(6).                                    
008000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008100     03 MOD-TIBEGPAC-RAD     OCCURS 14 TIMES                              
008200                             PIC 9(6).                                    
008300*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
008400     03 MOD-KVBEART-RAD      OCCURS 14 TIMES                              
008500                             PIC Z(7).                                    
008600*                                 BESTÄLLT ANTAL ARTIKLAR                 
008700     03 MOD-KVBYGGB-RAD      OCCURS 14 TIMES                              
008800                             PIC Z(6)9.                                   
008900*                                 ANTAL BYGGBARA SATSER                   
009000     03 MOD-KDCLAGER-RAD     OCCURS 14 TIMES                              
009100                             PIC 9.                                       
009200*                                 CENTRALLAGERKOD                         
009300     03 MOD-IDPRC-RAD        OCCURS 14 TIMES.                             
009400*                                 PRODUKTIONSKANAL                        
009500        05 MOD-IDPRCBAS      PIC X(3).                                    
009600*                                 PRC-BAS                                 
009700        05 MOD-IDPRCVAR      PIC X.                                       
009800*                                 PRC-VARIANT                             
009900     03 MOD-FLBYGGB-RAD      OCCURS 14 TIMES                              
010000                             PIC X.                                       
010100*                                 FLAGGA BYGGBAR SATSORDER                
010200     03 MOD-TEMFSINF         PIC X(61).                                   
010300*                                 INFORMATIONSMEDDELANDE                  
010400*** END COPY W2O30101C0  LENGTH=943                                       
