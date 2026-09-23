000100 01  MOD-W4O71501.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4071500           
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
001500     03 MOD-IDRAPPNR-IN      PIC X(7).                                    
001600*                                 RAPPORT NUMMER                          
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-IDARTNR-IN       PIC X(8).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-IDARTNR-UT       PIC X(8).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-IDRADNR-IN       PIC X(4).                                    
002400*                                 RADNUMMER                               
002500     03 MOD-IDRADNR-UT       PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-IDARTNR-ENTER    PIC 9(8).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MOD-IDRADNR-ENTER    PIC 9(5).                                    
003000*                                 RADNUMMER                               
003100     03 MOD-IDARTNR-NEXT     PIC 9(8).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
003400*                                 RADNUMMER                               
003500     03 MOD-TIRETILL         PIC 9(6).                                    
003600*                                 RETURTILLSTÅNDSDATUM                    
003700     03 MOD-IDANSTNR-RET     PIC Z(4)9.                                   
003800*                                 ANSTÄLLNINGSNUMMER RETURAVDELN.         
003900     03 MOD-LEVANM-RAD       OCCURS 13 TIMES.                             
004000*                                 LEVERANSANM.RAD I PGM W4071400          
004100        05 MOD-IDARTNR       PIC Z(7)9.                                   
004200*                                 ARTIKELNUMMER                           
004300        05 FILLER            PIC X.                                       
004400        05 MOD-IDRADNR       PIC Z(3)9.                                   
004500*                                 RADNUMMER                               
004600        05 FILLER            PIC X(2).                                    
004700        05 MOD-KDANMORS      PIC X(2).                                    
004800*                                 ORSAK TILL LEVERANSANMÄRKNING           
004900        05 FILLER            PIC X.                                       
005000        05 MOD-FLRETILL      PIC X(2).                                    
005100        05 MOD-KVLEVANM-BEKR PIC Z(5)9.                                   
005200*                                 BEKRÄFTAT RETURANTAL                    
005300        05 MOD-KVLEVANM      PIC Z(5)9.                                   
005400*                                 LEVERANSANMÄRKNINGSANTAL                
005500        05 FILLER            PIC X.                                       
005600        05 MOD-TIRETANK      PIC 9(6).                                    
005700*                                 ANKOMSTDATUM                            
005800        05 FILLER            PIC X.                                       
005900        05 MOD-TIINLINL      PIC 9(6).                                    
006000*                                 RAPPORTERINGSDATUM INLAGD (R32)         
006100        05 FILLER            PIC X.                                       
006200        05 MOD-KVRETINL      PIC Z(5)9.                                   
006300*                                 INLAGT ANTAL VID RETUR                  
006400        05 FILLER            PIC X.                                       
006500        05 MOD-KVRETINL-SKR  PIC Z(5)9.                                   
006600*                                 INRPT ANTAL SOM SKROTATS                
006700        05 MOD-KVAVV-KVANT   PIC Z(6)9.                                   
006800*                                 ANTALSAVVIKELSE KVANTITET               
006900        05 MOD-KVAVV-KVAL    PIC Z(6)9.                                   
007000*                                 ANTALSAVVIKELSE KVALITET                
007100        05 FILLER            PIC X.                                       
007200        05 MOD-FLTEXT        PIC X.                                       
007300*                                 FINNS TEXTINFORMATION ?                 
007400     03 MOD-TEMFSINF         PIC X(55).                                   
007500*                                 INFORMATIONSMEDDELANDE                  
007600*** END OF VILMAII-COPY LENGTH= 1182 BYTES                                
