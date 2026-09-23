000100 01  MOD-W4O70901.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4070900           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPARTNR-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDPARTNR-UT      PIC X(9).                                    
001000*                                 PARTNERNUMMER                           
001100     03 MOD-IDFTG-IN         PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDFTG-UT         PIC X(2).                                    
001400*                                 FÖRETAGSID EKONOM REDOVISNING           
001500     03 MOD-IDDISTR-IN       PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDDISTR-UT       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MOD-KDRAPPSTA-IN     PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-KDRAPPSTA-UT     PIC X(2).                                    
002600*                                 RAPPORTSTATUS HANDLING FEE              
002700     03 MOD-RADER            OCCURS 11 TIMES.                             
002800*                                 RADINFORMATION                          
002900        05 MOD-KDANMORS      PIC X(2).                                    
003000*                                 ORSAK TILL LEVERANSANMÄRKNING           
003100        05 MOD-IDDISTR       PIC X(4).                                    
003200*                                 DISTRIKTNUMMER                          
003300        05 MOD-IDKUNDNR      PIC X(6).                                    
003400*                                 KUNDNUMMER                              
003500        05 MOD-SUARTBTO      PIC Z(6)9.9(2).                              
003600*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
003700        05 MOD-KDVALISO      PIC X(3).                                    
003800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003900        05 MOD-IDRAPP        PIC X(10).                                   
004000*                                 RAPPORT ID                              
004100        05 MOD-KDRAPPSTA     PIC X(2).                                    
004200*                                 RAPPORTSTATUS HANDLING FEE              
004300        05 MOD-IDFINDOC      PIC Z(8)9.                                   
004400*                                 FINANSIELLT DOKUMENT ID                 
004500        05 MOD-DAFAKT        PIC 9(6).                                    
004600*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
004700        05 MOD-IDUSER-2      PIC X(8).                                    
004800*                                 ANVÄNDARENS SÄKERHETS ID                
004900        05 MOD-DAUPPDAT      PIC 9(6).                                    
005000*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
005100     03 MOD-SUARTBTO-RET     PIC Z(6)9.9(2).                              
005200*                                 SUMMA FÖRSÄLJNINGSVÄRDE FÖR RET         
005300*                                 URER                                    
005400     03 MOD-SUARTBTO-72      PIC Z(6)9.9(2).                              
005500*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
005600     03 MOD-KDVALISO-72      PIC X(3).                                    
005700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005800     03 MOD-SUARTBTO-98      PIC Z(6)9.9(2).                              
005900*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
006000     03 MOD-KDVALISO-98      PIC X(3).                                    
006100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006200     03 MOD-SUARTBTO-RR      PIC Z(6)9.9(2).                              
006300*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
006400     03 MOD-KDVALISO-RR      PIC X(3).                                    
006500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006600     03 MOD-TEMFSINF         PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 907 BYTES                                 
