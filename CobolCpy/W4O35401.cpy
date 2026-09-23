000100 01  MOD-W4O35401.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 4354              
000300*                                 FRÅGA PÅ PRODUKTIONSKANAL               
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPRC-IN         PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDPRC-UT.                                                     
001100*                                 PRODUKTIONSKANAL                        
001200        05 MOD-IDPRCBAS      PIC X(3).                                    
001300*                                 PRC-BAS                                 
001400        05 MOD-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600     03 MOD-IDUSER-IN        PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDUSER-UT        PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000     03 MOD-IDORDNSB-ENTER   PIC 9(4).                                    
002100*                                 SATSORDERNUMMER-BAS                     
002200     03 MOD-IDORDNSS-ENTER   PIC X.                                       
002300*                                 SATSORDERNUMMER-SUFFIX                  
002400     03 MOD-IDORDNSB-NEXT    PIC 9(4).                                    
002500*                                 SATSORDERNUMMER-BAS                     
002600     03 MOD-IDORDNSS-NEXT    PIC X.                                       
002700*                                 SATSORDERNUMMER-SUFFIX                  
002800     03 MOD-SUACKPTI-ENTER   PIC 9(5)V9(2).                               
002900*                                 ACK PRODUKTIONSTID SATS TIM MIN         
003000     03 MOD-SUACKPTI-NEXT    PIC 9(5)V9(2).                               
003100*                                 ACK PRODUKTIONSTID SATS TIM MIN         
003200     03 MOD-IDORDNSB-BYGGBAR PIC 9(4).                                    
003300*                                 SATSORDERNUMMER-BAS                     
003400     03 MOD-IDORDNSS-BYGGBAR PIC X.                                       
003500*                                 SATSORDERNUMMER-SUFFIX                  
003600     03 MOD-TABELLRAD        OCCURS 13 TIMES.                             
003700*                                 GRUPP MED TABELL RADER                  
003800        05 MOD-KDCMD-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-KDCMD         PIC X.                                       
004100*                                 RAD-UPPDATERINGSKOMMANDO                
004200        05 MOD-IDARTNR       PIC Z(8)9.                                   
004300*                                 ARTIKELNUMMER                           
004400        05 MOD-IDORDNSB      PIC 9(4).                                    
004500*                                 SATSORDERNUMMER-BAS                     
004600        05 MOD-IDORDNSS      PIC X.                                       
004700*                                 SATSORDERNUMMER-SUFFIX                  
004800        05 MOD-SUSATPTI      PIC Z(2)9.9(2).                              
004900*                                 TOT PRODUKTIONSTID SATS TIM MIN         
005000        05 MOD-SUACKPTI      PIC Z(4)9.9(2).                              
005100*                                 ACK PRODUKTIONSTID SATS TIM MIN         
005200        05 MOD-TIREGDAT      PIC 9(6).                                    
005300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005400        05 MOD-KVBEART       PIC Z(5)9.                                   
005500*                                 BESTÄLLT ANTAL STYCKEN                  
005600        05 MOD-KVRORAD       PIC Z(4)9.                                   
005700*                                 ANTAL RESTORDER-RADER                   
005800        05 MOD-KVROS         PIC Z(6)9.                                   
005900*                                 RESTORDERSALDO                          
006000        05 MOD-BEFT          PIC Z(2)9.                                   
006100*                                 FÖRPACKNINGSTYP                         
006200        05 MOD-IDANSK        PIC Z(2)9.                                   
006300*                                 ANSKAFFARNUMMER                         
006400     03 MOD-TEMFSINF         PIC X(55).                                   
006500*                                 INFORMATIONSMEDDELANDE                  
006600*** END OF VILMAII-COPY LENGTH= 937 BYTES                                 
