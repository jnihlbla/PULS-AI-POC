000100 01  MOD-W4O22801.                                                        
000200*                                 MOD-COPYTEXT FÖR W4022800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC Z(3)9.                                   
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDKUNDNR-UT      PIC Z(5)9.                                   
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDORDNR7-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDORDNR7-UT      PIC Z(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-FLORDLEV-IN      PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-FLORDLEV-UT      PIC X.                                       
002200*                                 FLAGGA LEVERANSORDERNUMMER              
002300     03 MOD-RAD              OCCURS 15 TIMES                              
002400                             INDEXED MOD-IX.                              
002500*                                                                         
002600        05 MOD-KDBEHX-ATTR   PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-KDBEHX-EDIT   PIC X.                                       
002900*                                 BEHANDLINGSKOD-X                        
003000        05 MOD-IDORDNR7-INFO PIC Z(6)9.                                   
003100*                                 ORDERNUMMER                             
003200        05 MOD-IDARTNR       PIC Z(7)9.                                   
003300*                                 ARTIKELNUMMER                           
003400        05 MOD-TIREGDAT      PIC 9(6).                                    
003500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003600        05 MOD-TIHHMM        PIC Z9.9(2).                                 
003700*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003800        05 MOD-KVBEART-URSP  PIC Z(5)9.                                   
003900*                                 BESTÄLLT ANTAL STYCKEN                  
004000        05 MOD-KVBEART-Q     PIC Z(5)9.                                   
004100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004200        05 MOD-KDORDBEK      PIC 9(2).                                    
004300*                                 ORDERBEKRÄFTELSEKOD                     
004400        05 MOD-IDORDNR-LEV   PIC Z(6)9.                                   
004500*                                 LEVERANSORDERNUMMER                     
004600        05 MOD-TIKLAR        PIC 9(6).                                    
004700*                                 KLARDATUM          (ÅÅMMDD)             
004800        05 MOD-TIKLATID      PIC Z9.9(2).                                 
004900*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005000        05 MOD-KDBEHX-INFO   PIC X.                                       
005100*                                 BEHANDLINGSKOD-X                        
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 1055 BYTES                                
