000100 01  RESP-WL0120O1.                                                       
000200*                                 RESPONS FROM PGM WL0120                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDANSTNR-KEY    PIC Z(4)9.                                   
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 RESP-FLPREPRINT-KEY  PIC X.                                       
000800*                                 FLAGGA PRE PRINT                        
000900     03 RESP-IDQUEUENR-KEY   PIC 9(3).                                    
001000*                                 PRE PRINT QUEUE NUMBER                  
001100     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
001200*                                 DISTRIKTNUMMER                          
001300     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
001400*                                 KUNDNUMMER                              
001500     03 RESP-IDORDNR-KEY     PIC Z(4)9.                                   
001600*                                 ORDERNUMMER                             
001700     03 RESP-IDKOLLI-KEY     PIC Z(4)9.                                   
001800*                                 KOLLINUMMER                             
001900     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
002000*                                 PRODUKTIONSNUMMER                       
002100     03 RESP-KVRADER         PIC Z(4)9.                                   
002200*                                 ANTAL RADER                             
002300     03 RESP-RAD             OCCURS 500 TIMES.                            
002400*                                                                         
002500        05 RESP-IDDISTR      PIC Z(3)9.                                   
002600*                                 DISTRIKTNUMMER                          
002700        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
002800*                                 KUNDNUMMER                              
002900        05 RESP-IDORDNR      PIC Z(4)9.                                   
003000*                                 ORDERNUMMER                             
003100        05 RESP-IDPRODNR     PIC Z(6)9.                                   
003200*                                 PRODUKTIONSNUMMER                       
003300        05 RESP-IDPLKLST     PIC Z(2)9.                                   
003400*                                 PLOCKLISTNUMMER                         
003500        05 RESP-IDRADNR-ORD-FROM                                          
003600                             PIC Z(3)9.                                   
003700*                                 RADNUMMER PÅ VOLVOORDER FROM            
003800        05 RESP-KDASTERISK   PIC X(2).                                    
003900        05 RESP-IDRADNR-ORD-TOM                                           
004000                             PIC Z(3)9.                                   
004100*                                 RADNUMMER PÅ VOLVOORDER TOM             
004200        05 RESP-IDPRCPLK     PIC X(4).                                    
004300*                                 ID FÖR EN PLOCKRUNDA                    
004400        05 RESP-IDLOTNR-PLK  PIC 9(3).                                    
004500*                                 VAGN-NUMMER FÖR PLOCKRUNDA              
004600        05 RESP-IDANSTNR-OLD PIC Z(4)9.                                   
004700*                                 ANSTÄLLNINGSNUMMER                      
004800        05 RESP-IDQUEUENR    PIC 9(3).                                    
004900*                                 PRE PRINT QUEUE NUMBER                  
005000        05 RESP-IDANSTNR-NEW PIC Z(4)9.                                   
005100*                                 ANSTÄLLNINGSNUMMER                      
005200        05 RESP-IDMSG-ERROR-LINE                                          
005300                             PIC X(3).                                    
005400*                                 FELMEDDELANDE ID                        
005500*** END OF VILMAII-COPY LENGTH= 29043 BYTES                               
