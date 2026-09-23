000100 01  W4O72101.                                                            
000200*                                 MODCOPYTEXT TILL W40721.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDANSV-IN            PIC X(6).                                    
000800     03 TILEVANM-IN          PIC X(6).                                    
000900*                                 DATUM LEVERANSANMÄRKNING                
001000     03 KDANMORS-IN          PIC X(2).                                    
001100*                                 ORSAK TILL LEVERANSANMÄRKNING           
001200     03 KDKREBEH-IN          PIC X(3).                                    
001300*                                 BEHANDLINGSSTATUS                       
001400     03 FLSUM-IN             PIC X.                                       
001500*                                 ALLMÄN FLAGGA                           
001600     03 IDANSV-UT            PIC X(6).                                    
001700     03 TILEVANM-UT          PIC X(6).                                    
001800*                                 DATUM LEVERANSANMÄRKNING                
001900     03 KDANMORS-UT          PIC X(2).                                    
002000*                                 ORSAK TILL LEVERANSANMÄRKNING           
002100     03 KDKREBEH-UT          PIC X(3).                                    
002200*                                 BEHANDLINGSSTATUS                       
002300     03 FLSUM-UT             PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 KVRADER-RT           PIC Z(4)9.                                   
002600*                                 ANTAL RADER                             
002700     03 INPUT                OCCURS 12 TIMES.                             
002800*                                                                         
002900        05 KDCMD-ATTR        PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 KDCMD             PIC X(4).                                    
003200     03 RADER                OCCURS 12 TIMES.                             
003300*                                                                         
003400        05 IDANSV            PIC X(6).                                    
003500        05 TILEVANM          PIC 9(6).                                    
003600*                                 DATUM LEVERANSANMÄRKNING                
003700        05 IDDISTR           PIC Z(3)9.                                   
003800*                                 DISTRIKTNUMMER                          
003900        05 IDKUNDNR          PIC Z(5)9.                                   
004000*                                 KUNDNUMMER                              
004100        05 IDRAPPNR          PIC Z(6)9.                                   
004200*                                 RAPPORT NUMMER                          
004300        05 IDORDNR5          PIC Z(4)9.                                   
004400*                                 ORDERNUMMER                             
004500        05 IDRADNR           PIC Z(3)9.                                   
004600*                                 RADNUMMER                               
004700        05 IDARTNR           PIC Z(7)9.                                   
004800*                                 ARTIKELNUMMER                           
004900        05 KVLEVANM-BEKR     PIC Z(5)9.                                   
005000*                                 BEKRÄFTAT RETURANTAL                    
005100        05 KDANMORS          PIC X(2).                                    
005200*                                 ORSAK TILL LEVERANSANMÄRKNING           
005300        05 FLTEXT            PIC X.                                       
005400*                                 FINNS TEXTINFORMATION ?                 
005500        05 KDKREBEH          PIC X(3).                                    
005600*                                 BEHANDLINGSSTATUS                       
005700     03 TEMFSINF             PIC X(55).                                   
005800*                                 INFORMATIONSMEDDELANDE                  
005900*** END OF VILMAII-COPY LENGTH= 908 BYTES                                 
