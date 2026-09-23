000100 01  W4O73601.                                                            
000200*                                 MODCOPYTEXT TILL W40736.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDANSV-IN            PIC X(6).                                    
000800     03 IDDISTR-IN           PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR-IN          PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 KDLEVANM-FOM-IN      PIC X.                                       
001300*                                 STATUS LEVERANSANMÄRKNING               
001400     03 KDLEVANM-TOM-IN      PIC X.                                       
001500*                                 STATUS LEVERANSANMÄRKNING               
001600     03 FLSUM-IN             PIC X.                                       
001700*                                 ALLMÄN FLAGGA                           
001800     03 IDDC-IN              PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 IDANSV-UT            PIC X(6).                                    
002100     03 IDDISTR-UT           PIC X(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 IDKUNDNR-UT          PIC X(6).                                    
002400*                                 KUNDNUMMER                              
002500     03 KDLEVANM-FOM-UT      PIC X.                                       
002600*                                 STATUS LEVERANSANMÄRKNING               
002700     03 KDLEVANM-TOM-UT      PIC X.                                       
002800*                                 STATUS LEVERANSANMÄRKNING               
002900     03 FLSUM-UT             PIC X.                                       
003000*                                 ALLMÄN FLAGGA                           
003100     03 IDDC-UT              PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 KVANT-RT             PIC Z(4)9.                                   
003400     03 KVRADER-RT           PIC Z(4)9.                                   
003500*                                 ANTAL RADER                             
003600     03 INPUT.                                                            
003700*                                                                         
003800        05 IDPRT-ATTR        PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 IDPRT             PIC X(3).                                    
004100*                                 LOGISK PRINTERIDENTITET                 
004200     03 RADER                OCCURS 11 TIMES.                             
004300*                                                                         
004400        05 KDCMD-ATTR        PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 KDCMD             PIC X(4).                                    
004700        05 IDANSV-ATTR       PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 IDANSV            PIC X(6).                                    
005000        05 TIRETILL-ATTR     PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 TIRETILL          PIC 9(6).                                    
005300*                                 RETURTILLSTÅNDSDATUM                    
005400        05 TIRETANK-ATTR     PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 TIRETANK          PIC 9(6).                                    
005700*                                 ANKOMSTDATUM                            
005800        05 IDDISTR-ATTR      PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 IDDISTR           PIC Z(3)9.                                   
006100*                                 DISTRIKTNUMMER                          
006200        05 IDKUNDNR-ATTR     PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 IDKUNDNR          PIC Z(5)9.                                   
006500*                                 KUNDNUMMER                              
006600        05 IDRAPPNR-ATTR     PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 IDRAPPNR          PIC X(7).                                    
006900*                                 RAPPORT NUMMER                          
007000        05 KDLEVANM-ATTR     PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 KDLEVANM          PIC X.                                       
007300*                                 STATUS LEVERANSANMÄRKNING               
007400        05 KVRADER-ATTR      PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 KVRADER           PIC Z(4)9.                                   
007700*                                 ANTAL RADER                             
007800        05 KVRADER-OBEH-ATTR PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 KVRADER-OBEH      PIC Z(4)9.                                   
008100*                                 ANTAL RADER                             
008200        05 KVKOLLI-ATTR      PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 KVKOLLI           PIC Z(3)9.                                   
008500*                                 ANTAL KOLLI                             
008600     03 TEMFSINF             PIC X(55).                                   
008700*                                 INFORMATIONSMEDDELANDE                  
008800*** END OF VILMAII-COPY LENGTH= 992 BYTES                                 
