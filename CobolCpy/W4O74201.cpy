000100 01  W4O74201.                                                            
000200*                                 MODCOPYTEXT TILL W40742.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDKOLLI-IN           PIC X(5).                                    
000800*                                 KOLLINUMMER                             
000900     03 FLVISA-IN            PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 IDKOLLI-UT           PIC X(5).                                    
001200*                                 KOLLINUMMER                             
001300     03 FLVISA-UT            PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500     03 KOLLIUPG.                                                         
001600*                                                                         
001700        05 IDKOLLI           PIC Z(4)9.                                   
001800*                                 KOLLINUMMER                             
001900        05 IDKOLLI-FOM       PIC Z(4)9.                                   
002000*                                 KOLLINUMMER                             
002100        05 IDKOLLI-TOM       PIC Z(4)9.                                   
002200*                                 KOLLINUMMER                             
002300     03 FLFARLIG-KOLLI       PIC X.                                       
002400*                                 FARLIGT GODS-FLAGGA                     
002500*                                                                         
002600     03 RADER                OCCURS 10 TIMES.                             
002700*                                                                         
002800        05 IDDISTR           PIC Z(3)9.                                   
002900*                                 DISTRIKTNUMMER                          
003000        05 IDKUNDNR          PIC Z(5)9.                                   
003100*                                 KUNDNUMMER                              
003200        05 IDRAPPNR          PIC X(7).                                    
003300*                                 RAPPORT NUMMER                          
003400        05 KVKOLLI-AAF       PIC Z(3)9.                                   
003500*                                 ANTAL KOLLI                             
003600        05 FLFARLIG          PIC X.                                       
003700*                                 FARLIGT GODS-FLAGGA                     
003800*                                                                         
003900     03 INPUT.                                                            
004000*                                                                         
004100        05 FLNYKLI-ATTR      PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 FLNYKLI           PIC X.                                       
004400*                                 ALLMÄN FLAGGA                           
004500        05 KVKOLLI-ATTR      PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 KVKOLLI           PIC X(4).                                    
004800*                                 ANTAL KOLLI                             
004900        05 INPUT-RAD         OCCURS 10 TIMES.                             
005000*                                                                         
005100           07 KDCMD-ATTR     PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300           07 KDCMD          PIC X.                                       
005400*                                 RAD-UPPDATERINGSKOMMANDO                
005500        05 IDDISTR-IN-ATTR   PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 IDDISTR-IN        PIC X(4).                                    
005800*                                 DISTRIKTNUMMER                          
005900        05 IDKUNDNR-IN-ATTR  PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 IDKUNDNR-IN       PIC X(6).                                    
006200*                                 KUNDNUMMER                              
006300        05 IDRAPPNR-IN-ATTR  PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 IDRAPPNR-IN       PIC X(7).                                    
006600*                                 RAPPORT NUMMER                          
006700     03 TEMFSINF             PIC X(55).                                   
006800*                                 INFORMATIONSMEDDELANDE                  
006900*** END OF VILMAII-COPY LENGTH= 409 BYTES                                 
