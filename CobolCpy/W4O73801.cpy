000100 01  W4O73801.                                                            
000200*                                 MODCOPYTEXT TILL W40738.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDILIST-IN           PIC 9(5).                                    
000800*                                 INLÄGGNINGSLISTEIDENTITET               
000900     03 IDILIST-UT           PIC 9(5).                                    
001000*                                 INLÄGGNINGSLISTEIDENTITET               
001100     03 RADER                OCCURS 13 TIMES.                             
001200*                                                                         
001300        05 IDARTNR           PIC Z(7)9.                                   
001400*                                 ARTIKELNUMMER                           
001500        05 BEART             PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700        05 KVANTAL-KVAR      PIC Z(5).                                    
001800*                                 ANTAL ALLMÄNT                           
001900        05 KDANMORS          PIC X(2).                                    
002000*                                 ORSAK TILL LEVERANSANMÄRKNING           
002100        05 ADLAGOMR          PIC Z9.                                      
002200*                                 LAGEROMRÅDE                             
002300        05 ADGANG            PIC X(2).                                    
002400*                                 GÅNG                                    
002500        05 ADPLATS           PIC Z(4)9.                                   
002600*                                 LAGERPLATSNUMMER                        
002700        05 IDRADNR           PIC Z(3)9.                                   
002800*                                 RADNUMMER                               
002900     03 INPUT.                                                            
003000*                                                                         
003100        05 FLKLAR-ATTR       PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 FLKLAR            PIC X.                                       
003400*                                 ALLMÄN FLAGGA                           
003500        05 IDANSTNR-ATTR     PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 IDANSTNR          PIC X(5).                                    
003800*                                 ANSTÄLLNINGSNUMMER                      
003900        05 FLMAK-ATTR        PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 FLMAK             PIC X.                                       
004200*                                 ALLMÄN FLAGGA                           
004300        05 IDPRT-ATTR        PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 IDPRT             PIC X(3).                                    
004600*                                 LOGISK PRINTERIDENTITET                 
004700        05 INPUTLINE         OCCURS 13 TIMES.                             
004800*                                                                         
004900           07 KDCMDVAL-ATTR  PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100           07 KDCMDVAL       PIC X(3).                                    
005200*                                 GENERELL KOMMANDOKOD                    
005300           07 KVANTAL-ATTR   PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500           07 KVANTAL        PIC X(6).                                    
005600*                                 ANTAL ALLMÄNT                           
005700     03 TEMFSINF             PIC X(55).                                   
005800*                                 INFORMATIONSMEDDELANDE                  
005900*** END OF VILMAII-COPY LENGTH= 985 BYTES                                 
