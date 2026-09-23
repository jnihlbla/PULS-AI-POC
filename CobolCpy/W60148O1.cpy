000100 01  RESP-W60148O1.                                                       
000200*                                 COPYTEXT FOR RESP W60148O1              
000300*                                                                         
000400     03 RESP-IDRADNR-START   PIC 9(4).                                    
000500*                                 RADNUMMER                               
000600     03 RESP-IDRADNR-NEXT    PIC 9(4).                                    
000700*                                 RADNUMMER                               
000800     03 RESP-IDLOPNRM        PIC X(8).                                    
000900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001000*                                 (0VVDLLLLK)                             
001100     03 RESP-RAD-OUT.                                                     
001200*                                 LINES                                   
001300        05 RESP-IDARTNR      PIC Z(7)9.                                   
001400*                                 ARTIKELNUMMER                           
001500        05 RESP-KVAVIS       PIC Z(5)9.                                   
001600*                                 AVISERAT ANTAL                          
001700        05 RESP-BEART        PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900        05 RESP-ADLAGOMR     PIC Z9.                                      
002000*                                 LAGEROMRÅDE                             
002100        05 RESP-ADGANG       PIC Z9.                                      
002200*                                 GÅNG                                    
002300        05 RESP-ADPLATS      PIC Z(4)9.                                   
002400*                                 LAGERPLATSNUMMER                        
002500        05 RESP-KVRAPP       PIC Z(5)9.                                   
002600*                                 DELRAPPORTERAT ANTAL                    
002700        05 RESP-KVRADER      PIC 9(5).                                    
002800*                                 ANTAL RADER                             
002900        05 RESP-RAD          OCCURS 50 TIMES.                             
003000*                                 LINES                                   
003100           07 RESP-KDCMDVAL-LINE-ATTR                                     
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400           07 RESP-KDCMDVAL-LINE                                          
003500                             PIC X(3).                                    
003600*                                 GENERELL KOMMANDOKOD                    
003700           07 RESP-IDRADNR-LINE                                           
003800                             PIC Z(3)9.                                   
003900*                                 RADNUMMER                               
004000           07 RESP-IDLEVNR-KOLLI-LINE                                     
004100                             PIC X(5).                                    
004200*                                 LEVERANTÖRNUMMER KOLLI                  
004300           07 RESP-IDOKOLLI-LINE                                          
004400                             PIC Z(9).                                    
004500*                                 ODETTE KOLLINUMMER                      
004600           07 RESP-KVINLART-LINE                                          
004700                             PIC Z(5)9.                                   
004800*                                 ANTAL I PARTIRAD                        
004900           07 RESP-ADINLOMR-LINE                                          
005000                             PIC X(4).                                    
005100*                                 INLEVERANSOMRÅDE                        
005200           07 RESP-FLPRIO-LINE                                            
005300                             PIC X.                                       
005400*                                 PRIORITERAD                             
005500           07 RESP-KDINLSTA-LINE                                          
005600                             PIC X(3).                                    
005700*                                 SYSTEMSTATUS INLEVERANS                 
005800*** END OF VILMAII-COPY LENGTH= 1925 BYTES                                
