000100 01  MOD-W3O16501.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O16501                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDARTNR-UT       PIC X(8).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-OUTPUTDATA.                                                   
001300        05 MOD-IDARTNR-CORE  PIC Z(7)9.                                   
001400*                                 ARTIKELNUMMER                           
001500        05 MOD-BEART-CORE    PIC X(25).                                   
001600*                                 ENGELSK ARTIKELBENÄMNING                
001700        05 MOD-IDARTNR-REN   PIC Z(7)9.                                   
001800*                                 ARTIKELNUMMER                           
001900        05 MOD-BEART-REN     PIC X(25).                                   
002000*                                 ENGELSK ARTIKELBENÄMNING                
002100        05 MOD-IDANSK        PIC Z(2)9.                                   
002200*                                 ANSKAFFARNUMMER                         
002300        05 MOD-RELARM-FAC    PIC 9.9(2).                                  
002400*                                 SHOWS ALARM LIMIT FOR EXCH-PART         
002500*                                 S                                       
002600        05 MOD-KVPOINT       PIC Z(5)9.                                   
002700*                                 POINT VALUE                             
002800        05 MOD-RERETUR-ATTR  PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-RERETUR       PIC Z9.                                      
003100*                                 VISAR PROCENT FÖR RETURER               
003200        05 MOD-REREUSE-ATTR  PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-REREUSE       PIC Z9.                                      
003500*                                 VISAR PROCENT FÖR ÅTERANV.              
003600        05 MOD-RELARM-PER-ATTR                                            
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-RELARM-PER    PIC Z9.                                      
004000*                                 PERCENTAGE THAT INDICATES WHEN          
004100*                                 A DEVIATION BETWEEN                     
004200*                                 OUTBOUND QTY AND RETURNED QTY I         
004300*                                 S TO BE ALARMED                         
004400        05 MOD-KVLS-REN      PIC -(6)9.                                   
004500*                                 LAGERSALDO                              
004600        05 MOD-KVLS-CORE     PIC -(6)9.                                   
004700*                                 LAGERSALDO                              
004800        05 MOD-SULEVANT-RAAR PIC -(3)B-(3)B-(2)9.                         
004900*                                 ANTAL LEV ART RULLANDE ÅR               
005000*                                 (AF2)                                   
005100        05 MOD-FLLARM-ACT-ATTR                                            
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-FLLARM-ACT    PIC X.                                       
005500*                                 OBJEKT LARMRAPPORT UTFÄRDAD             
005600     03 MOD-TEMFSINF         PIC X(55).                                   
005700*                                 INFORMATIONSMEDDELANDE                  
005800*** END OF VILMAII-COPY LENGTH= 228 BYTES                                 
