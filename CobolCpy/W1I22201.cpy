000100 01  MID-W1I22201.                                                        
000200*                                 MID-COPYTEXT FÖR W122200                
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-BELEVART-IN      PIC X(30).                                   
000800*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
000900     03 MID-BELEVART-UT      PIC X(30).                                   
001000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001100     03 MID-IDLEVNR-IN       PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MID-IDLEVNR-UT       PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 MID-IDSKYLT-IN-DOLT  PIC X(3).                                    
001600*                                 NATIONALITETSTECKEN                     
001700*                                 SPRÅKIDENTIFIKATION                     
001800     03 MID-IDSKYLT-UT-DOLT  PIC X(3).                                    
001900*                                 NATIONALITETSTECKEN                     
002000*                                 SPRÅKIDENTIFIKATION                     
002100     03 MID-1002-SATS-IN-DOLT                                             
002200                             PIC X.                                       
002300*                                 ALLMÄN SVARSFLAGGA                      
002400     03 MID-1002-SATS-UT-DOLT                                             
002500                             PIC X.                                       
002600*                                 ALLMÄN SVARSFLAGGA                      
002700     03 MID-KDPRODSL-IN-DOLT PIC X(2).                                    
002800*                                 PRODUKTSLAG                             
002900     03 MID-KDPRODSL-UT-DOLT PIC X(2).                                    
003000*                                 PRODUKTSLAG                             
003100     03 MID-ANTAL-SEGMENT-ENTER                                           
003200                             PIC 9(3).                                    
003300     03 MID-ANTAL-SEGMENT-NEXT                                            
003400                             PIC 9(3).                                    
003500     03 MID-INPUT.                                                        
003600*                                                                         
003700        05 MID-BEART-UTG-ART PIC X(25).                                   
003800*                                 ARTIKELBENÄMNING                        
003900        05 MID-AANGRA        PIC X.                                       
004000*                                 ALLMÄN SVARSFLAGGA                      
004100        05 MID-IDAO          PIC X(10).                                   
004200*                                 ÄNDRINGSORDERNUMMER                     
004300        05 MID-BORTTAG       PIC X.                                       
004400*                                 ALLMÄN SVARSFLAGGA                      
004500        05 MID-TIAAVV        PIC X(4).                                    
004600*                                 ÅR - VECKA  (ÅÅVV)                      
004700        05 MID-RADER         OCCURS 2 TIMES.                              
004800*                                  TILLKOMMANDE ARTIKLAR                  
004900           07 MID-IDLEVNR    PIC X(5).                                    
005000*                                 LEVERANTÖRNUMMER                        
005100           07 MID-BELEVART   PIC X(30).                                   
005200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
005300           07 MID-IDARTNR    PIC X(9).                                    
005400*                                 ARTIKELNUMMER                           
005500           07 MID-REANTPSA   PIC X(6).                                    
005600*                                 ANTAL PER SATS                          
005700           07 MID-KDSORT     PIC X(2).                                    
005800*                                 SORT-KOD                                
005900           07 MID-BEART      PIC X(25).                                   
006000*                                 ARTIKELBENÄMNING                        
006100           07 MID-KDBENHOM   PIC X.                                       
006200*                                 HOMONYMKOD                              
006300           07 MID-IDSTRTYP   PIC X.                                       
006400*                                 STRUKTURTYP                             
006500           07 MID-TESTRNOT   OCCURS 2 TIMES                               
006600                             PIC X(60).                                   
006700        05 MID-KLAR          PIC X.                                       
006800*                                 ALLMÄN SVARSFLAGGA                      
006900*** END OF VILMAII-COPY LENGTH= 546 BYTES                                 
