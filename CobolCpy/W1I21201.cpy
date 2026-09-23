000100 01  MID-W1I21201.                                                        
000200*                                 MID-COPYTEXT FÖR W121200                
000300     03 MID-STRNR-IN         PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-STRNR-UT         PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDSKYLT-IN       PIC X(3).                                    
000800*                                 NATIONALITETSTECKEN                     
000900     03 MID-IDSKYLT-UT       PIC X(3).                                    
001000*                                 NATIONALITETSTECKEN                     
001100     03 MID-IDRADNR-IN       PIC X(4).                                    
001200*                                 RADNUMMER                               
001300     03 MID-IDRADNR-UT       PIC X(4).                                    
001400*                                 RADNUMMER                               
001500     03 MID-IDRADNR-B        PIC 9(4).                                    
001600*                                 RADNUMMER                               
001700     03 MID-INPUT.                                                        
001800*                                                                         
001900        05 MID-IDRADNR-K     PIC X(4).                                    
002000*                                 RADNUMMER                               
002100        05 MID-IDRADNR-F     PIC X(4).                                    
002200*                                 RADNUMMER                               
002300        05 MID-UPPDAT.                                                    
002400*                                                                         
002500           07 MID-IDARTNR    PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700           07 MID-IDLEVNR    PIC X(5).                                    
002800*                                 LEVERANTÖRNUMMER                        
002900           07 MID-BELEVART   PIC X(30).                                   
003000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003100           07 MID-BEART      PIC X(25).                                   
003200*                                 ARTIKELBENÄMNING                        
003300           07 MID-KDBENHOM   PIC X.                                       
003400*                                 HOMONYMKOD                              
003500           07 MID-REANTPSA   PIC X(6).                                    
003600*                                 ANTAL PER SATS                          
003700           07 MID-IDSTRTYP   PIC X.                                       
003800*                                 STRUKTURTYP                             
003900           07 MID-KDSORT     PIC X(2).                                    
004000*                                 SORT-KOD                                
004100        05 MID-IDAO          PIC X(10).                                   
004200*                                 ÄNDRINGSORDERNUMMER                     
004300        05 MID-TIAAVV        PIC X(4).                                    
004400*                                 ÅR - VECKA  (ÅÅVV)                      
004500        05 MID-TESTRNOT      OCCURS 2 TIMES                               
004600                             PIC X(70).                                   
004700*                                 STRUKTURNOTERING                        
004800        05 MID-KLAR          PIC X.                                       
004900*                                 ALLMÄN SVARSFLAGGA                      
005000        05 MID-BORT          PIC X.                                       
005100*                                 ALLMÄN SVARSFLAGGA                      
005200*** END COPY W1I21201C0  LENGTH=279                                       
